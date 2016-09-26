# tidy the MCMC output
source("config.R")

library(dplyr)
sports <- c("mlb", "nba", "nfl", "nhl")

get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_8_23_teamHFA.RData")))
  out <- data.frame(
    sigma_w = 1/z$sigmab[,,1],
    sigma_s = 1/z$sigmabSeason[,,1],
    gamma_w = z$gammaWeek[,,1],
    gamma_s = z$gammaSeason[,,1],
    alpha = z$alpha[,,1]
  ) %>%
    mutate(sport = sport)
  alphaInd <- z$alphaInd
  beta <- z$beta
  rm(z)
  return(list(out = out, beta = beta, alphaInd = alphaInd))
}

# Extract the values we will need
dat <- lapply(sports[1:4], get_sport)

params <- lapply(dat, function(x) { return(x[["out"]]) } ) %>% 
  bind_rows()
# save the params
save(params, file = file.path(root, "data", "params.rda"), compress = "xz")

betas <- lapply(dat,function(x){return(x[["beta"]])})  
names(betas) <-  sports

alphaInds <- lapply(dat, function(x) {return(x[["alphaInd"]])})
names(alphaInds) <- sports

# to save memory!
rm(dat)

n_sports <- sapply(betas, length) / 12000

# tidy up the betas mcarray
library(broom)
tidy.mcarray <- function(x, ...) {
  avgs <- apply(x, c(1,2,3), mean)
  dims <- dim(avgs)
  names(dims) <- c("nseasons", "nweeks", "nteams")
  out <- data.frame(
    beta = as.vector(avgs),
    season = rep(1:dims["nseasons"]),
    week = rep(1:dims["nweeks"], each = dims["nseasons"]), 
    team_id = rep(1:dims["nteams"], each = dims["nseasons"] * dims["nweeks"])
  ) %>%
    mutate(cumweek = (season - 1) * dims["nweeks"] + week)
  return(out)
}

tidy_betas <- lapply(betas, tidy) %>%
  bind_rows() %>%
  mutate(sport = rep(sports, times = n_sports)) %>%
  group_by(season) %>%
  mutate(time_val = 2004 + season + week / max(week),
         time_val = ifelse(sport == "nhl", time_val + 1, time_val)) %>%
  ungroup()


# crosscheck
# the means should all be relatively close to 0, right??
tidy_betas %>%
  group_by(sport, season) %>%
  summarize(N = n(), mean_beta = mean(beta), sd_beta = sd(beta)) %>%
  print(n = Inf)

#  devtools::install_github("beanumber/teamcolors")

# match up the colors
library(teamcolors)
colors <- teamcolors %>%
  filter(sport %in% sports) %>%
  #Our data has the St. Louis Rams.  The colors has LA Rams.  
  mutate(name = ifelse(name == "Los Angeles Rams", "St. Louis Rams", name),
         name = ifelse(name == "Los Angeles Angels of Anaheim", "Los Angeles Angels", name),
         name = ifelse(name == "St Louis Blues", "St. Louis Blues", name)) %>%
  arrange(sport, name) %>%
  group_by(sport) %>%
  mutate(team_id = 1:n())

# check to make sure that names all match up
load("data/bigfour.rda")
teams <- bigfour %>%
  group_by(sport, home_team) %>%
  summarize(N = n()) %>%
  arrange(sport, home_team) %>%
  ungroup() %>%
  select(-sport) %>%
  bind_cols(colors)
teams %>%
  filter(home_team != name)

tidy_betas <- tidy_betas %>%
  inner_join(colors, by = c("sport" = "sport", "team_id" = "team_id"))

# save the results so we don't have to do this everytime. 
save(tidy_betas, file = file.path(root, "data", "tidy_betas.rda"), compress = "xz")

### update with an alpha's output, and run the tidy_mcmc
save(tidy_alphas, file = file.path(root, "data", "tidy_alphas.rda"), compress = "xz")  
