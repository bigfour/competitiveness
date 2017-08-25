# tidy the MCMC output
source("config.R") 

sports <- c("nba", "nhl", "mlb", "nfl")

get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_paper_teamHFA.R2.RData")))
  out <- data.frame(
    sigma_g = 1/z$tauGame[,,1],
    sigma_w = 1/z$tauWeek[,,1],
    sigma_s = 1/z$tauSeason[,,1],
    gamma_w = z$gammaWeek[,,1],
    gamma_s = z$gammaSeason[,,1]
    #alpha = z$alpha[,,1]
  ) %>%
    mutate(sport = sport)
  alphaInd <- z$alphaInd
  theta <- z$theta
  rm(z)
  return(list(out = out, theta = theta, alphaInd = alphaInd))
}

# Extract the values we will need
dat <- lapply(sports[1:4], get_sport)

params <- lapply(dat, function(x) { return(x[["out"]]) } ) %>% 
  bind_rows()
# save the params
save(params, file = file.path(root, "data", "params.R2.rda"), compress = "xz")



thetas <- lapply(dat,function(x){return(x[["theta"]])})  
names(thetas) <-  sports

alphaInds <- lapply(dat, function(x) {return(x[["alphaInd"]])})
names(alphaInds) <- sports

# to save memory!
rm(dat)

n_sports <- sapply(thetas, length) / 1200
## want the above to be 8250, 9240, etc


# tidy up the thetas mcarray
library(broom)
tidy.mcarray <- function(x, ...) {
  avgs <- apply(x, c(1,2,3), mean)
  dims <- dim(avgs)
  names(dims) <- c("nseasons", "nweeks", "nteams")
  out <- data.frame(
    theta = as.vector(avgs),
    season = rep(1:dims["nseasons"]),
    week = rep(1:dims["nweeks"], each = dims["nseasons"]), 
    team_id = rep(1:dims["nteams"], each = dims["nseasons"] * dims["nweeks"])
  ) %>%
    mutate(cumweek = (season - 1) * dims["nweeks"] + week)
  return(out)
}

tidy_thetas <- lapply(thetas, tidy) %>%
  bind_rows() %>%
  mutate(sport = rep(sports, times = n_sports), 
         max.week = ifelse(sport == "nfl", 17, ifelse(sport == "nba", 24, 28))) %>%
  group_by(season) %>%
  mutate(time_val = 2004 + season + week / max.week) %>%
  ungroup()


# crosscheck
# the means should all be relatively close to 0, right??
tidy_thetas %>%
  group_by(sport, season, week) %>%
  summarize(N = n(), mean_theta = mean(theta), sd_theta = sd(theta)) %>%
  print(n = Inf)

#  devtools::install_github("beanumber/teamcolors")

# match up the colors
library(teamcolors)
colors <- teamcolors %>%
  filter(sport %in% sports) %>%
  #Our data has the St. Louis Rams.  The colors has LA Rams.  
  mutate(name = ifelse(name == "Los Angeles Angels of Anaheim", "Los Angeles Angels", name),
         name = ifelse(name == "St Louis Blues", "St. Louis Blues", name)) %>%
  arrange(sport, name) %>%
  group_by(sport) %>%
  mutate(team_id = 1:n())

# check to make sure that names all match up
load(file.path("data", "bigfour_public.rda"))
teams <- bigfour_public %>%
  group_by(sport, home_team) %>%
  summarize(N = n()) %>%
  arrange(sport, home_team) %>%
  ungroup() %>%
  select(-sport) %>%
  bind_cols(colors)
teams %>%
  filter(home_team != name)



tidy_thetas <- tidy_thetas %>%
  inner_join(colors, by = c("sport" = "sport", "team_id" = "team_id"))

# save the results so we don't have to do this everytime. 
save(tidy_thetas, file = file.path(root, "data", "tidy_thetas.R2.rda"), compress = "xz")


### Alphas
## Overall sport estimate
sport.est <- params %>% 
  group_by(sport) %>% 
  summarise(alpha.sport = mean(alpha))

makeAlphas <- function(sport){
  teamnames <- sort(t(unique(bigfour_public[bigfour_public$sport==sport,"home_team"])))
  if (sport == "nba"){teamnames[31] <- "Seattle Supersonics"}
  if (sport == "nhl"){teamnames[31] <- "Atlanta Thrashers"}
  out.med <- apply(alphaInds[[sport]],c(1),median)
  out.lower <- apply(alphaInds[[sport]],c(1), quantile, probs = 0.025)
  out.upper <- apply(alphaInds[[sport]],c(1), quantile, probs = 0.975)
  dat.alphas <- data.frame(alpha.team = out.med, alpha.lower = out.lower, 
                           alpha.upper = out.upper, team = teamnames, sport = sport)
  return(dat.alphas)
}

dat.alphas <- lapply(sports[1:4], makeAlphas) 
alphas.all <- lapply(dat.alphas,function(x){return(x[])}) %>%  
  bind_rows() %>% 
  mutate(alpha.team.overall = alpha.team, 
         alpha.team.lower = alpha.lower, 
         alpha.team.upper = alpha.upper)

colors.new <- data.frame(name = c("Seattle Supersonics", "Atlanta Thrashers"), 
                         primary = c("#025736", "#0A2351"),
                         secondary = c("#FDBB2F", "#F58220"), 
                         tertiary = c("<NA>", "<NA>"), 
                         quaternary = c("<NA>", "<NA>"),
                         sport = c("nba", "nhl"),
                         team_id = c(nrow(colors)+1, nrow(colors) + 2))
colors.hfa <- bind_rows(colors, colors.new)


tidy_alphas <- alphas.all %>%
  inner_join(colors.hfa, by = c("team" = "name", "sport" = "sport"))

tidy_alphas %>% group_by(sport) %>% summarise(ave.alpha = mean(alpha.team))

### update with an alpha's output, and run the tidy_mcmc
save(tidy_alphas, file = file.path(root, "data", "tidy_alphas.R2.rda"), compress = "xz")  
