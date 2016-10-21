# tidy the MCMC output
source("config.R")
library(dplyr)
library(ggplot2)
sports <- c("mlb", "nba", "nfl", "nhl")

get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_9_23_teamHFA.RData")))
  out <- data.frame(
    alpha = z$alpha[,,1]
  ) %>%
    mutate(sport = sport)
  beta <- z$beta
  rm(z)
  return(list(beta = beta, out = out))
}

# Extract the values we will need
dat <- lapply(sports[1:4], get_sport)
betas <- lapply(dat,function(x){return(x[["beta"]])})  
names(betas) <-  sports


# to save memory!
rm(dat)

n_sports <- sapply(betas, length) / 12000


load("data/bigfour.final.rda")
bigfour <- bigfour.final
teamnames <- bigfour %>% group_by(sport, home_team) %>% summarise(n.games = n())


makeBetas <- function(sport){
  teamnames <- teamnames[teamnames$sport==sport,]
  teamnames <- teamnames[order(teamnames$home_team),]
  out <- apply(betas[[sport]],c(1,2,3),mean)
  #out1 <- apply(alphaInds[[sport]],c(1),mean)
  dims <- dim(betas[[sport]])
  names(dims) <- c("nseasons","nweeks","nteams","iteration","chain")
  getbetas <- function(i){
    out <- data.frame(betas = as.vector(betas[[sport]][,(dims["nweeks"]-4):dims["nweeks"],i,,1]), 
                      team = teamnames$home_team[i], 
                      season = rep(1:dims["nseasons"], 4000*5))
  }
  datbetas2 <- lapply(c(1:dims["nteams"]),getbetas) %>% bind_rows()
  datbetas2 <- datbetas2 %>%
    mutate(sport = sport)
  return(datbetas2)
}

dat.betas <- lapply(sports[1:4], makeBetas) 
betas.all <- lapply(dat.betas,function(x){return(x[])})  
names(betas.all) <- sports
betas.lastfour.all <- rbind(betas.all$nba, betas.all$mlb, betas.all$nhl, betas.all$nfl)

betas.lastfour.all <- betas.lastfour.all %>%
  mutate(season = season + 2005)


#Extract HFA from each
load("data/params.rda")
hfa <- select(params, sport, alpha)

#Expand grid

df.sim <- expand.grid(first.seed = 1:8, second.seed = 1:8, season = 2006:2016, sport = sports)
df.sim <- df.sim %>% 
  filter(second.seed > first.seed, season < 2016|sport!="nfl") %>%
  arrange(sport, first.seed)



#Rank teams
team.rank <- betas.lastfour.all %>%
  group_by(sport, season, team) %>%
  summarise(mean.beta = mean(betas)) %>%
  arrange(desc(mean.beta)) %>%
  mutate(rank = 1:n()) 

## Data check
filter(team.rank, team == "New England Patriots")
head(team.rank)


df.sim1 <- df.sim %>%
  left_join(team.rank, by = c("season" = "season", "sport" = "sport", "first.seed" = "rank")) %>%
  rename(first.team = team) %>%
  select(-mean.beta) %>%
  left_join(team.rank, by = c("season" = "season", "sport" = "sport", "second.seed" = "rank")) %>%
  rename(second.team = team) %>%
  select(-mean.beta) %>% 
  filter(first.seed == 1)


## Merge team ranked data with posterior draws

betas.lastfour.all.rank <- betas.lastfour.all %>%
  left_join(team.rank) %>%
  arrange(sport, team, season)




for (seasons in 2006:2016){
  p <- ggplot(filter(betas.lastfour.all.rank, season ==seasons), 
              aes(x = betas, group = team)) +
    geom_density() + facet_wrap(~sport, nrow = 2) + 
    theme(legend.position="none") + 
    theme_bw(12) + ggtitle("Team strength estimates") + 
    scale_x_continuous("Beta-hat")
  file <- paste0("~/Dropbox/Competitiveness/figures/beta.hats",seasons, ".pdf")
  ggsave(plot = p, filename = file)
}

betas.lastfour.all.rank %>% 
  group_by(sport, season, team) %>%
  summarise(ave.strength = mean(betas), sd.strength = sd(betas)) %>%
  ungroup() %>%
  arrange(sd.strength)


teams.ex <- betas.lastfour.all.rank %>%
  filter(sport == "nba", team == "San Antonio Spurs" |team == "Atlanta Hawks", season ==2016)

ggplot(teams.ex, aes(betas, colour = team)) + 
  geom_density() + ggtitle("Spurs vs. Hawks, 2016 NBA")
  
# define limits of a common grid
lower <- -3
upper <- 3
library(sfsmisc)

overlap.fn <- function(team1.betas, team2.betas){
da <- density(team1.betas, from=lower, to=upper)  #Kernal density
db <- density(team2.betas, from=lower, to=upper)  #Kernal density
d <- data.frame(x=da$x, a=da$y, b=db$y)
d$w <- pmin(d$a, d$b) #intersection density  
total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
intersection <- integrate.xy(d$x, d$w)
overlap <- 2 * intersection / total  # compute overlap coefficient
return(overlap)
}

overlap.fn(filter(teams.ex, team == "San Antonio Spurs")$betas, 
           filter(teams.ex, team == "Atlanta Hawks")$betas)

