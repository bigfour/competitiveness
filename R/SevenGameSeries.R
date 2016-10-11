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
  mutate(rank = 1:n()) %>%
  filter(rank < 9)


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

##########################
## Simulate the postseason
##########################

n.sim <- 10000
df.sim1$better.winP <- NULL
for (i in 1:nrow(df.sim1)){
temp <- df.sim1[i,]
fave.win.nohfa <- NULL; fave.win.hfa <- NULL
top.team <- betas.lastfour.all %>% 
  filter(season == temp$season, team == temp$first.team) %>%
  select(betas)
lower.team <- betas.lastfour.all %>% 
  filter(season == temp$season, team == temp$second.team) %>%
  select(betas)
hfa.all <- hfa %>%
  filter(sport == temp$sport) %>%
  select(alpha)
n.games <- ifelse(temp$sport == "nfl", 1, ifelse(temp$sport == "mlb", 5, 7))
for (j in 1:n.sim){
    top.draws <- top.team %>% do(sample_n(., n.games)) 
    lower.draws <- lower.team %>% do(sample_n(., n.games)) 
    hfa.draws <- hfa.all %>% do(sample_n(., n.games))
    hfa.switch <- c(rep(1, ceiling(n.games/2)), rep(-1, floor(n.games/2)))
    logit.probs.nohfa <- top.draws - lower.draws
    logit.probs.hfa <- top.draws - lower.draws + hfa.draws*hfa.switch
    ### Note: want to add in sport-specific variability in draws (e.g., residuals)
    probs.nohfa <- exp(logit.probs.nohfa)/(1+exp(logit.probs.nohfa))
    probs.hfa <- exp(logit.probs.hfa)/(1+exp(logit.probs.hfa))
    fave.win.nohfa[j] <- sum(rbinom(n.games, 1, probs.nohfa$betas)) >= ceiling(n.games/2)  
    fave.win.hfa[j] <- sum(rbinom(n.games, 1, probs.hfa$betas)) >= ceiling(n.games/2)  
  }
df.sim1$better.winP.nohfa[i] <- mean(fave.win.nohfa)
df.sim1$better.winP.hfa[i] <- mean(fave.win.hfa)
print(i)
}


write.csv(df.sim1, "data/seven.simulations.final.csv")


##################################################
### df.sim1 stores simulated draws of each series
### probability is probability the better team wins
##################################################
 


## 2005 MLB: transitivity?  Can't do with new data
probs %>% filter(sport=="mlb", season==2005)

teams <- c("New York Yankees", "St Louis Cardinals", "Cleveland Indians")
draws.transitivity <- betas.lastfour.all %>%
  filter(team %in% teams, season==2005)
p <- ggplot(draws.transitivity, aes(x = betas, fill = team))
p + geom_density(alpha = 0.25) + ggtitle("2005 MLB team strengths")
#Text for each year


##########################################
##### How many games to match the NBA's 80% (eighth seed) and 72% (fourth seed)
##########################################

first.eighth <- filter(df.sim1, second.seed == 8, sport =="mlb"|sport=="nhl"|sport =="nba"|sport == "nfl")
n.sim <- 1000
n.games <- 7
first.eighth$better.winP <- NULL
for (i in 1:nrow(first.eighth)){
  temp <- first.eighth[i,]
  fave.win <- NULL
  top.team <- betas.lastfour.all %>% 
    filter(season == temp$season, team == temp$first.team) %>%
    select(betas)
  lower.team <- betas.lastfour.all %>% 
    filter(season == temp$season, team == temp$second.team) %>%
    select(betas)
  hfa.all <- hfa %>%
    filter(sport == temp$sport) %>%
    select(alpha)
  for (j in 1:n.sim){
    top.draws <- top.team %>% do(sample_n(., n.games)) 
    lower.draws <- lower.team %>% do(sample_n(., n.games)) 
    hfa.draws <- hfa.all %>% do(sample_n(., n.games))
    hfa.switch <- c(rep(1, ceiling(n.games/2)), rep(-1, floor(n.games/2)))
    logit.probs <- top.draws - lower.draws + hfa.draws*hfa.switch
    ### Note: want to add in sport-specific variability in draws (e.g., residuals)
    probs <- exp(logit.probs)/(1+exp(logit.probs))
    fave.win[j] <- sum(rbinom(n.games, 1, probs$betas)) >= ceiling(n.games/2)  
  }
  first.eighth$better.winP[i] <- mean(fave.win)
  print(i)
}


first.eighth %>% group_by(sport) %>% summarise(ave.win = mean(better.winP))

#Most similar using No. 1 v No. 8 (80%): NFL (9 games), NHL (39 games), MLB (51 games)
#Most similar using No. 1 v No. 4 (72%): NFL (9 games), NHL (39 games), MLB (51 games)