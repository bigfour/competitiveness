library(dplyr) 
library(ggplot2)

sports <- c("mlb", "nba", "nfl", "nhl")
source("config.R")
load("data/bigfour.rda")
library(knitr)


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
  beta <- z$beta
  alphaInd <- z$alphaInd
  rm(z)
  return(list(out = out , beta = beta, alphaInd = alphaInd))
}

dat <- lapply(sports[1:4], get_sport) 
betas <- lapply(dat,function(x) {return(x[["beta"]])})  
alphaInds <- lapply(dat, function(x) {return(x[["alphaInd"]])})
sigmas <- lapply(dat,function(x){return(x[["out"]])})  %>% bind_rows()

rm(dat)
names(betas) <-  sports
names(alphaInds) <- sports

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
  mutate(season = ifelse(sport == "nhl", season + 2005, season + 2004)) %>%
  filter(season != 2015)


#Extract HFA from each
hfa <- select(sigmas, sport, alpha)

#Expand grid

df.sim <- expand.grid(first.seed = 1:8, second.seed = 1:8, season = 2005:2014, sport = sports)
df.sim <- df.sim %>% 
  filter(second.seed > first.seed, season > 2005|sport!="nhl") %>%
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
n.games <- ifelse(temp$sport == "nfl", 1, 7)
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
df.sim1$better.winP[i] <- mean(fave.win)
print(i)
}


#write.csv(df.sim1, "seven.simulations.csv")


##################################################
### df.sim1 stores simulated draws of each series
### probability is probability the better team wins
##################################################
 


## 2005 MLB: transitivity?
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

first.eighth <- filter(df.sim1, second.seed ==4, sport =="mlb"|sport=="nhl")
n.sim <- 1000
n.games <- 59
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