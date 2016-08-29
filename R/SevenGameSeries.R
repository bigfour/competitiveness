library(dplyr) 
library(ggplot2)

sports <- c("mlb", "nba", "nfl", "nhl")
source("config.R")
load("data/bigfour.rda")
library(knitr)


get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_7_23_teamHFA.RData")))
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

probs <- read.csv("data/seven.simulations.csv")
library(dplyr)
library(ggplot2)
probs.1 <- probs %>%
  group_by(sport, second.seed) %>%
  summarise(mean.win = mean(better.winP))

p <- ggplot(probs.1, aes(second.seed, mean.win, group = sport, colour = sport)) 
p + geom_point() + 
  geom_line() + 
  ggtitle("Probability of No. 1 team winning in postseason") + 
  scale_x_continuous("Opponent rank", breaks = 2:8) + 
  scale_y_continuous("", breaks = c(0.6, 0.7, 0.8), labels = c("60%", "70%", "80%"))


p <- ggplot(probs, aes(second.seed, better.winP, group = as.factor(season), colour = as.factor(season))) 
p + geom_point() + 
  geom_line() + 
  theme_bw() + 
  ggtitle("Probability of best team winning in the PS") + 
  scale_x_continuous("Opponent rank", breaks = 2:8) + 
  scale_y_continuous("", breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                     labels = c("50%","60%", "70%", "80%", "90%", "100%")) + 
  facet_wrap(~sport)
 


## 2005 MLB: transitivity?
probs %>% filter(sport=="mlb", season==2005)

teams <- c("New York Yankees", "St Louis Cardinals", "Cleveland Indians")
draws.transitivity <- betas.lastfour.all %>%
  filter(team %in% teams, season==2005)
p <- ggplot(draws.transitivity, aes(x = betas, fill = team))
p + geom_density(alpha = 0.25) + ggtitle("2005 MLB team strengths")
#Text for each year


## How sure are we?  Update these numbers
#NBA probability: 84% after 7 games, 
#NFL probability: 77% after 7 games, 83% after 15 games
#MLB probability: 71% after 7 games, 79% after 15 games, 83% after 25 games, 
#NHL probability: 65% after 7 games, 74% after 15 games, 82% after 35 games







####################
## Assuming no team-numbers (sample top teams. This is a work in progress)
####################

select.teams <- function(df, season, numteams){
  out <- df %>%
  filter(seas==season) %>%
  group_by(unique.names) %>%
  summarise(ave = mean(beta.est), sd = sd(beta.est)) %>%
  arrange(desc(ave)) %>%
  mutate(rank = 1:n()) %>%
  head(numteams)
  return(out)
}

nfl.teams <- select.teams(nfl.beta, 2010, 12)
mlb.teams <- select.teams(mlb.beta, 2010, 8)
nba.teams <- select.teams(nba.beta, 2010, 16)
nhl.teams <- select.teams(nhl.beta, 2010, 16)


function.series <- function(n.games, teams, betas, simulations){
fave.win <- NULL
for (i in 1:simulations){
 pair <- sample(teams, 2, replace = FALSE)
 draws <- betas %>%
   filter(unique.names %in% pair$unique.names, seas == 2010) %>%
   group_by(unique.names) %>%
   do(sample_n(.,n.games)) %>%
   inner_join(pair, by = "unique.names") %>%
   ungroup() %>%
   arrange(rank)
 logit.probs <- draws$beta.est[1:n.games] - draws$beta.est[(n.games+1):(2*n.games)]
        ### Note: want to add in sport-specific variability in draws (e.g., residuals)
 probs <- exp(logit.probs)/(1+exp(logit.probs))
 fave.win[i] <- rbinom(1, n.games, probs) >= ceiling(n.games/2)
}
return(mean(fave.win)); print(mean(fave.win))
}

function.series(101, mlb.teams, mlb.beta, 1000)

## How sure are we?
#NBA probability: 69% after 7 games, 73% after 51 games, 76% after 101 games
#NFL probability: 57% after 7 games, 63% after 51 games, 67% after 101 games
#NHL probability: 55% after 7 games, 59% after 51 games, 65% after 101 games
#MLB probability: 54% after 7 games, 63% after 51 games, 64% after 101 games




