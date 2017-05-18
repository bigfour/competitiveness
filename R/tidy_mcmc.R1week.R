# tidy the MCMC output
source("config.R") 
sports <- c("nfl", "nba", "nhl", "mlb")

#### This file will get the week by week theta's
#### This file will also make a coefficient of determination plot with future win percentage comparing four methods
#### This file will also compare log losses'

get_sport <- function(week, sport) {
  message(paste("reading week", week, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_paper_teamHFA.R1.Week", week, ".RData")))
  out <- data.frame(
    week = week
  ) %>%
    mutate(sport = sport)
  theta <- z$theta
  rm(z)
  return(list(out = out, theta = theta))
}

# Extract the values we will need
weeks <- 2:17
dat.nfl <- lapply(weeks, sport = "nfl", get_sport)

weeks <- 2:24
dat.nba <- lapply(weeks, sport = "nba", get_sport)

weeks <- 2:28
dat.nhl <- lapply(weeks, sport = "nhl", get_sport)

weeks <- 2:28
dat.mlb <- lapply(weeks, sport = "mlb", get_sport)


thetas.nfl <- lapply(dat.nfl,function(x){return(x[["theta"]])})
thetas.nba <- lapply(dat.nba,function(x){return(x[["theta"]])})
thetas.nhl <- lapply(dat.nhl,function(x){return(x[["theta"]])})
thetas.mlb <- lapply(dat.mlb,function(x){return(x[["theta"]])})


## Format: 
#thetas[[1]][season, week, team, iteration, chain] ## contains estimates ending in week 2

## Exists: thetas[[2]][10, 3, 1, , ], thetas[[2]][10, 2, 1, , ], thetas[[2]][10, 1, 1, , ]
## Does not exist: thetas[[2]][10, 4, 1, , ]

fweek <- function(week, thetas, nteams, sport){
  last.seas <- 11
  if (sport == "nfl"){last.seas <- 10}
  week.temp <- thetas[[week-1]][last.seas, week, , , ] ## contains estimates ending in given week of last season
  return(data.frame(theta = apply(week.temp, 1, "mean"), team_id = 1:nteams, week = week, season = last.seas, sport = sport)) ## average by team across iteration and chain
}

nfl.avg <- lapply(2:17, fweek, thetas = thetas.nfl, nteams = 32, sport = "nfl") %>%
  bind_rows()
nba.avg <- lapply(2:24, fweek, thetas = thetas.nba, nteams = 30, sport = "nba") %>%
  bind_rows()  
nhl.avg <- lapply(2:28, fweek, thetas = thetas.nhl, nteams = 30, sport = "nhl") %>%
  bind_rows()  
mlb.avg <- lapply(2:28, fweek, thetas = thetas.mlb, nteams = 30, sport = "mlb") %>%
  bind_rows()  



library(broom)
tidy_thetas <- bind_rows(
  nfl.avg %>%
    bind_rows(), 
  nba.avg %>% 
    bind_rows(), 
  nhl.avg %>% 
    bind_rows(), 
  mlb.avg %>% 
    bind_rows()) %>%
  mutate(max.week = ifelse(sport == "nfl", 17, ifelse(sport == "nba", 24, 28))) %>% 
  mutate(time_val = 2004 + season + week / max.week)





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


tidy_thetas.week <- tidy_thetas %>%
  inner_join(colors, by = c("sport" = "sport", "team_id" = "team_id"))

# save the results so we don't have to do this everytime. 
save(tidy_thetas.week, file = file.path(root, "data", "tidy_thetas.R1.week.rda"), compress = "xz")





load(file.path(data_raw, "bigfour.rda"))
load(file.path(root, "data", "tidy_thetas.R1.rda"))


bigfour <- bigfour %>% 
  arrange(sport, Date)

min.day <- bigfour %>%
  group_by(sport, season) %>%
  summarise(min.day = min(gameDate), max.day = max(gameDate))

bigfour1 <- bigfour %>%
  left_join(min.day) %>%
  mutate(day = as.Date(gameDate) - as.Date(min.day),
         days.left = as.Date(max.day) - as.Date(gameDate), 
         max.days = day + days.left, 
         percent = as.numeric(day)/as.numeric(max.days), 
         scorediff.vis = visitor_score - home_score, 
         scorediff.home = -1*scorediff.vis)


date.vis <- bigfour1 %>% 
  select(gameDate, day, visitor_team, season, home_win, sport, scorediff.vis) %>%
  rename(team = visitor_team, pdiff = scorediff.vis) %>%
  mutate(win = !home_win) %>%
  select(-home_win)

date.home <- bigfour1 %>% 
  select(gameDate, day, home_team, season, home_win, sport, scorediff.home) %>%
  rename(team = home_team, pdiff = scorediff.home) %>%
  mutate(win = home_win) %>%
  select(-home_win)

stack.game <- rbind(date.vis, date.home)
stack.game <- stack.game %>%
  arrange(sport, season, team, day) 

n.season <- data.frame(sport = c("nhl", "nba", "mlb", "nfl"), 
                       season.n = c(82, 82, 162, 16))

stack.game1 <- stack.game %>%
  group_by(season, team) %>%
  mutate(n.game = row_number(), cum.wins = cumsum(win), 
         win.p = cum.wins/n.game, percent.season = n.game/n(), 
         wins.final = sum(win), win.p.final = wins.final/n(), 
         win.p.left = (wins.final - cum.wins)/(n()-n.game), 
         cum.diff = cumsum(pdiff)) #%>%
#filter(!(sport == "nba" & n.game > 20))

stack.game1 <- filter(stack.game1, !(season==2012&sport == "nba"),
                      !(season==2013 & sport == "nhl"))
stack.game1 <- filter(stack.game1, (season == 2015 & sport == "nfl") | (season == 2016) )

####################################################
## Next step: consider comparing wins and losses vs. thetas at predicting 
## end of regular season wins and losses
##########################

############ Correlation of current win percentage with remaining win percentage

func.sport2 <- function(sports, days){
  names.sport <- subset(stack.game1, sport == sports)  
  corr.grid <- expand.grid(day.season = 1:days, corr.day = NA)
  for (i in 1:days){
    day.sport <- subset(stack.game1, n.game == i & sport == sports)
    corr.grid[i, 2] <- cor(day.sport$win.p, day.sport$win.p.left, use = "pairwise.complete.obs")^2
  }
  return(corr.grid)
}

nfl <- func.sport2("nfl", 16)
nba <- func.sport2("nba", 82)
nhl <- func.sport2("nhl", 82)
mlb <- func.sport2("mlb", 162)



mlb$sport <- "mlb"
nba$sport <- "nba"
nhl$sport <- "nhl"
nfl$sport <- "nfl"
all.sport.winp.remains <- rbind(mlb, nba, nhl, nfl)



############ Correlation of current point differential with remaining win percentage

func.sport2 <- function(sports, days){
  names.sport <- subset(stack.game1, sport == sports)  
  corr.grid <- expand.grid(day.season = 1:days, corr.day = NA)
  for (i in 1:days){
    day.sport <- subset(stack.game1, n.game == i & sport == sports)
    corr.grid[i, 2] <- cor(day.sport$cum.diff, day.sport$win.p.left, use = "pairwise.complete.obs")^2
  }
  return(corr.grid)
}

mlb <- func.sport2("mlb", 162)
nfl <- func.sport2("nfl", 16)
nba <- func.sport2("nba", 82)
nhl <- func.sport2("nhl", 82)


mlb$sport <- "mlb"; 
nba$sport <- "nba" 
nhl$sport <- "nhl" 
nfl$sport <- "nfl"
all.sport.pdiff.remains <- rbind(mlb, nba, nhl, nfl)




############ Correlation of current team strength estimate with remaining win percentage
tidy_thetas <- filter(tidy_thetas, !(season==7&sport == "nba"),
                      !(season==8 & sport == "nhl"), !(season==2 & sport == "nfl"))

unique.weeks <- tidy_thetas %>%
  rename(team = name) %>%
  mutate(time_val = time_val + 1, season = floor(time_val)) %>% 
  arrange(sport, season, team) %>%
  select(sport, season, team, week, theta)

min.day <- stack.game1 %>%
  group_by(season, sport) %>%
  summarise(min.day = min(gameDate))

stack.game2 <- stack.game1 %>%
  left_join(min.day) %>%
  mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7)+1)) %>%
  left_join(unique.weeks)


func.sport3 <- function(sports, week){
  names.sport <- subset(stack.game2, sport == sports)  
  corr.grid <- expand.grid(day.season = 1:week, corr.day = NA)
  for (i in 1:week){
    day.sport <- subset(stack.game2, n.game == i & sport == sports)
    corr.grid[i, 2] <- cor(day.sport$theta, day.sport$win.p.left, 
                           use = "pairwise.complete.obs")^2
  }
  return(corr.grid)
}

mlb <- func.sport3("mlb", 162)
nfl <- func.sport3("nfl", 16)
nba <- func.sport3("nba", 82)
nhl <- func.sport3("nhl", 82)


mlb$sport <- "mlb"
nba$sport <- "nba"
nhl$sport <- "nhl"
nfl$sport <- "nfl"
all.sport.theta.remains <- rbind(nfl, nba, nhl, mlb)
all.sport.theta.remains$type <- "est.theta"




### new files
week.data <- tidy_thetas.week %>% 
  select(sport, season, name, week, theta) %>% 
  rename(team = name) %>% mutate(season = ifelse(sport == "nfl", 2015, 2016))


min.day <- stack.game1 %>%
  group_by(season, sport) %>%
  summarise(min.day = min(gameDate))

stack.game2 <- stack.game1 %>%
  left_join(min.day) %>%
  mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7)+1)) %>%
  left_join(week.data)


func.sport3 <- function(sports, week){
  names.sport <- subset(stack.game2, sport == sports)  
  corr.grid <- expand.grid(day.season = 1:week, corr.day = NA)
  for (i in 1:week){
    day.sport <- subset(stack.game2, n.game == i & sport == sports)
    corr.grid[i, 2] <- cor(day.sport$theta, day.sport$win.p.left, 
                           use = "pairwise.complete.obs")^2
  }
  return(corr.grid)
}

mlb <- func.sport3("mlb", 162)
nfl <- func.sport3("nfl", 16)
nba <- func.sport3("nba", 82)
nhl <- func.sport3("nhl", 82)


mlb$sport <- "mlb"
nba$sport <- "nba"
nhl$sport <- "nhl"
nfl$sport <- "nfl"

all.sport.week <- rbind(nfl, nba, nhl, mlb)



all.sport.winp.remains$type <- "winp"
all.sport.pdiff.remains$type <- "pdiff"
all.sport.week$type <- "week"


all.sport.both <- rbind(all.sport.theta.remains, all.sport.winp.remains, all.sport.pdiff.remains, all.sport.week)
#pdiff is 1, thetas is 2, winp is 3
gg.r2 <- ggplot(all.sport.both, aes(day.season, corr.day, lty = type)) +
  geom_line() + 
  scale_y_continuous(labels = scales::percent, "", lim = c(0, .85)) + 
  scale_x_continuous("Game of season") +
  scale_linetype_manual(labels = c("Simultaneous", "Point differential", "Sequential", "Win %"), 
                        values = c(1, 2, 3, 4), "Type") +
  facet_wrap(~toupper(sport), scales = "free")  + 
  labs(title = "Coefficient of determination with future in-season win %") + 
  facet_wrap(~sport, scales = "free_x")

gg.r2



#### Log loss

load(file.path(root, "data", "tidy_thetas.R1.rda"))
bigfour.last <- filter(bigfour, (season == 2015 & sport == "nfl") | (season == 2016))
tidy_thetas <- filter(tidy_thetas, (season == 10 & sport == "nfl") | (season == 11))
min.day <- bigfour.last %>%
  group_by(season) %>%
  summarise(min.day = min(gameDate))

bigfour.last <- bigfour.last %>%
  left_join(min.day) %>%
  mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7) + 1))
load(file.path("data", "tidy_alphas.R1.rda"))
tidy_alphas <- select(tidy_alphas, alpha.team.overall, team)
bigfour.last <- bigfour.last %>% left_join(tidy_alphas, by = c("home_team" = "team"))


bigfour.last <- bigfour.last %>% 
  left_join(select(tidy_thetas.week, theta, week, name), by = (c("visitor_team" = "name", "week" = "week"))) %>% 
  rename(vis_thetaS = theta) %>%
  left_join(select(tidy_thetas.week, theta, week, name), by = (c("home_team" = "name", "week" = "week"))) %>% 
  rename(home_thetaS = theta) %>%
  mutate(p_homeS = exp(alpha.team.overall + home_thetaS - vis_thetaS)/(1 + exp(alpha.team.overall + home_thetaS - vis_thetaS)))%>%
  left_join(select(tidy_thetas, theta, week, name), by = (c("visitor_team" = "name", "week" = "week"))) %>% 
  rename(vis_thetaC = theta) %>%
  left_join(select(tidy_thetas, theta, week, name), by = (c("home_team" = "name", "week" = "week"))) %>% 
  rename(home_thetaC = theta) %>%
  mutate(p_homeC = exp(alpha.team.overall + home_thetaC - vis_thetaC)/(1 + exp(alpha.team.overall + home_thetaC - vis_thetaC)))
         
bigfour.last.ll <- bigfour.last %>% 
  mutate(log.loss.true = home_win*log(p_home) + (1-home_win)*log(1-p_home), 
         log.loss.Sequential = home_win*log(p_homeS) + (1-home_win)*log(1-p_homeS), 
         log.loss.Cumulative = home_win*log(p_homeC) + (1-home_win)*log(1-p_homeC)) 




temp <- gather(bigfour.last.ll, "type", "logloss", log.loss.true:log.loss.Cumulative) %>% select(Date: ml_home, home_win, type, sport, logloss, week) %>%
   filter(week > 1)

meds <- temp %>% group_by(sport, type) %>% summarise(ave.loss = mean(logloss, na.rm = TRUE), med.loss = median(logloss, na.rm = TRUE)) 
meds

ggplot(temp, aes(x = logloss, colour = type, fill = type)) + geom_density(alpha = 0.2) + 
   ggtitle("Density curves of game level log loss") + 
   geom_vline(xintercept = log(0.5), lty = 2) + 
   annotate("text", x = -1, y = 1.2, label = "Coin flip")  + facet_wrap(~sport)
## Add medians or means to the plot... add in coin toss line

ggplot(bigfour.last.ll, aes(log.loss.Cumulative - log.loss.Sequential)) + geom_histogram() + geom_vline(xintercept= 0) + 
  facet_wrap(~sport, scales = "free_y") + ggtitle("Differences in game level log loss, cumulative - sequential")  + 
  xlab("Difference in log loss (sequential model predictions are better < 0)")


