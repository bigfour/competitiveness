# tidy the MCMC output
source("config.R") 
sports <- c("nfl", "nba", "nhl", "mlb")
sport <- "nfl"

get_sport <- function(week) {
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
dat <- lapply(weeks, get_sport)
thetas <- lapply(dat,function(x){return(x[["theta"]])})  


fweek <- function(week){
  week.temp <- thetas[[week-1]][10, week, , , ] ## contains estimates ending in given week of season 10
  return(data.frame(theta = apply(week.temp, 1, "mean"), team_id = 1:32, week = week, season = 10, sport = "nfl")) ## average by team across iteration and chain
}

nfl.avg <- lapply(weeks, fweek) %>%
  bind_rows() 

thetas[[1]][season, week, team, iteration, chain] ## contains estimates ending in week 2


library(broom)
tidy_thetas <- nfl.avg %>%
  bind_rows() %>%
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
stack.game1 <- filter(stack.game1, season == 2015, sport == "nfl")

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


#mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; 
nfl$sport <- "nfl"
#all.sport.winp.remains <- rbind(mlb, nba, nhl, nfl)
all.sport.winp.remains <- rbind(nfl)



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

#mlb <- func.sport2("mlb", 162)
nfl <- func.sport2("nfl", 16)
#nba <- func.sport2("nba", 82)
#nhl <- func.sport2("nhl", 82)


#mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; 
nfl$sport <- "nfl"
#all.sport.pdiff.remains <- rbind(mlb, nba, nhl, nfl)
all.sport.pdiff.remains <- rbind(nfl)




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

#mlb <- func.sport3("mlb", 162)
nfl <- func.sport3("nfl", 16)
#nba <- func.sport3("nba", 82)
#nhl <- func.sport3("nhl", 82)


#mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; nfl$sport <- "nfl"
nfl$sport <- "nfl"




### new files
week.data <- tidy_thetas.week %>% select(sport, season, name.x, week, theta) %>% rename(team = name.x) %>% mutate(season = 2015)


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

#mlb <- func.sport3("mlb", 162)
nfl <- func.sport3("nfl", 16)
#nba <- func.sport3("nba", 82)
#nhl <- func.sport3("nhl", 82)


#mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; nfl$sport <- "nfl"
nfl$sport <- "nfl"

all.sport.week <- nfl

#all.sport.theta.remains <- rbind(mlb, nba, nhl, nfl)
all.sport.theta.remains <- rbind(nfl)
all.sport.theta.remains$type <- "est.theta"
all.sport.winp.remains$type <- "winp"
all.sport.pdiff.remains$type <- "pdiff"
all.sport.week$type <- "week"


all.sport.both <- rbind(all.sport.theta.remains, all.sport.winp.remains, all.sport.pdiff.remains, all.sport.week)
#pdiff is 1, thetas is 2, winp is 3
gg.r2 <- ggplot(all.sport.both, aes(day.season, corr.day, lty = type, colour = type)) +
  geom_line() + 
  scale_y_continuous(labels = scales::percent, "", lim = c(0, .8)) + 
  scale_x_continuous("Game of season") +
  #scale_linetype_manual(labels = c("Our estimates", "Point differential", "Win %", "Weekly"), 
   #                     values = c(1, 2, 3, 4), "Type") +
  facet_wrap(~toupper(sport), scales = "free")  + 
  labs(title = "Coefficient of determination with future in-season win %") 

gg.r2
