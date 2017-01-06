library(dplyr); library(ggplot2)
load("data/bigfour.rda"); library(dplyr)
bigfour <- bigfour %>% arrange(sport, Date)
head(bigfour)


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
sample_n(bigfour1, 5) %>% print.data.frame()


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


n.season <- data.frame(sport = c("nhl", "nba", "mlb", "nfl"), season.n = c(82, 20, 162, 16))

stack.game1 <- stack.game %>%
  group_by(season, team) %>%
  mutate(n.game = row_number(), cum.wins = cumsum(win), 
         win.p = cum.wins/n.game, percent.season = n.game/n(), 
         wins.final = sum(win), win.p.final = wins.final/n(), 
         win.p.left = (wins.final - cum.wins)/(n()-n.game), 
         cum.diff = cumsum(pdiff)) #%>%
  #filter(!(sport == "nba" & n.game > 20))

stack.game1 <- filter(stack.game1, !(season==2012&sport == "nba"),
                      !(season==2013 & sport == "nhl"), !(season==2007 & sport == "nfl"))




####################################################
## Next step: consider comparing wins and losses vs. betas at predicting 
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

mlb <- func.sport2("mlb", 162)
nfl <- func.sport2("nfl", 16)
nba <- func.sport2("nba", 82)
nhl <- func.sport2("nhl", 82)


mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; nfl$sport <- "nfl"
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


mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; nfl$sport <- "nfl"
all.sport.pdiff.remains <- rbind(mlb, nba, nhl, nfl)




############ Correlation of current team strength estimate with remaining win percentage

load("data/tidy_betas.rda")
head(tidy_betas)
tidy_betas <- filter(tidy_betas, !(season==7&sport == "nba"),
                      !(season==8 & sport == "nhl"), !(season==2 & sport == "nfl"))

unique.weeks <- tidy_betas %>%
  rename(team = name) %>%
  mutate(time_val = time_val + 1, season = floor(time_val)) %>% 
  arrange(sport, season, team) %>%
  select(sport, season, team, week, beta)

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
    corr.grid[i, 2] <- cor(day.sport$beta, day.sport$win.p.left, 
                           use = "pairwise.complete.obs")^2
  }
  return(corr.grid)
}

mlb <- func.sport3("mlb", 162)
nfl <- func.sport3("nfl", 16)
nba <- func.sport3("nba", 82)
nhl <- func.sport3("nhl", 82)


mlb$sport <- "mlb"; nba$sport <- "nba"; nhl$sport <- "nhl"; nfl$sport <- "nfl"


all.sport.beta.remains <- rbind(mlb, nba, nhl, nfl)
all.sport.beta.remains$type <- "betas"
all.sport.winp.remains$type <- "winp"
all.sport.pdiff.remains$type <- "pdiff"

all.sport.both <- rbind(all.sport.beta.remains, all.sport.winp.remains, all.sport.pdiff.remains)



r2 <- ggplot(all.sport.both, aes(day.season, corr.day, lty = type)) +
  geom_step() + scale_y_continuous(labels = scales::percent, "", lim = c(0, 1)) + 
  scale_x_continuous("Game of season") +
  scale_linetype_manual(labels = c("Our estimates", "Past point differential", "Past win %"), 
                        values = c(1, 2, 3)) +
  facet_wrap(~toupper(sport), scales = "free")  + 
  labs(title = "Coefficient of determination with year end win %") 

ggsave(plot = r2, width = 6, height = 4, filename = "figure/R2-1.pdf")





