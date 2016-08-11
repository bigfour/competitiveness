
library(rjags)
library(dplyr)
load("data/bigfour.rda")
bigfour <- select(bigfour, -dh.x, -dh.y)
bigfour <- filter(bigfour, !is.na(vig))


nfl <- filter(bigfour, sport =="nfl")
names1 <- sort(unique(nfl$visitor_team))
names1.map <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
                "DET", "GB", "HOU", "IND", "JAC", "KC", "MIA", "MIN", "NE", "NO", "NYG",
                "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "STL", "TB", "TEN", "WAS")
df.teams <- data.frame(names1, names1.map)


nfl <- filter(bigfour, sport =="nfl")
ps <- read.csv("data/GAME.csv")
ps$home_win <- ps$ptsh > ps$ptsv
ps$quad.term <- ps$sprv^2
fit <- glm(home_win ~ sprv + quad.term, data = ps, family = "binomial")
ps$p_home_AA <- fitted(fit)


min.day <- nfl %>%
  group_by(season) %>%
  summarise(min.day = min(gameDate))

nfl <- nfl %>%
  left_join(min.day) %>%
  mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7)+1))
head(nfl)

nfl1 <- left_join(nfl, df.teams, by = c("home_team" = "names1"))

nfl2 <- left_join(nfl1, ps, by = c("names1.map" = "h", "week" = "week", "season"="seas"))

ggplot(nfl2, aes(x = p_home_AA, y = p_home)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)



 nfl2 %>%
  mutate(diff.prob = p_home_AA - p_home) %>%
  filter(abs(diff.prob) > 0.1) %>%
  group_by(season) %>%
  summarise(length.seas = n())










ps <- mutate(ps, p_home_cat = cut(p_home_AA, 15))
ps %>%
  group_by(p_home_cat) %>%
  summarise(m.homewin = mean(home_win), n.games = n())

ps <- ps %>%
  arrange(p_home_AA) %>%
  mutate(ecdf.game = cumsum(home_win)/c(1:n()))

ggplot(ps, aes(x = p_home_AA, y = ecdf.game)) + 
  geom_point() 


bigfour %>% filter(sport == "nfl", p_home < 0.42, p_home > 0.3) %>% summarise(p.win = mean(home_win))

bigfour %>% filter(sport == "nfl", p_home < 0.42, p_home > 0.3) %>% sample_n(5)
