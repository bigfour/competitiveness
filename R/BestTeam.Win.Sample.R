source("config.R")
sports <- c("mlb", "nba", "nfl", "nhl")
library(dplyr)
get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_paper_teamHFA.RData")))
  out <- data.frame(
    sigma_game = 1/z$sigma[,,1],
    alpha = z$alpha[,,1]
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

head(params)

thetas <- lapply(dat,function(x){return(x[["theta"]])})  
names(thetas) <-  sports

alphaInds <- lapply(dat, function(x) {return(x[["alphaInd"]])})
names(alphaInds) <- sports

# to save memory!
rm(dat)


# 1 For each sport: 
# 2 Pick a season
# 3 Pick a week 
# 4 Pick two teams
# 5 Sample team strenghts
# 6 Sample HA
# 7 Sample game level error
# 8 Impute simulated log-odds
# 9 Repeat (2) - (8)

## Step 1a: Required for function

fun.bestwin <- function(sport, thetas.sport){

## Step 1b: functions of sport
season.max <- ifelse(sport == "nfl", 10, 11)
week.max <- ifelse(sport == "nfl", 17, 
                   ifelse(sport == "nba", 25, 28))
teams.list <- ifelse(sport == "nfl", 32, 30)

## Start loop here
probs.sport <- NULL
probs.sportHA <- NULL

for (i in 1:1000){
## Step 2
season <- sample(1:season.max, 1)

## Step 3
week <- sample(1:week.max, 1)

## Step 4
teams <- sample(1:teams.list, 2, replace = FALSE)

## Step 5a

thetas1 <- thetas.sport[season, week, teams[1],sample(1:4000, 1),sample(1:3, 1)]
thetas2 <- thetas.sport[season, week, teams[2],sample(1:4000, 1),sample(1:3, 1)]

## Step 5b: ensure team1 is better team

#team1.mean <- mean(thetas.sport[season, week, teams[1],,])
#team2.mean <- mean(thetas.sport[season, week, teams[2],,])


## Step 6
sport_ <- sport
HA.league <- params %>% filter(sport == sport_) %>% 
  select(alpha) 
HA <- sample(HA.league$alpha, 1)

## Step 7
sigma.league <- params %>% mutate(sigma_game = sqrt(sigma_game)) %>% 
  select(sigma_game) 
sigma <- sample(sigma.league$sigma_game, 1)
epsilon <- rnorm(1, 0, sigma)
## Could be positive or negative
#sigma <- 0

## Step 8

## Log odds would be positive if Team 1 better on that draw, negative if Team 2 better on that draw
## Take the absolute value to ensure all probabilities start at 0.5 to reflect the "Favorite"
log.odds <- abs(thetas1 - thetas2 + epsilon)


prob.sim <- exp(0 + log.odds)/
  (1 + exp(0 + log.odds))

prob.simHA <- exp(HA + log.odds)/
  (1 + exp(HA + log.odds))

probs.sport <- c(probs.sport, prob.sim)
probs.sportHA <- c(probs.sportHA, prob.simHA)
}

return(list(Probs = probs.sport, ProbsHA = probs.sportHA))
}
set.seed(0)
nba <- fun.bestwin("nba", thetas$nba)
nhl <- fun.bestwin("nhl", thetas$nhl)
mlb <- fun.bestwin("mlb", thetas$mlb)
nfl <- fun.bestwin("nfl", thetas$nfl)

df.nba <- data.frame(sport = "NBA", probs = nba$Probs, probsH = nba$ProbsHA)
df.nfl <- data.frame(sport = "NFL", probs = nfl$Probs, probsH = nfl$ProbsHA)
df.mlb <- data.frame(sport = "MLB", probs = mlb$Probs, probsH = mlb$ProbsHA)
df.nhl <- data.frame(sport = "NHL", probs = nhl$Probs, probsH = nhl$ProbsHA)

cdf.all <- rbind(df.mlb, df.nfl, df.nba, df.nhl)
save(cdf.all, file = file.path(root, "data", "bestwin.rda"))  

p <- ggplot(cdf.all) + 
  stat_ecdf(aes(probs, colour = sport)) + 
  stat_ecdf(data = cdf.all, aes(probsH, colour = sport), lty = "dotted") + 
  ggtitle("How often does the best team win?") + 
  geom_vline(xintercept = 0.5, colour = "black", lty = 5) + 
  geom_vline(xintercept = 1, colour = "black", lty = 5) + 
  labs(subtitle = "Solid: neutral site, Dashed: home game for better team") +
  xlab("Simulated win probability") + ylab("CDF") + xlim(0.5, 1.0) + 
  annotate("text", x = .51, y = 1,  hjust = 0, vjust = 1, label = paste("All games \n coin flips"))+ 
  annotate("text", x = .99, y = 0,  hjust = 1, vjust = 0, label = paste("All games \n pre-determined")) + 
  scale_colour_brewer(palette = "Spectral", "League")
#p
#ggsave(plot = p, width = 6, height = 3.5, filename = "figure/BestWin.pdf")


### Note: preferred to sample with more games, but dotted lines don't work too well


### CDFs without HA
P <- ecdf(nfl$Probs)
z <- seq(0.5, 1, by = 0.00001)
nfl.cdf <- P(z)

P <- ecdf(nba$Probs)
nba.cdf <- P(z)

P <- ecdf(mlb$Probs)
mlb.cdf <- P(z)

P <- ecdf(nhl$Probs)
nhl.cdf <- P(z)

cdf.df <- data.frame(Probability = rep(z, 4),
                     cdf = c(nfl.cdf, nhl.cdf, nba.cdf, mlb.cdf), 
                     sport = rep(c("NFL", "NHL", "NBA", "MLB"), each = length(z)), 
                                 Type = "No HA")


library(zoo)

### CDFs with HA
P <- ecdf(nfl$ProbsHA)
z <- seq(0.5, 1, by = 0.00001)
nfl.cdf <- P(z)

P <- ecdf(nba$ProbsHA)
nba.cdf <- P(z)

P <- ecdf(mlb$ProbsHA)
mlb.cdf <- P(z)

P <- ecdf(nhl$ProbsHA)
nhl.cdf <- P(z)

cdf.dfHA <- data.frame(Probability = rep(z, 4),
                     cdf = c(nfl.cdf, nhl.cdf, nba.cdf, mlb.cdf), 
                     sport = rep(c("NFL", "NHL", "NBA", "MLB"), each = length(z)), 
                     Type = "HA")
cdf.all2 <- rbind(cdf.df, cdf.dfHA)


### Area under the curve
cdf.all2 %>% group_by(Type, sport) %>%
  summarise(AUC = 2*sum(diff(Probability)*rollmean(cdf,2)))

x <- z
y <- nhl.cdf
id <- order(x)
AUC <- sum(diff(z)*rollmean(y,2))


#cdf.all %>% mutate(dist.50 = abs(probs - 0.5), dist.50H = abs(probsH - 0.5)) %>%
#  group_by(sport) %>% summarise(ave.dist = mean(dist.50), ave.distH = mean(dist.50H))

