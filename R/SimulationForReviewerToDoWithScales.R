# We want to repeat the constant home advantage model (model CHA, jags model constantHFA_R2.bug) under four scenarios
# 
# 1) Using the observed data. (We may not even need to simulate this, and can just use our resulting output if we want)
# 2) Reversing the order of the home advantages:
#   
#   MLB gets the NBA's, and visa versa
# NFL gets the NHL's, and visa versa
# 
# 3) Every game gets the NBA's home advantage
# 4) Every game gets the MLB's home advantage
# 
# Under those four scenarios, we run Model CHA and explore the output.
# 
# For each week, season, and in each sport, we take the standard deviation of the team strength parameters from the posterior distribution. We compare each of the three simulated model's to the observed data (this could be a scatter plot with about 250 points (25 weeks x 10 seasons), or we could average over weeks and seasons and turn into a table). 

source("config.R")
library(rjags)
load(file.path(data_raw, "bigfour.rda"))

### Here's the logit function
logit <- function(p) { 
  out <- log(p/(1 - p))
  return(out)
}


asinTransform <- function(p) { asin(sqrt(p)) }

alphaList<- sdList <- list()
set.seed(1234)
yList <- postPred <- list()
#leagueAlpha<-"mlb"
leagues <- c("mlb","nhl","nfl","nba")
df.sim <- data.frame(league = leagues, 
                     alpha.sim.1 = leagues, 
                     alpha.sim.2 = rev(leagues), 
                     alpha.sim.3 = "nba", 
                     alpha.sim.4 = "mlb")

### Read in data

get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_paper_constantHFA.R1.RData")))
  out <- data.frame(
    sigma_g = 1/z$tauGame[,,1],
    alpha = z$alpha[,,1]
  ) %>%
    mutate(sport = sport)
  theta <- z$theta
  rm(z)
  return(list(out = out, theta = theta))
}

# Extract the values we will need
dat <- lapply(leagues[1:4], get_sport)

params <- lapply(dat, function(x) { return(x[["out"]]) } ) %>% 
  bind_rows()

thetas <- lapply(dat,function(x){return(x[["theta"]])})  
names(thetas) <-  leagues





sim_scales <- function(league.thetas, league.hfa){
  league <- league.thetas ## league where we'll draw games and thetas from
  hfa <- league.hfa ## league that we'll draw hfa from
  
  test <- subset(bigfour, sport == league)
  min.day <- test %>%
    group_by(season) %>%
    summarise(min.day = min(gameDate))
  
  test <- test %>%
    left_join(min.day) %>%
    mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7) + 1))
  head(test)
  
  
  
  w <- test$week
  s <- test$season - min(test$season) + 1
  table(s, w)
  
  
  #create a design matrix 
  Teams <- sort(as.character(unique(c(as.character(test$home_team),
                                      as.character(test$visitor_team)))))
  
  #Defining the number of things
  nTeams <- length(Teams)
  nWeeks <- max(test$week)
  nSeas <- max(s)
  n <- nrow(test)
  
  #Defining the design matrix
  x <- matrix(0, nrow = dim(test)[1], ncol = length(Teams))
  for (i in 1:dim(test)[1]) {
    x[i, which(as.character(test[i,"home_team"]) == Teams)] <- (1)
    x[i, which(as.character(test[i,"visitor_team"]) == Teams)] <- (-1)
  } 
  print("...finished design matrix...")
  
  #HFA index
  zzz <- apply(x, 1, function(x){ which(x == 1) })
  #z<-x
  #z[z==(-1)]<-0
  
  #HFA changes: create a new term for old arena's 
  ### Seattle to OKC
  if (league == "nba") {
    zzz[test$season < 2009 & test$home_team == "Oklahoma City Thunder"] <- max(zzz) + 1}
  
  ### Atlanta to Winnipeg
  if (league == "nhl") {
    zzz[test$season < 2012 & test$home_team == "Winnipeg Jets"] <- max(zzz) + 1}
  
  nHFAs <- max(zzz)
  
  ######################################################################
  #####    Sample From posterior distribution
  ######################################################################
  #Create a vector of thetas for a season a and week
  
    draw <- sample(1:4000,1)
    chain <- sample(1:3,1)
    
    thetaMat <- thetas[[league.thetas]][,,,draw,chain]
    
    
    
    if (hfa == "none"){
      alpha <- 0
    }
    
    if (hfa != "none"){
      alpha.league <- subset(params, sport == hfa)
      alpha <- alpha.league$alpha[draw]
    }
    
    tau.league <- subset(params, sport == league)
    tauGame<- sqrt(tau.league$sigma_g[draw])
    
    
    mu<-rep(NA,nrow(x))
    
    for (i in 1:nrow(x)){
      mu[i]<- alpha  + c(thetaMat[s[i],w[i],]%*%x[i,])
    }
  
    #alphaList[[paste(leagueAlpha)]] <- alpha
    yList[[paste(league.thetas, league.hfa)]] <- y <-rnorm(length(mu), mu, tauGame)

    
    ### Run the model on the simulated data
          bugFile <- file.path("R/jags_model_constantHFA_R1.bug")
          n.adapt = 100
          n.update = 200
          n.draws = 100
          thin = 5
          n.chains = 3
          fit.type = "team"
          posteriorDraws = c('alpha','theta','tauGame','tauWeek',
                             'tauSeason','gammaWeek','gammaSeason')
                               
          
          jags <- jags.model(bugFile,
                             data = list('y' = y,'x' = x, 's' = s, 'w' = w, 'n' = n, 
                                         'z' = zzz, 'nTeams' = nTeams, 
                                         'nWeeks' = nWeeks, 'nHFAs' = nHFAs, 'nSeas' = nSeas), 
                             n.chains = n.chains, n.adapt = n.adapt)
          
          update(jags, n.update)
          out <- jags.samples(jags, posteriorDraws, n.draws, thin = thin)

 ## Extract standard deviation of theta's at each week
 temp<-list()
 
 for (i in 1:dim(out$theta)[1]){
    temp[[i]] <- apply(out$theta, c(1,2,3), mean)[i,,]
 }
 sd.week <- apply(do.call(rbind,temp), 1, sd)
 time <- expand.grid(week = 1:nWeeks, seas = 1:nSeas)
 df.out <- data.frame(sd.week, league.thetas, league.hfa)
 df.out <- bind_cols(df.out, time)
 return(df.out)
}


## Simulation 1 (observed)

load(file.path(root, "data", "tidy_thetas.R2.rda"))
sim1 <- tidy_thetas %>% 
  group_by(sport, season, week) %>% 
  summarise(sd.week = sd(theta)) %>% 
  rename(league.thetas = sport, seas = season) %>% 
  mutate(league.hfa = league.thetas) %>% 
  select(sd.week, league.thetas, league.hfa, week, seas) %>%
   mutate(sim.type = "Observed")

## Simulation 2 (reversed)

mlb.nba <- sim_scales("mlb", "none")
nhl.nfl <- sim_scales("nhl", "none")
nfl.nhl <- sim_scales("nfl", "none")
nba.mlb <- sim_scales("nba", "none")

sim2 <- bind_rows(mlb.nba, nhl.nfl, nfl.nhl, nba.mlb) %>%
  mutate(sim.type = "None")



## Simulation 3 (all NBA)

mlb.nba <- sim_scales("mlb", "nba")
nhl.nba <- sim_scales("nhl", "nba")
nfl.nba <- sim_scales("nfl", "nba")
nba.nba <- sim_scales("nba", "nba")

sim3 <- bind_rows(mlb.nba, nhl.nba, nfl.nba, nba.nba) %>%
  mutate(sim.type = "All NBA")



## Simulation 4 (all MLB)

mlb.mlb <- sim_scales("mlb", "mlb")
nhl.mlb <- sim_scales("nhl", "mlb")
nfl.mlb <- sim_scales("nfl", "mlb")
nba.mlb <- sim_scales("nba", "mlb")

sim4 <- bind_rows(mlb.mlb, nhl.mlb, nfl.mlb, nba.mlb) %>%
  mutate(sim.type = "All MLB")

ave.sport <- sim1 %>% 
  group_by(league.thetas) %>% 
  summarise(ave.sd = mean(sd.week))
  

df.all <- bind_rows(sim1, sim2, sim3, sim4) %>% 
  mutate(max.week = ifelse(league.thetas == "nfl", 17, ifelse(league.thetas == "nba", 24, 28)), 
         time_val = 2004 + seas + week / max.week)

p1 <- ggplot(df.all, aes(time_val, sd.week, colour = sim.type)) + 
  geom_line() + 
  geom_point(size = 0.3) + 
  scale_y_continuous(breaks = c(0, round(ave.sport$ave.sd, 2)), lim = c(0, .95)) + 
  geom_hline(data = ave.sport, aes(yintercept = ave.sd), lty = 2) + 
  facet_wrap(~ toupper(league.thetas))  + 
  xlab("Season") + 
  ylab("") + 
  scale_color_brewer(palette = "Set2", "") + 
  ggtitle("Week-level standard deviations") 
p1 + theme_bw(14)
ggsave("~/Dropbox/Compete/figure/scales_simulated.png", p1)



#save(postPred,file="/Users/gregorymatthews/Dropbox/competitivenessGit/postPred.RData")
#save(yList,file="/Users/gregorymatthews/Dropbox/competitivenessGit/yList.RData")

