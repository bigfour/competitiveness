source("config.R")
library(rjags)
load(file.path(data_raw, "bigfour.rda"))

### Here's the logit function

logit <- function(p) { 
  out <- log(p/(1 - p))
  return(out)
}


asinTransform <- function(p) { asin(sqrt(p)) }


jagsModel <- function(data,
                      league, 
                      n.adapt = 100,
                      n.update = 2000,
                      n.draws = 10000,
                      thin = 5, 
                      n.chains = 3, 
                      bugFile = bugFile, 
                      fit.type = fit.type, 
                      posteriorDraws = c('alpha','theta','tauGame','tauWeek',
                                         'tauSeason','gammaWeek','gammaSeason')){
  
  
  test <- subset(bigfour, sport == league)
  greg <- as.Date(gsub(" 0:00","",test$event_date),"%m/%d/%Y")
  min.day <- test %>%
    group_by(season) %>%
    summarise(min.day = min(gameDate))
  
  test <- test %>%
    left_join(min.day) %>%
    mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7) + 1))
  head(test)
  
  
  y <- asinTransform(test$p_home)
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
  
  
  #HFA index
  z <- apply(x, 1, function(x){ which(x == 1) })
  #z<-x
  #z[z==(-1)]<-0
  
  #HFA changes: create a new term for old arena's 
  ### Seattle to OKC
  if (league == "nba") {
    z[test$season < 2009 & test$home_team == "Oklahoma City Thunder"] <- max(z) + 1}
  
  ### Atlanta to Winnipeg
  if (league == "nhl") {
    z[test$season < 2012 & test$home_team == "Winnipeg Jets"] <- max(z) + 1}
  
  nHFAs <- max(z)
  
  
  #bugFile <- file.path("R/jags_model_TeamHFA.bug")
  #bugFile <- file.path("R/jags_model_constantHFA.bug")
  jags <- jags.model(bugFile,
                     data = list('y' = y,'x' = x, 's' = s, 'w' = w, 'n' = n, 
                                 'z' = z, 'nTeams' = nTeams, 
                                 'nWeeks' = nWeeks, 'nHFAs' = nHFAs, 'nSeas' = nSeas), 
                     n.chains = n.chains, n.adapt = n.adapt)
  
  update(jags, n.update)
  #dic.pD <- dic.samples(jags, 20, type = "pD") # Deviance Information Criterion
  #dic.popt <- dic.samples(jags, 100, type = "popt") # Penalized expected deviance
  z <- jags.samples(jags, posteriorDraws, n.draws, thin = thin)
  #z$dic <- dic.pD
  filename <- file.path(mcmc_dir, paste0(league, "_paper_", fit.type, "HFA.R2", ".RData"))
  save(z, file = filename, compress = "xz")
}


### Parallel computing
library(parallel)
library(doParallel)
library(foreach)

### Function inputs
num_adapt <- 100
num_update <- 1000
num_draws <- 2000

posteriorDraws = c('alpha','theta','tauGame','tauWeek',
                   'tauSeason','gammaWeek','gammaSeason', 'alphaInd', 'tauAlpha')
leaguestodo <- c("nhl", "nba", "nfl", "mlb")


### Run the models across leagues using parallel computing

## Home advantage specific model 
bugFile <- file.path("R", "jags_model_TeamHFA_R1.bug")
fit.type <- "team"

cl <- makeCluster(4)
registerDoParallel(cl)
ptm <- proc.time()

foreach(j = leaguestodo, .packages=c('dplyr', 'rjags')) %dopar% { 
  
  jagsModel(data = bigfour, league = j, bugFile = bugFile, 
            posteriorDraws = posteriorDraws, 
            n.adapt = num_adapt, n.update = num_update, 
            n.draws = num_draws, n.chains = 3, thin = 5, fit.type = fit.type)
}

proc.time() - ptm
stopCluster(cl)  





## Constant home advantage model 
bugFile <- file.path("R", "jags_model_constantHFA_R1.bug")
fit.type <- "constant"


cl <- makeCluster(4)
registerDoParallel(cl)
ptm <- proc.time()

foreach(j = leaguestodo, .packages=c('dplyr', 'rjags')) %dopar% { 
  
  jagsModel(data = bigfour, league = j, bugFile = bugFile, 
            posteriorDraws = posteriorDraws, 
            n.adapt = num_adapt, n.update = num_update, 
            n.draws = num_draws, n.chains = 3, thin = 5, fit.type = fit.type)
}

proc.time() - ptm
stopCluster(cl)  

