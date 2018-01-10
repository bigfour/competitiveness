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

alphaList<-sdList<-list()
set.seed(1234)
yList<-postPred<-list()
#leagueAlpha<-"mlb"
for (leagueAlpha in c("mlb","nhl","nba","nfl")){
league<-"nfl"
  postPred[[league]]<-list()
  load(paste0("/Users/gregorymatthews/Dropbox/Posterior_Draws/",leagueAlpha,"_paper_constantHFA.R1.RData"))
  zAlpha<-z
  rm(z)
  load(paste0("/Users/gregorymatthews/Dropbox/Posterior_Draws/",league,"_paper_constantHFA.R1.RData"))
  
  
  test <- subset(bigfour, sport == league)
  greg <- as.Date(gsub(" 0:00","",test$event_date),"%m/%d/%Y")
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
  
    draw<-sample(1:400,1)
    chain<-sample(1:3,1)
    
    thetaMat<-z$theta[,,,draw,chain]
    alpha<-zAlpha$alpha[1,draw,chain]
    # alphaInd<-z$alphaInd[,draw,chain]
    # alphaInd<-rnorm(dim(thetaMat)[3],0,10)
    tauGame<-z$tauGame[1,draw,chain]
    
    
    mu<-rep(NA,nrow(x))
    
    for (i in 1:nrow(x)){
      mu[i]<- alpha  + c(thetaMat[s[i],w[i],]%*%x[i,])
      
    }
  
    alphaList[[paste(leagueAlpha)]]<-alpha
    yList[[paste(league,leagueAlpha)]]<-y<-rnorm(length(mu), mu, sqrt(1/tauGame))




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

temp<-list()
for (i in 1:dim(out$theta)[1]){
temp[[i]]<-apply(out$theta,c(1,2,3),mean)[i,,]
}

sdList[[paste(league,leagueAlpha)]]<-apply(do.call(rbind,temp),1,sd)
}

lapply(yList,sd)

plot(sdList[[1]],type="l")
points(sdList[[2]],type="l",col="red")
points(sdList[[3]],type="l",col="blue")
points(sdList[[4]],type="l",col="green")

tosave<-list(sdList,yList,alphaList)
save(tosave,file="~/Dropbox/competitivenessGit/simulationForReviewers.RData")

#save(postPred,file="/Users/gregorymatthews/Dropbox/competitivenessGit/postPred.RData")
#save(yList,file="/Users/gregorymatthews/Dropbox/competitivenessGit/yList.RData")

