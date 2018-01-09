source("config.R")
library(rjags)
load(file.path(data_raw, "bigfour.rda"))

### Here's the logit function
logit <- function(p) { 
  out <- log(p/(1 - p))
  return(out)
}


asinTransform <- function(p) { asin(sqrt(p)) }

sdList<-list()
set.seed(1234)
yList<-postPred<-list()
leagueAlpha<-"nba"
league<-"nfl"
  postPred[[league]]<-list()
  load(paste0("/Users/gregorymatthews/Dropbox/Posterior_Draws/",leagueAlpha,"_paper_teamHFA.RData"))
  zAlpha<-z
  rm(z)
  load(paste0("/Users/gregorymatthews/Dropbox/Posterior_Draws/",league,"_paper_teamHFA.RData"))
  
  
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
    alpha<-z$alpha[1,draw,chain]
    alphaInd<-z$alphaInd[,draw,chain]
    alphaInd<-rnorm(dim(thetaMat)[3],0,10)
    tauGame<-z$tauGame[1,draw,chain]
    
    
    mu<-rep(NA,nrow(x))
    
    for (i in 1:nrow(x)){
      mu[i]<-alpha + alphaInd[zzz[i]] + c(thetaMat[s[i],w[i],]%*%x[i,])
      
    }
  
    
    yList[[league]]<-y<-rnorm(length(mu),mu, sqrt(1/tauGame))




bugFile <- file.path("R/jags_model_TeamHFA.bug")
n.adapt = 100
n.update = 200
n.draws = 100
thin = 5
n.chains = 3
fit.type = "team"
posteriorDraws = c('alpha','theta','tauGame','tauWeek',
                   'tauSeason','gammaWeek','gammaSeason',"alphaInd")
                     

jags <- jags.model(bugFile,
                   data = list('y' = y,'x' = x, 's' = s, 'w' = w, 'n' = n, 
                               'z' = zzz, 'nTeams' = nTeams, 
                               'nWeeks' = nWeeks, 'nHFAs' = nHFAs, 'nSeas' = nSeas), 
                   n.chains = n.chains, n.adapt = n.adapt)

update(jags, n.update)
out <- jags.samples(jags, posteriorDraws, n.draws, thin = thin)


sdList[[f]]<-c(sd(apply(apply(out$theta,c(1,2,3),mean)[5,,],2,mean)),sd(apply(out$alphaInd,c(1),mean)),sd(alphaInd),sd(y))



#save(postPred,file="/Users/gregorymatthews/Dropbox/competitivenessGit/postPred.RData")
#save(yList,file="/Users/gregorymatthews/Dropbox/competitivenessGit/yList.RData")

