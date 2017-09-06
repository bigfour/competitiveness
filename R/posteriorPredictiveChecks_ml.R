source("config.R")
library(rjags)
load(file.path(data_raw, "bigfour.rda"))

### Here's the logit function
logit <- function(p) { 
  out <- log(p/(1 - p))
  return(out)
}


asinTransform <- function(p) { asin(sqrt(p)) }

set.seed(1234)
yList <- postPred <- list()
nsim <- 20
for (league in c("nfl","mlb","nhl","nba")){print(league)
  postPred[[league]]<-list()
  load(paste0(mcmc_dir,"/", league,"_paper_teamHFA.R1.RData"))
  
  test <- subset(bigfour, sport == league)
  greg <- as.Date(gsub(" 0:00","",test$event_date),"%m/%d/%Y")
  min.day <- test %>%
    group_by(season) %>%
    summarise(min.day = min(gameDate))
  
  test <- test %>%
    left_join(min.day) %>%
    mutate(day = as.Date(gameDate) - as.Date(min.day), week = as.numeric(floor(day/7) + 1))
  head(test)
  
  
  yList[[league]] <- y <- logit(test$p_home)
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

# Properties of chains  
n.draws <- dim(z$theta)[4]
n.chains <- dim(z$theta)[5]  

#Create a vector of thetas for a season a and week
  for (sim in 1:nsim){print(sim)
  draw<-sample(1:n.draws,1)
  chain<-sample(1:n.chains,1)

    thetaMat<-z$theta[,,,draw,chain]
    alpha<-z$alpha[1,draw,chain]
      alphaInd<-z$alphaInd[,draw,chain]
    tauGame<-z$tauGame[1,draw,chain]
  
    
    mu<-rep(NA,nrow(x))
    
    for (i in 1:nrow(x)){
    mu[i]<-alpha + alphaInd[zzz[i]] + c(thetaMat[s[i],w[i],]%*%x[i,])
    }
    
    ## Note the code specification for rnorm()
    postPred[[league]][[sim]] <- rnorm(length(mu), mean = mu, sd = sqrt(1/tauGame))
    
  # df <- data.frame(mu = mu, pred = postPred[[league]][[sim]], y = y)
  # ggplot(df, aes(y, mu)) + geom_smooth() + geom_point()  + geom_abline(aes(intercept = 0, slope = 1), colour = "red") + 
  #   scale_y_continuous(lim = c(-3.5, 3.5)) + scale_x_continuous(lim = c(-3.5, 3.5))
  # ggplot(df, aes(y, pred)) + geom_smooth() + geom_point()  + geom_abline(aes(intercept = 0, slope = 1), colour = "red")+ 
  #   scale_y_continuous(lim = c(-3.5, 3.5)) + scale_x_continuous(lim = c(-3.5, 3.5))
  
  }
  
}
  
save(postPred, file = paste0(data_raw, "postPred.v2.RData"))
save(yList, file = paste0(data_raw, "yList.v2.RData"))




######################################################################
source("config.R")
library(rjags)
load(file.path(data_raw, "bigfour.rda"))
load(file.path(data_raw, "postPred.v2.RData"))
load(file.path(data_raw, "yList.v2.RData"))
nsim <- 20        #define given above inputs
n.draws <- 2000   #define  given above inputs
n.chains <- 3     #define given above inputs

df.all <- NULL

for (leagues in c("nfl","mlb","nhl","nba")){
  y.obs <- yList[[leagues]]
  for (sim in 1:nsim){
    y.tilde <- postPred[[leagues]][[sim]]
    sim.number <- sim
    sport <- leagues
    df.current <- data.frame(sport, sim.number, y.obs, y.tilde, real = FALSE)
    df.all <- rbind(df.all, df.current)
  }
  df.real <- data.frame(sport, sim.number = sim.number + 1, y.obs, y.tilde = y.obs, real = TRUE)
  df.all <- rbind(df.all, df.real)
}

library(ggjoy)

ggplot(filter(df.all, sim.number > 10), aes(x = y.tilde, y = factor(sim.number), fill = real)) + 
  geom_joy() + facet_wrap(~sport, scales = "free") + 
  ylab("simulation")


df.all %>% ggplot(aes(y.tilde, group = sim.number)) + 
  geom_density(colour = "grey") + 
  geom_density(data = filter(df.all, sim.number == 11), aes(y.tilde), colour = "red") + 
  facet_wrap(~sport, scales = "free")




### compare predicted residuals

df.all %>% filter(sim.number == 2) %>% mutate(resid = y.obs - y.tilde) %>% 
  ggplot(aes(y.obs, resid)) + geom_point() + geom_smooth() + facet_wrap(~sport)





