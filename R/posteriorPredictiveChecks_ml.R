source("config.R")
library(rjags)
load(file.path(data_raw, "bigfour.rda"))

### Here's the logit function
logit <- function(p) { 
  out <- log(p/(1 - p)) 
  return(out)
}


####################################################################################
##### Team-level home advantage
####################################################################################

asinTransform <- function(p) { asin(sqrt(p)) }

set.seed(1234)
yList <- postPred <- list()
nsim <- 20


for (league in c("nfl","mlb","nhl","nba")){print(league)
  postPred[[league]]<-list()
  load(paste0(mcmc_dir,"/", league,"_paper_teamHFA.R1.RData"))
  
  test <- subset(bigfour, sport == league)
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
  draw<-sample(1:n.draws, 1)
  chain<-sample(1:n.chains, 1)

    thetaMat<-z$theta[,,,draw,chain]
    alpha<-z$alpha[1,draw,chain]
    alphaInd<-z$alphaInd[,draw,chain]
    tauGame<-z$tauGame[1,draw,chain]
  
    
    mu<-rep(NA,nrow(x))
    
    for (i in 1:nrow(x)){
    mu[i] <- alpha + alphaInd[zzz[i]] + c(thetaMat[s[i],w[i],]%*%x[i,])
    }
    
    ## Note the code specification for rnorm()
    postPred[[league]][[sim]] <- rnorm(length(mu), mean = mu, sd = sqrt(1/tauGame))
  }
  
}
  
save(postPred, file = paste0(root, "/data/postPred.team.v2.RData"))
save(yList, file = paste0(root, "/data/yList.team.v2.RData"))


####################################################################################
##### Constant home advantage
####################################################################################


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
yList <- postPred.constant <- list()
nsim <- 20

for (league in c("nfl","mlb","nhl","nba")){print(league)
  postPred.constant[[league]]<-list()
  load(paste0(mcmc_dir,"/", league,"_paper_constantHFA.R1.RData"))
  
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
  
  
  ######################################################################
  #####    Sample From posterior distribution
  ######################################################################
  
  # Properties of chains  
  n.draws <- dim(z$theta)[4]
  n.chains <- dim(z$theta)[5]  
  
  #Create a vector of thetas for a season a and week
  for (sim in 1:nsim){print(sim)
    draw<-sample(1:n.draws, 1)
    chain<-sample(1:n.chains, 1)
    
    thetaMat<-z$theta[,,,draw,chain]
    alpha<-z$alpha[1,draw,chain]
    tauGame<-z$tauGame[1,draw,chain]
    
    
    mu<-rep(NA,nrow(x))
    
    for (i in 1:nrow(x)){
      mu[i]<-alpha + c(thetaMat[s[i],w[i],]%*%x[i,])
    }
    
    ## Note the code specification for rnorm()
    postPred.constant[[league]][[sim]] <- rnorm(length(mu), mean = mu, sd = sqrt(1/tauGame))
    
  }
  
}

save(postPred.constant, file = paste0(root, "/data/postPred.constant.v2.RData"))

######################################################################
source("config.R")
library(rjags)
library(ggjoy)
load(file.path(data_raw, "bigfour.rda"))
load(file.path(root, "data/postPred.constant.v2.RData"))
load(file.path(root, "data/postPred.team.v2.RData"))
postPred.team <- postPred
load(file.path(root, "data/yList.team.v2.RData"))


nsim <- 20        #define given above inputs
n.draws <- 2000   #define  given above inputs
n.chains <- 3     #define given above inputs
seasonList <- list()
hteamList <- list()
vteamList <- list()
dayList <- list()


for (leagues in c("nfl","mlb","nhl","nba")){
  test <- subset(bigfour, sport == leagues)
  seasonList[[leagues]] <- test$season
  hteamList[[leagues]] <- test$home_team
  vteamList[[leagues]] <- test$visitor_team
  dayList[[leagues]] <- weekdays(test$gameDate)
}


df.all <- NULL
for (leagues in c("nfl","mlb","nhl","nba")){
  y.obs <- yList[[leagues]]
  season.obs <- seasonList[[leagues]]
  hteam.obs <- hteamList[[leagues]]
  vteam.obs <- vteamList[[leagues]]
  day <- dayList[[leagues]]
  for (sim in 1:nsim){
    y.tilde.team <- postPred.team[[leagues]][[sim]]
    y.tilde.constant <- postPred.constant[[leagues]][[sim]]
    sim.number <- sim
    sport <- leagues
    df.current <- data.frame(sport, day, sim.number, y.obs, y.tilde.team, y.tilde.constant, season.obs, hteam.obs, vteam.obs, real = FALSE)
    df.all <- rbind(df.all, df.current)
  }
  df.real <- data.frame(sport, day, sim.number = sim.number + 1, y.obs, y.tilde.team = y.obs, y.tilde.constant = y.obs, season.obs, hteam.obs, vteam.obs, real = TRUE)
  df.all <- rbind(df.all, df.real)
}

library(ggjoy)

ggplot(filter(df.all, sim.number > 10), aes(x = y.tilde.team, y = factor(sim.number), fill = real)) + 
  geom_joy() + facet_wrap(~sport, scales = "free") + 
  ylab("simulation")


df.all %>% filter(!real) %>% ggplot(aes(y.tilde.team, group = sim.number)) + 
  geom_density(colour = "grey") + 
  geom_density(data = filter(df.all, real), aes(y.tilde.team), colour = "red") + 
  facet_wrap(~sport, scales = "free")

adjust <- 1/4
p1 <- df.all %>% filter(!real) %>% ggplot(aes(y.tilde.team, group = sim.number)) + 
  geom_density(colour = "grey", adjust = adjust) + 
  geom_density(data = filter(df.all, real), aes(y.tilde.team), colour = "red", adjust = adjust) + 
  xlab("logit(p)") + ylab("Density") + 
  facet_wrap(~toupper(sport), scales = "free")

ggsave(p1, "~/post.pred.pdf")


df.all %>% group_by(sport, y.obs) %>% count() %>% ungroup() %>% mutate(n = n/21) %>% group_by(sport) %>% arrange(-n) %>% slice(1:5)
bigfour %>% group_by(sport, p_home) %>% count() %>% ungroup() %>% group_by(sport) %>% arrange(-n) %>% slice(1:5)


df.all %>% filter(!real)  %>% ggplot(aes(y.tilde.team, group = sim.number)) + 
  geom_density(colour = "grey") + 
  geom_density(data = filter(df.all, real), aes(y.tilde.team), colour = "red") + 
  facet_wrap(~sport + season.obs, scales = "free", nrow = 4)

##### team HFA model
filter(df.all, sport == "mlb", !real) %>% ggplot(aes(y.tilde.team, group = sim.number)) + 
  geom_density(colour = "grey") + 
  geom_density(data = filter(df.all, sport == "mlb", real) , aes(y.tilde.team), colour = "red") + 
  facet_wrap(~hteam.obs, scales = "free", nrow = 4)

##### team comparison
team.analyze <- "Denver Nuggets"
data.analyze <- df.all %>% filter(hteam.obs == team.analyze)

filter(data.analyze, !real) %>% ggplot(aes(y.tilde.constant, group = sim.number)) + 
  geom_density(colour = "grey") + 
  geom_density(data = filter(data.analyze, real) , aes(y.tilde.constant), colour = "red")


filter(data.analyze, !real) %>% ggplot(aes(y.tilde.team, group = sim.number)) + 
  geom_density(colour = "grey") + 
  geom_density(data = filter(data.analyze, real) , aes(y.tilde.team), colour = "red")

ggplot(data.analyze, aes(y.tilde.team, y.tilde.constant)) + geom_point() + geom_smooth() +  geom_abline(aes(intercept = 0, slope = 1), col = "red")


df.all %>% mutate(diff.constant = y.tilde.constant - y.obs, 
                  diff.team = y.tilde.team - y.obs) %>% 
                  group_by(hteam.obs, sport) %>% 
                  summarise(ave.diff.constant = mean(diff.constant),  ave.diff.team = mean(diff.team)) %>% 
          gather("type", "Average_difference", ave.diff.constant:ave.diff.team) %>% 
          ggplot(aes(x = hteam.obs, y = Average_difference, colour = type)) + geom_point() + xlab("Team") + geom_hline(aes(yintercept = 0)) +  coord_flip() + 
         facet_wrap(~sport, scales = "free_y") + theme_gray(16) + ggtitle("Average difference between PPV of logit(p) versus observed logit(p), by home team")



df.all %>% mutate(diff.constant = y.tilde.constant - y.obs, 
                  diff.team = y.tilde.team - y.obs) %>% 
  group_by(vteam.obs, sport) %>% 
  summarise(ave.diff.constant = mean(-diff.constant),  ave.diff.team = mean(-diff.team)) %>% ### take the negative to get in terms of away team
  gather("type", "Average_difference", ave.diff.constant:ave.diff.team) %>% 
  ggplot(aes(x = vteam.obs, y = Average_difference, colour = type)) + geom_point() + xlab("Team") + geom_hline(aes(yintercept = 0)) +  coord_flip() + 
  facet_wrap(~sport, scales = "free_y") + theme_gray(16) + ggtitle("Average difference between PPV of logit(p) versus observed logit(p), by road team")

### Compare night football games

nfl.wday <- df.all %>% filter(sport == "nfl")  %>% mutate(night.game = (day == "Monday")|(day == "Thursday")) 
filter(nfl.wday, !real) %>% ggplot(aes(y.tilde.team, group = sim.number)) + 
  geom_density(colour = "grey")  + 
  geom_density(data = filter(nfl.wday, real) , aes(y.tilde.team), colour = "red")
  


### compare predicted residuals

df.all %>% filter(sim.number == 2) %>% mutate(resid = y.obs - y.tilde) %>% 
  ggplot(aes(y.obs, resid)) + geom_point() + geom_smooth() + facet_wrap(~sport)





