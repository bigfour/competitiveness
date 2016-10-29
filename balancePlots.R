load("~/Dropbox/competitivenessGit/data/tidy_betas.final.rda")
expit <- function(beta1,beta2){exp(beta1-beta2)/(1+exp(beta1-beta2))}

balance <- list()

sport<-"nba"
for (sport in c("mlb","nba","nhl","nfl")){
  
dat <- tidy_betas[tidy_betas$sport==sport,]


balance[[sport]]<-data.frame(cumweek = NA, season=NA,balance=NA,balance2 = NA)

for (t in 1:max(dat$cumweek)){print(t)
temp <- subset(dat, cumweek == t)
beta <- sort(temp$beta)
  
mat <- matrix(NA,ncol=length(beta),nrow=length(beta))
for (i in 1:length(beta)){
  for (j in 1:length(beta)){
mat[i,j]<-expit(beta[i],beta[j])    
  }
}

#Remove the game against themselves.
balance2 <- mean(abs((mat%*%rep(1,nrow(mat))-0.5)/(nrow(mat)-1)-0.5))

balance[[sport]][t,]<-data.frame(cumweek = temp$cumweek[1], season=temp$season[1], balance=4*var(Re(eigen(mat)$values))/length(beta), balance2 = balance2)
}

}

png("~/Dropbox/competitivenessGit/figures/balancePlot2.png")
par(mfrow=c(2,2))
for (sport in c("mlb","nba","nhl","nfl")){
plot(balance[[sport]]$cumweek,balance[[sport]]$balance,type="l",main=sport,xlab="cumulative week",ylab="balance")
  maxx <- max(balance[[sport]]$cumweek)/max(balance[[sport]]$season)
  for (g in seq(0,10,2)){
  polygon(c(0,maxx,maxx,0)+maxx*g,c(0,0,1,1),col=rgb(1,0,0,0.5),bor=rgb(1,0,0,0.5))
  }
  a<-smooth.spline(balance[[sport]]$cumweek,balance[[sport]]$balance,df=20)
  points(balance[[sport]]$cumweek,predict(a,balance[[sport]]$cumweek)$y,type="l",col="red",lwd=3)
}
dev.off()


png("~/Dropbox/competitivenessGit/figures/balance2Plot2.png")
par(mfrow=c(2,2))
for (sport in c("mlb","nba","nhl","nfl")){
  plot(balance[[sport]]$cumweek,balance[[sport]]$balance2,type="l",main=sport,ylim=c(0,0.16),xlab="cumulative week",ylab="Mean absolute error in predicted winning\n percentage with a balanced schedule")
  maxx <- max(balance[[sport]]$cumweek)/max(balance[[sport]]$season)
  for (g in seq(0,10,2)){
    polygon(c(0,maxx,maxx,0)+maxx*g,c(0,0,1,1),col=rgb(1,0,0,0.5),bor=rgb(1,0,0,0.5))
  }
  a<-smooth.spline(balance[[sport]]$cumweek,balance[[sport]]$balance2,df=20)
  points(balance[[sport]]$cumweek,predict(a,balance[[sport]]$cumweek)$y,type="l",col="red",lwd=3)
}
dev.off()