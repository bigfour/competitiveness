expit<-function(x,y){p<-exp(x-y)/(1+exp(x-y));p}
#Randomly sample a set of betas. 
#randomly simulate wins and losses.  
#Compute Noll Scully.  
#repeat. 


#[season, week, teams, draws, chain]
nsim<-100
betavar<-list()
for (sport in c("mlb","nhl","nba","nfl")){
  load(paste0("/Users/gregorymatthews/Dropbox/Posterior_Draws/",sport,"_9_23_teamHFA.RData"))
  betavar[[sport]]<-list()
  for (season in 1:dim(z$beta)[1]){print(paste0("Season",season))
    betavar[[sport]][[season]]<-list()
    for (week in 1:dim(z$beta)[2]){print(paste0("Week",week))
      betavar[[sport]][[season]][[week]]<-matrix(NA,ncol=1,nrow=nsim)
      temp<-z$beta[season,week,1:dim(z$beta)[3],1:dim(z$beta)[4],1]
      for (r in 1:nsim){print(r)
        beta<-apply(temp,1,function(x){sample(x,1)})
        betavar[[sport]][[season]][[week]][r,]<-sd(beta)
        
      }
      
    }
    
  }
}

save(betavar,file="/Users/gregorymatthews/Dropbox/competitiveness/betavarSimultions_20161203.RData")
load("/Users/gregorymatthews/Dropbox/competitiveness/betavarSimultions_20161203.RData")


#Make the plots
#png("/Users/gregorymatthews/Dropbox/competitiveness/betavarDistributions.png",res=300,units="in",w=20,h=10)
par(mfrow=c(2,2))
for (sport in c("mlb","nhl","nba","nfl")){
  plot(0,0,type="l",xlim=c(-1,13),ylim=c(0,1),ylab="Noll - Scully",main = sport,xaxt='n',xlab="Year")
  axis(1,c(1:11),c(2006:2016))
  abline(h=c(0:10),col=rgb(0.5,0.5,0.5,0.5))
  for (season in 1:length(betavar[[sport]])){
    for (week in 1:length(betavar[[sport]][[season]])){
      means<-apply(betavar[[sport]][[season]][[week]],2,mean)
      print(means)
      #points((season)+week/28,means[1],col="red",pch=16,cex=0.5)
      points((season)+week/length(betavar[[sport]][[season]]),means,col="red",pch=16,cex=0.5)
    
      means<-apply(betavar[[sport]][[season]][[week]],2,quantile,c(0.025,0.975))
      points(rep((season)+week/length(betavar[[sport]][[season]]),2),as.vector(means),col=rgb(1,0,0,0.5),type="l")
      
    }
  }
  legend(-1,4,c("1 game", "3 games", "5 games"),c("red","green","blue"))
  
}

dev.off()



