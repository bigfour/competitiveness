expit<-function(x,y){p<-exp(x-y)/(1+exp(x-y));p}
#Randomly sample a set of betas. 
#randomly simulate wins and losses.  
#Compute Noll Scully.  
#repeat. 


#[season, week, teams, draws, chain]
nsim<-500
games<-c(1:5)
nollScully<-list()
for (sport in c("mlb","nhl","nba","nfl")){
  load(paste0("/Users/gregorymatthews/Dropbox/Posterior_Draws/",sport,"_9_23_teamHFA.RData"))
  nollScully[[sport]]<-list()
for (season in 1:dim(z$beta)[1]){print(paste0("Season",season))
  nollScully[[sport]][[season]]<-list()
  for (week in 1:dim(z$beta)[2]){print(paste0("Week",week))
    nollScully[[sport]][[season]][[week]]<-matrix(NA,ncol=length(games)+1,nrow=nsim)
  temp<-z$beta[season,week,1:dim(z$beta)[3],1:dim(z$beta)[4],1]
  for (r in 1:nsim){print(r)
  
  beta<-apply(temp,1,function(x){sample(x,1)})
  beta<--sort(-beta)

mat <- matrix(NA,ncol=length(beta),nrow=length(beta))
for (i in 1:length(beta)){
  for (j in 1:length(beta)){
    mat[i,j]<-expit(beta[i],beta[j])    
  }
}


#"Observed" standard deviation
observedSD<-sd(apply(mat,1,sum)-0.5)
#Idealized standard deviation
idealizedSD<-mean(apply(mat,1,sum)-0.5)/sqrt(dim(mat)[1]-1)




outSim<-rep(NA,length(games))
for (nnn in 1:length(games)){
sim<-matrix(rbinom(dim(mat)[1]*dim(mat)[2],games[nnn],c(mat)),nrow=dim(mat)[1])
diag(sim)<-0
#"Observed" standard deviation
observedSDsim<-sd(apply(sim,1,sum))
#Idealized standard deviation
idealizedSDsim<-mean(apply(sim,1,sum))/sqrt(games[nnn]*(dim(sim)[1]-1))
outSim[nnn]<-observedSDsim/idealizedSDsim
}

nollScully[[sport]][[season]][[week]][r,]<-c(theory = observedSD/idealizedSD,sim = outSim)


}
  
  }
  save(nollScully,file="/Users/gregorymatthews/Dropbox/competitiveness/nollScullySimultions_20161123.RData")
}
}
load("/Users/gregorymatthews/Dropbox/competitiveness/nollScullySimultions_20161123.RData")


#Make the plots
png("/Users/gregorymatthews/Dropbox/competitiveness/NollScullyDistributions.png",res=300,units="in",w=20,h=10)
par(mfrow=c(2,2))
for (sport in c("mlb","nhl","nba","nfl")){
plot(0,0,type="l",xlim=c(-1,13),ylim=c(0,5),ylab="Noll - Scully",main = sport,xaxt='n',xlab="Year")
axis(1,c(1:11),c(2006:2016))
  abline(h=c(0:10),col=rgb(0.5,0.5,0.5,0.5))
for (season in 1:length(nollScully[[sport]])){
  for (week in 1:length(nollScully[[sport]][[season]])){
means<-apply(nollScully[[sport]][[season]][[week]],2,mean)
print(means)
#points((season)+week/28,means[1],col="red",pch=16,cex=0.5)
points((season)+week/length(nollScully[[sport]][[season]]),means[2],col="red",pch=16,cex=0.5)
#points((season)+week/28,means[3],col="green",pch=16,cex=0.5)
points((season)+week/length(nollScully[[sport]][[season]]),means[4],col="green",pch=16,cex=0.5)
#points((season)+week/28,means[5],col="black",pch=16,cex=0.5)
points((season)+week/length(nollScully[[sport]][[season]]),means[6],col="blue",pch=16,cex=0.5)
means<-apply(nollScully[[sport]][[season]][[week]],2,quantile,c(0.025,0.975))
#points(rep((season)+week/28,2),means[,1],col="red",type="l")
points(rep((season)+week/length(nollScully[[sport]][[season]]),2),means[,2],col=rgb(1,0,0,0.5),type="l")
#points(rep((season)+week/28,2),means[,3],col="green",type="l")
points(rep((season)+week/length(nollScully[[sport]][[season]]),2),means[,4],col=rgb(0,1,0,0.5),type="l")
#points(rep((season)+week/28,2),means[,5],col="black",type="l")
points(rep((season)+week/length(nollScully[[sport]][[season]]),2),means[,6],col=rgb(0,0,1,0.5),type="l")

  }
}
legend(-1,4,c("1 game", "3 games", "5 games"),c("red","green","blue"))

}

dev.off()

