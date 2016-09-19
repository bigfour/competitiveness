library(XML)
library(RCurl)

### Note: we need updated scripts (Septemer 2016)


seasons<-list()
for (yyy in 2000:2015){print(yyy)
url<-paste("http://www.basketball-reference.com/leagues/NBA_",yyy,"_games.html?lid=header_seasons",sep="")
temp <- readHTMLTable(url)
#Just regular season data
seasons[[as.character(yyy)]]<- rbind(cbind(season = yyy,  temp$schedule, playoffs = 0), cbind(season = yyy, temp$games_playoffs, playoffs = 1))
}

output <- do.call(rbind,seasons)
write.csv(output,"data/NBA_results.csv",row.names=FALSE)
