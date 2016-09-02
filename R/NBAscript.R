library(XML)
library(RCurl)


seasons<-list()
for (yyy in 1950:2015){print(yyy)
url<-paste("http://www.basketball-reference.com/leagues/NBA_",yyy,"_games.html?lid=header_seasons",sep="")
print(url)
temp <- readHTMLTable(url)
#Just regular season data
seasons[[as.character(yyy)]]<- rbind(cbind(season = yyy,  temp$games, playoffs = 0), cbind(season = yyy, temp$games_playoffs, playoffs = 1))
}

output <- do.call(rbind,seasons)
write.csv(output,"data/NBA_results.csv",row.names=FALSE)
