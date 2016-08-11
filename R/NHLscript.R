library(XML)
library(RCurl)

################################################
#Do people really still watch hockey?  
################################################

seasons<-list()
for (yyy in setdiff(1950:2014, 2005)){print(yyy)
                       
                       url <- paste("http://www.hockey-reference.com/leagues/NHL_",yyy,"_games.html",sep="")
                       temp <- readHTMLTable(url)
                       #Just regular season data
                       seasons[[as.character(yyy)]] <- rbind(cbind(season = yyy,  temp$games, playoffs = 0), cbind(season = yyy, temp$games_playoffs, playoffs = 1))
                      
}

output <- do.call(rbind,seasons)
write.csv(output,"~/Dropbox/competitiveness/Data/NHL_results.csv",row.names=FALSE)
