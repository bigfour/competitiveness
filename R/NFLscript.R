library(XML)
yyy <- 2014
seasons <- list()
for (i in 1960:2014){print(i)
url <- paste("http://www.pro-football-reference.com/years/",i,"/games.htm",sep="")
seasons[[i]] <- cbind(season = i, readHTMLTable(url)$games)
}

output <- do.call(rbind,seasons)
output <- output[output$Date!="Date",]
write.csv(output,"~/Dropbox/competitiveness/Data/NFL_results.csv",row.names=FALSE)
