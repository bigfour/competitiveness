library(XML)
library(RCurl)
#url <- paste("http://www.baseball-reference.com/teams/")
#x <- htmlParse((url))
##pull out all the links
#y <- xpathApply(x, "//a/@href")
#teams <- do.call(rbind,y)
#teams
#pull out the team links
#teams <- teams[grep("teams",teams)]
#pull out only the teams
#teams <- teams[nchar(teams)==11]
#teams <- substring(teams,8,10)


require(Lahman)
keys <- unique(Teams[Teams$yearID >= 2000, c("yearID","teamID")])
require(dplyr)
keys <- keys %>%
  mutate(teamID = ifelse(yearID <= 2007 & teamID == "TBA", "TBD", as.character(teamID))) %>%
  mutate(teamID = ifelse(yearID >= 2008 & teamID == "TBA", "TBR", as.character(teamID))) %>%
  mutate(teamID = ifelse(teamID == "WAS", "WSN", teamID)) %>%
  mutate(teamID = ifelse(teamID == "KCA", "KCR", teamID))


seasons <- list()
for (i in 1:dim(keys)[1]){print(i)
url <- paste("http://www.baseball-reference.com/teams/",keys$teamID[i],"/",keys$yearID[i],"-schedule-scores.shtml",sep="")
temp <- try(readHTMLTable(url))
if (length(temp)>0){
  seasons[[paste(keys$teamID[i],keys$yearID[i],sep="-")]] <- temp$team_schedule
  seasons[[paste(keys$teamID[i],keys$yearID[i],sep="-")]]$year <- keys$yearID[i]
}
}


output <- do.call(rbind,seasons)
output$teamYear <- row.names(output)

#Clean it up
#remove rows with variable names
output <- output[!output$Tm=="Tm",]

write.csv(output, "data/MLB_results.csv", row.names=FALSE)



