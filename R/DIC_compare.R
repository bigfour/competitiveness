sports <- c("mlb", "nba", "nfl", "nhl")
library(rjags)
source("config.R")
get_sport <- function(sport) {
  message(paste("reading", sport, "data..."))
  load(file.path(mcmc_dir, paste0(sport, "_9_23_constantHFA.RData")))
  z.null <- z
  load(file.path(mcmc_dir, paste0(sport, "_9_23_teamHFA.RData")))
  z.alt <- z
  diff.dic <- diffdic(z.alt$dic, z.null$dic)
  print(diff.dic)
  print(sport)
  return(list(diff.dic = diff.dic, 
              constant.dic = z.null$dic, 
              team.dic = z.alt$dic, 
              sport = sport))
}

#Your code is so much better written than mine that I feel physical pain.  
dat <- lapply(sports[1:4], get_sport) 
dat
