library(ggplot2)
library(dplyr)
load("~/Dropbox/Posterior_Draws/nhl_8_23_constantHFA.RData")
nhlJAGS<-z

load("~/Dropbox/Posterior_Draws/mlb_8_23_constantHFA.RData")
mlbJAGS<-z

load("~/Dropbox/Posterior_Draws/nfl_8_23_constantHFA.RData")
nflJAGS<-z

load("~/Dropbox/Posterior_Draws/nba_8_23_constantHFA.RData")
nbaJAGS<-z
rm(z)


league <- nbaJAGS
df.all <- NULL
param.id <- c(1, 3:7)
len <- 4000
for (i in param.id){
  estimates <- c(league[[i]][,,1], league[[i]][,,2], league[[i]][,,3])
  if (i > 4){estimates <- 1/estimates}
  chain <- c(rep(1, len), rep(2, len), rep(3, len))
  iter <- c(1:len, 1:len, 1:len)
  df.est <- data.frame(estimates, iter, chain, param = names(league[i]))
  df.all <- rbind(df.all, df.est)
}


param_names <- list(
  'alpha' = expression(alpha),
  'gammaWeek' = expression(gamma[week]),
  'gammaSeason' = expression(gamma[seas]),
  'sigma' = expression(tau[game]),
  'sigmab' = expression(tau[week]),
  'sigmabSeason' = expression(tau[seas])
)

## Check the parameter names

param_labeller <- function(variable,value){
  return(param_names[value])
}

ggplot(df.all, aes(iter, estimates, colour = as.factor(chain))) + 
  geom_line() + facet_wrap(~param, labeller = param_labeller, scales = "free_y", ncol = 1, strip.position = "top") + 
  scale_colour_brewer(palette = "Set1", "Chain") +
  xlab("Chain index") + ylab("") +
  theme(strip.background = element_blank())



