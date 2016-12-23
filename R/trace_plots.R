library(ggplot2)
library(dplyr)
load("~/Dropbox/Posterior_Draws/nhl_9_23_teamHFA.RData")
nhlJAGS<-z

load("~/Dropbox/Posterior_Draws/mlb_9_23_teamHFA.RData")
mlbJAGS<-z

load("~/Dropbox/Posterior_Draws/nfl_9_23_teamHFA.RData")
nflJAGS<-z

load("~/Dropbox/Posterior_Draws/nba_9_23_teamHFA.RData")
nbaJAGS<-z
rm(z)

league <- nbaJAGS
len <- length(league$gammaSeason)/3
plot(1:len, league[[3]][3,10,15,,1], col = "red", type = "l")
points(1:len, league[[3]][3,10,15,,2], col = "green", type = "l")
points(1:len, league[[3]][3,10,15,,3], col = "blue", type = "l")
#### Labels for ggplot


param_names <- list(
  'alpha' = expression(alpha),
  'gammaSeason' = expression(gamma[season]),
  'gammaWeek' = expression(gamma[week]),
  'sigma' = expression(tau[game]),
  'sigmab' = expression(tau[week]),
  'sigmabSeason' = expression(tau[season])
)

## Check the parameter names

param_labeller <- function(variable,value){
  return(param_names[value])
}


### Function

func.trace <- function(league, league.name){
df.all <- NULL
param.id <- c(1, 4:6, 8, 9)
len <- 4000
for (i in param.id){
  estimates <- c(league[[i]][,,1], league[[i]][,,2], league[[i]][,,3])
  if (i > 4){estimates <- 1/estimates}
  chain <- c(rep(1, len), rep(2, len), rep(3, len))
  iter <- c(1:len, 1:len, 1:len)
  df.est <- data.frame(estimates, iter, chain, param = names(league[i]))
  df.all <- rbind(df.all, df.est)
}


p <- ggplot(df.all, aes(iter, estimates, colour = as.factor(chain))) + 
  geom_line() + facet_wrap(~param, labeller = param_labeller, scales = "free_y", ncol = 3) + 
  scale_colour_brewer(palette = "Set1", "Chain") +
  xlab("Chain index") + ylab("") + labs(title = league.name) + 
  theme(plot.title = element_text(hjust = 0.5))
p
}

NBA.trace <- func.trace(nbaJAGS, "NBA")
NHL.trace <- func.trace(nhlJAGS, "NHL")
MLB.trace <- func.trace(mlbJAGS, "MLB")
NFL.trace <- func.trace(nflJAGS, "NFL")

ggsave(plot = NBA.trace, width = 8, height = 6, filename = "figure/NBAtrace.pdf")
ggsave(plot = NHL.trace, width = 8, height = 6, filename = "figure/NHLtrace.pdf")
ggsave(plot = MLB.trace, width = 8, height = 6, filename = "figure/MLBtrace.pdf")
ggsave(plot = NFL.trace, width = 8, height = 6, filename = "figure/NFLtrace.pdf")

