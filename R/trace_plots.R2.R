library(ggplot2)
library(dplyr)
load("~/Dropbox/Posterior_Draws/nhl_paper_teamHFA.R1.RData")
nhlJAGS<-z

load("~/Dropbox/Posterior_Draws/mlb_paper_teamHFA.R1.RData")
mlbJAGS<-z

load("~/Dropbox/Posterior_Draws/nfl_paper_teamHFA.R1.RData")
nflJAGS<-z

load("~/Dropbox/Posterior_Draws/nba_paper_teamHFA.R1.RData")
nbaJAGS<-z
rm(z)


#### Labels for ggplot
param_names <- list(
  'alpha' = expression(alpha[q[o]]),
  'gammaSeason' = expression(gamma[season]),
  'gammaWeek' = expression(gamma[week]),
  'sigma' = expression(sigma[game]),
  'sigmab' = expression(sigma[week]),
  'sigmabSeason' = expression(sigma[season])
)

## Check the parameter names

param_labeller <- function(variable,value){
  return(param_names[value])
}

## change the order of week and season for sigmas
### Function

func.trace <- function(league, league.name){
df.all <- NULL
param.id <- c(1, 3, 4, 6:8)
len <- 4000
for (i in param.id){
  chain <- c(rep(1, len), rep(2, len), rep(3, len))
  iter <- c(1:len, 1:len, 1:len)
  estimates <- c(league[[i]][,,1], league[[i]][,,2], league[[i]][,,3])
  #chain <- c(rep(1, len))
  #iter <- c(1:len)
  #estimates <- c(league[[i]][,,1])
  if (i > 5){estimates <- sqrt(1/estimates)}
  df.est <- data.frame(estimates, iter, chain, param = names(league[i]))
  df.all <- rbind(df.all, df.est)
}


p <- ggplot(df.all, aes(iter, estimates, colour = as.factor(chain),
                        alpha = as.factor(chain))) + 
  geom_line() + 
  facet_wrap(~param, labeller = param_labeller, scales = "free_y", ncol = 3) + 
  scale_colour_brewer(palette = "Set1", "Chain") +
  scale_alpha_manual(values = c(1, .6, .3)) + 
  xlab("Chain index") + ylab("") + labs(title = league.name) + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(alpha=FALSE)

p
}

NBA.trace <- func.trace(nbaJAGS, "NBA")
NHL.trace <- func.trace(nhlJAGS, "NHL")
MLB.trace <- func.trace(mlbJAGS, "MLB")
NFL.trace <- func.trace(nflJAGS, "NFL")

ggsave(plot = NBA.trace, width = 8, height = 6, filename = "~/Dropbox/Compete/figure/NBAtrace_R1.pdf")
ggsave(plot = NHL.trace, width = 8, height = 6, filename = "~/Dropbox/Compete/figure/NHLtrace_R1.pdf")
ggsave(plot = MLB.trace, width = 8, height = 6, filename = "~/Dropbox/Compete/figure/MLBtrace_R1.pdf")
ggsave(plot = NFL.trace, width = 8, height = 6, filename = "~/Dropbox/Compete/figure/NFLtrace_R1.pdf")

