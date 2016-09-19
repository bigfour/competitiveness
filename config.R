# local configuration hack

if (Sys.info()["user"] == "bbaumer") {
  # For Ben
  root <- "~/Dropbox/lib/competitiveness"
  mcmc_dir <- "~/dumps/Posterior_Draws"
} else {
  # For Greg and Mike
  root <- "~/Dropbox/competitiveness"
  mcmc_dir <- "~/Dropbox/Posterior_Draws"
}

library(knitr)
opts_chunk$set(tidy = FALSE, highlight = TRUE, comment = NA, echo = FALSE,
               prompt = FALSE, fig.width = 10, fig.height = 5, 
               message = FALSE, warning = FALSE)
