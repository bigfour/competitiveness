# local configuration hack

if (Sys.info()["user"] == "bbaumer") {
  # For Ben
  root <- "~/Dropbox/git/competitiveness"
  mcmc_dir <- "~/dumps/Posterior_Draws"
}

if (Sys.info()["user"] == "mlopez1") {
  # For Ben
  root <- "~/Dropbox/Compete"
  mcmc_dir <- "~/Dropbox/Posterior_Draws"
}

if (Sys.info()["user"] == "gregorymatthews") {
  # For Greg and Mike
  root <- "~/Dropbox/competitivenessGit"
  mcmc_dir <- "~/Dropbox/Posterior_Draws"
}

library(knitr)
opts_chunk$set(tidy = FALSE, highlight = TRUE, comment = NA, echo = FALSE,
               prompt = FALSE, fig.width = 10, fig.height = 5, 
               message = FALSE, warning = FALSE)
library(tidyverse)
