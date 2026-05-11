rm(list = ls())
library(cmdstanr)
library(bayesplot)
library(tidyverse)

# Load fits ---------------------------------------------------------------

fit_abstract_full <- readRDS("Exp1/Output/computational_model/modelfit_empirical_abstract_full.rds")

# Group-level parameters to plot ------------------------------------------

mu_pars <- c(
  "mu_alpha", "mu_beta", "mu_lambda",
  "mu_explore_card", "mu_explore_key",
  "mu_decay_explore_card", "mu_decay_explore_key"
)

# Figure S9 ---------------------------------------------------------------

p_abstract <- mcmc_pairs(
  fit_abstract_full$draws(variables = mu_pars),
  diag_fun  = "dens",
  off_diag_fun = "scatter",
  off_diag_args = list(size = 0.3, alpha = 0.3)
)

ggsave("Exp1/Output/computational_model/plots/figureS9.pdf",
       p_abstract, width = 10, height = 10)

p_abstract
