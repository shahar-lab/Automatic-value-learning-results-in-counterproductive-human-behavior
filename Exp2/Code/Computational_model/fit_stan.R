
rm(list = ls())
library(cmdstanr)
library(jsonlite)
library(tidyverse)
library(bayestestR)
library(brms)

# Fit model ---------------------------------------------------------------

run_stan <- function(iter_sampling = 2, # DEMO use 2000 for full fit
                     iter_warmup = 2,   # DEMO use 2000 for full fit
                     chains = 4,
                     parallel_chains = 4) {
  data_path   <- "Exp2/Data/Analysis/standata/data_for_stan.json"
  model_path  <- "Exp2/Code/Computational_model/Stan_models/parameters_per_session.stan"
  output_name <- "Exp2/Output/computational_model/modelfit_empirical_exp2.rds"
  data_for_stan <- read_json(data_path, simplifyVector = TRUE)
  my_compiledmodel <- cmdstan_model(model_path)
  fit <- my_compiledmodel$sample(
    data            = data_for_stan,
    iter_sampling   = iter_sampling,
    iter_warmup     = iter_warmup,
    chains          = chains,
    parallel_chains = parallel_chains
  )
  fit$save_object(output_name)
  invisible(fit)
}

# Fit ---------------------------------------------------------------------

fit_exp2 <- run_stan()


