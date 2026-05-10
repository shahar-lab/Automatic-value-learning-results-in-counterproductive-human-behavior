
rm(list = ls())
library(cmdstanr)
library(jsonlite)
library(tidyverse)
library(bayestestR)

# Fit model ---------------------------------------------------------------

run_stan <- function(sample = c("abstract", "story"),
                     model_type = c("full", "reduced"),
                     iter_sampling = 2, # DEMO use 2000 for full fit
                     iter_warmup = 2,   # DEMO use 2000 for full fit
                     chains = 4,
                     parallel_chains = 4) {
  sample     <- match.arg(sample)
  model_type <- match.arg(model_type)
  data_path   <- paste0("Exp1/Data/Analysis/standata/data_for_stan_", sample, ".json")
  model_path  <- paste0("Exp1/Code/Computational_model/Stan_models/", model_type, "_model.stan")
  output_name <- paste0("Exp1/output/computational_model/modelfit_empirical_", sample, "_", model_type, ".rds")
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

# Fit (both samples, full and reduced model) ------------------------------

fit_story_full        <- run_stan(sample = "story",    model_type = "full")
fit_abstract_full     <- run_stan(sample = "abstract", model_type = "full")
fit_story_reduced     <- run_stan(sample = "story",    model_type = "reduced")
fit_abstract_reduced  <- run_stan(sample = "abstract", model_type = "reduced")

