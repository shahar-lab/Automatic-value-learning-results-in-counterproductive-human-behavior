
rm(list = ls())
library(cmdstanr)
library(jsonlite)

# Fit model ---------------------------------------------------------------

run_stan <- function(iter_sampling = 2, # DEMO use 2000 for full fit
                     iter_warmup   = 2, # DEMO use 2000 for full fit
                     chains          = 4,
                     parallel_chains = 4) {
  data_path   <- "Exp4B/Data/Analysis/data_for_stan.json"
  model_path  <- "Exp4B/Code/Computational_model/Stan_models/full_model_visual.stan"
  output_name <- "Exp4B/output/computational_model/modelfit_empirical_exp4B.rds"
  data_for_stan    <- read_json(data_path, simplifyVector = TRUE)
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

fit_exp4B <- run_stan()
