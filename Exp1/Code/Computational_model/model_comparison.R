
library(loo)

compare_models <- function(sample = c("story", "abstract"), output_dir = "Exp1/output/computational_model") {
  sample <- match.arg(sample)
  fit_full    <- readRDS(file.path(output_dir, paste0("modelfit_empirical_", sample, "_full.rds")))
  fit_reduced <- readRDS(file.path(output_dir, paste0("modelfit_empirical_", sample, "_reduced.rds")))

  log_lik_full    <- fit_full$draws("log_lik", format = "matrix")
  log_lik_reduced <- fit_reduced$draws("log_lik", format = "matrix")
  log_lik_full[is.na(log_lik_full)]       <- log(0.5) # first trial in block
  log_lik_reduced[is.na(log_lik_reduced)] <- log(0.5) # first trial in block

  loo_full    <- loo(log_lik_full)
  loo_reduced <- loo(log_lik_reduced)

  save(loo_full, loo_reduced, file = file.path(output_dir, paste0("loo_", sample, ".RData")))

  print(loo_compare(loo_reduced, loo_full))
}

compare_models("story")
compare_models("abstract")
