
##### Setup --------------------
# Install required packages if missing
required_packages <- c("loo")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load required libraries
library(loo)

# Function to compare models for Experiment 1 (story/abstract, full/reduced)
compare_models_exp1 <- function(sample = c("story", "abstract"), output_dir = "output") {
  sample <- match.arg(sample)
  # Use the naming convention from fit_stan.R
  fit_full    <- readRDS(file.path(output_dir, paste0("modelfit_empirical_", sample, "_full.rds")))
  fit_reduced <- readRDS(file.path(output_dir, paste0("modelfit_empirical_", sample, "_reduced.rds")))

  log_lik_full    <- fit_full$draws("log_lik", format = "matrix")
  log_lik_reduced <- fit_reduced$draws("log_lik", format = "matrix")
  log_lik_full[is.na(log_lik_full)] <- -0.693147 # log(0.5) to handle first trial in block, where values are reset.
  log_lik_reduced[is.na(log_lik_reduced)] <- -0.693147

  loo_full    <- loo(log_lik_full)
  loo_reduced <- loo(log_lik_reduced)

  # Save LOO results
  save(loo_full, loo_reduced, file = file.path(output_dir, paste0("loo_", sample, ".RData")))

  print(loo_compare(loo_reduced, loo_full))
}

# Run for both samples
compare_models_exp1("story")
compare_models_exp1("abstract")
