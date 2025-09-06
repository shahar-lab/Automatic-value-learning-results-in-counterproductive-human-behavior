
#Install required packages if missing ----------------------------------
required_packages <- c("cmdstanr", "jsonlite", "tidyverse","bayestestR","brms")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

rm(list=ls())
library(cmdstanr)
library(jsonlite)
library(tidyverse)
library(bayestestR)
library(brms)

# Generalized function for Exp1 and Exp2 ---------------------------------

run_stan_for_experiment <- function(experiment = c("Exp1", "Exp2"),
                                    sample = NULL, # "abstract" or "story" for Exp1, NULL for Exp2
                                    model_type = c("full", "reduced"), # only for Exp1
                                    iter_sampling = 2, # DEMO use 2000 for full fit
                                    iter_warmup = 2,   # DEMO use 2000 for full fit
                                    chains = 4,
                                    parallel_chains = 4) {
  experiment <- match.arg(experiment)
  if (experiment == "Exp1") {
    sample <- match.arg(sample, c("abstract", "story"))
    model_type <- match.arg(model_type)
    data_path <- paste0("data/Exp1/ready_for_stan_model_fit/data_for_stan_", sample, ".json")
    model_path <- paste0("stan_models/Exp1/", model_type, "_model.stan")
    output_name <- paste0('output/modelfit_empirical_', sample, '_', model_type, '.rds')
  } else if (experiment == "Exp2") {
    data_path <- "data/Exp2/ready_for_stan_model_fit/data_for_stan.json"
    model_path <- "stan_models/Exp2/parameters_per_session.stan"
    output_name <- 'output/modelfit_empirical_exp2.rds'
  } else {
    stop("Unknown experiment")
  }
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

# Experiment 1 (both samples, full model)----------------------------------------------------
fit_story <- run_stan_for_experiment("Exp1", sample = "story", model_type = "full")
fit_abstract <- run_stan_for_experiment("Exp1", sample = "abstract", model_type = "full")

# Examine results
fit_story <- readRDS('output/modelfit_empirical_story.rds')
fit_abstract <- readRDS('output/modelfit_empirical_abstract.rds')

# population-level
lambda_story <- fit_story$draws(variables = 'mu_lambda', format = 'draws_matrix')
lambda_abstract <- fit_abstract$draws(variables = 'mu_lambda', format = 'draws_matrix')
diff_lambda <- as.vector(plogis(lambda_story) - plogis(lambda_abstract))
describe_posterior(diff_lambda)

# individual-level
lambda_sbj_story <- colMeans(fit_story$draws(variables = 'lambda_sbj', format = 'draws_matrix'))
lambda_sbj_abstract <- colMeans(fit_abstract$draws(variables = 'lambda_sbj', format = 'draws_matrix'))

# estimated differences in Q values
Qcard_diff <- colMeans(fit_abstract$draws(variables = 'Qcard_diff', format = 'draws_matrix'))
Qkey_diff <- colMeans(fit_abstract$draws(variables = 'Qkey_diff', format = 'draws_matrix'))

# Experiment 2 ------------------------------------------------------------

fit_exp2 <- run_stan_for_experiment("Exp2")

# Extract posterior draws and coerce to numeric vector
lambda1 <- as_draws_df(fit$draws("mu_lambda1"))$mu_lambda1
lambda2 <- as_draws_df(fit$draws("mu_lambda2"))$mu_lambda2
lambda3 <- as_draws_df(fit$draws("mu_lambda3"))$mu_lambda3

describe_posterior(plogis(lambda1))
describe_posterior(plogis(lambda2))
describe_posterior(plogis(lambda3))

#Intraclass correaltion
# Extract subject-level lambdas for each session
lambda1_sbj <- colMeans(fit_exp2$draws(variables = 'lambda1_sbj', format = 'draws_matrix'))
lambda2_sbj <- colMeans(fit_exp2$draws(variables = 'lambda2_sbj', format = 'draws_matrix'))
lambda3_sbj <- colMeans(fit_exp2$draws(variables = 'lambda3_sbj', format = 'draws_matrix'))

# Create long-format data.frame for ICC
lambda_long <- data.frame(
  subject = rep(seq_along(lambda1_sbj), 3),
  session = rep(1:3, each = length(lambda1_sbj)),
  value = c(lambda1_sbj, lambda2_sbj, lambda3_sbj)
)
#fit random intercepts model
fit_icc <- brm(
  value ~ 1 + (1 | subject),
  data = lambda_long,
  backend = "cmdstanr",
  warmup=2, # DEMO use 2000 for full fit
  iter =4, # DEMO use 4000 for full fit
  chains = 4,
  cores = 4)

#Extract variance components and compute ICC
post <- posterior_samples(fit_icc, pars = c("sd_subject__Intercept", "sigma"))

# Compute variances
post$var_subject <- post$sd_subject__Intercept^2
post$var_residual <- post$sigma^2

# Compute ICC per posterior draw
post$ICC <- post$var_subject / (post$var_subject + post$var_residual)
describe_posterior(post$ICC)
