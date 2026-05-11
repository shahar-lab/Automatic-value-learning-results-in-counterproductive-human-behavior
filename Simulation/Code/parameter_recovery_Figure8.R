
rm(list = ls())
library(cmdstanr)
library(tidyverse)
library(ggplot2)
library(ggdist)
library(patchwork)

# Fit model ---------------------------------------------------------------

run_stan <- function(iter_sampling = 2,  # DEMO use 2000 for full fit
                     iter_warmup   = 2,  # DEMO use 2000 for full fit
                     chains          = 4,
                     parallel_chains = 4) {
  load("Simulation/Data/Parameter_recovery/simulated_standata.rdata")
  model_path  <- "Simulation/Code/Stan_models/full_model.stan"
  output_name <- "Simulation/Output/parameter_recovery/modelfit_parameter_recovery.rds"
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

dir.create("Simulation/Output/parameter_recovery/plots", recursive = TRUE, showWarnings = FALSE)

fit <- run_stan()

# True parameters ---------------------------------------------------------

load("Simulation/Data/Parameter_recovery/model_parameters.Rdata")

# artificial_individual_parameters is a 200x7 matrix; columns named in
# model_parameters$names; logit-transformed params listed in $transformation
param_names <- model_parameters$names
indiv_mat   <- model_parameters$artificial_individual_parameters
colnames(indiv_mat) <- param_names

true_params <- as.data.frame(indiv_mat) %>%
  mutate(
    alpha              = plogis(alpha),
    lambda             = plogis(lambda),
    decay_explore_card = plogis(decay_explore_card),
    decay_explore_key  = plogis(decay_explore_key)
  )

# Recovered individual-level parameters (posterior means of _sbj vectors) -

get_sbj_means <- function(var) {
  colMeans(fit$draws(variables = var, format = "draws_matrix"))
}

recovered_params <- tibble(
  subject            = seq_len(nrow(true_params)),
  alpha              = get_sbj_means("alpha_sbj"),
  beta               = get_sbj_means("beta_sbj"),
  lambda             = get_sbj_means("lambda_sbj"),
  explore_card       = get_sbj_means("explore_card_sbj"),
  explore_key        = get_sbj_means("explore_key_sbj"),
  decay_explore_card = get_sbj_means("decay_explore_card_sbj"),
  decay_explore_key  = get_sbj_means("decay_explore_key_sbj")
)

# Panel A — population-level posteriors -----------------------------------

get_pop_draws <- function(var, transform = identity) {
  transform(as.numeric(fit$draws(variables = var, format = "draws_matrix")))
}

pop_draws <- list(
  alpha              = get_pop_draws("mu_alpha",              plogis),
  beta               = get_pop_draws("mu_beta"),
  lambda             = get_pop_draws("mu_lambda",             plogis),
  explore_card       = get_pop_draws("mu_explore_card"),
  explore_key        = get_pop_draws("mu_explore_key"),
  decay_explore_card = get_pop_draws("mu_decay_explore_card", plogis),
  decay_explore_key  = get_pop_draws("mu_decay_explore_key",  plogis)
)

param_labels <- c(
  alpha              = "alpha",
  beta               = "beta",
  lambda             = "lambda",
  explore_card       = "rho[color]",
  explore_key        = "rho[location]",
  decay_explore_card = "eta[color]",
  decay_explore_key  = "eta[location]"
)

make_pop_plot <- function(draws, label) {
  df  <- data.frame(x = draws)
  med <- median(draws)
  ggplot(df, aes(x = x, y = 0)) +
    stat_slab(fill = "gray80") +
    stat_pointinterval(
      .width     = c(0.80, 0.90),
      point_size = 2,
      linewidth  = c(2, 1)
    ) +
    geom_point(aes(x = med, y = 0), color = "#4477AA", shape = 15, size = 2.5) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid   = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y  = element_blank(),
      axis.line.x  = element_line(colour = "grey30")
    ) +
    labs(x = parse(text = label)) +
    coord_cartesian(ylim = c(0, 1.3))
}

pop_plots <- mapply(make_pop_plot, pop_draws, param_labels, SIMPLIFY = FALSE)

panel_A <- wrap_plots(pop_plots, nrow = 2)

# Panel B — individual-level true vs recovered ----------------------------

make_scatter_plot <- function(true_vec, rec_vec, label) {
  r_val <- cor(true_vec, rec_vec, use = "complete.obs")
  df    <- data.frame(true = true_vec, recovered = rec_vec)
  ggplot(df, aes(x = true, y = recovered)) +
    geom_point(color = "#4477AA", alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", color = "#4477AA", se = TRUE, linewidth = 0.8) +
    annotate("text", x = -Inf, y = Inf,
             label = sprintf("r=%.2f", r_val),
             hjust = -0.2, vjust = 1.4, size = 3.5) +
    labs(x = parse(text = paste0("True\n", label)),
         y = "Recovered") +
    theme_bw(base_size = 11) +
    theme(panel.grid = element_blank())
}

scatter_plots <- mapply(
  make_scatter_plot,
  true_vec  = as.list(true_params[names(param_labels)]),
  rec_vec   = as.list(recovered_params[names(param_labels)]),
  label     = param_labels,
  SIMPLIFY  = FALSE
)

panel_B <- wrap_plots(scatter_plots, nrow = 2)

# Combined figure ---------------------------------------------------------

fig8 <- (panel_A / panel_B) +
  plot_annotation(tag_levels = "A")

ggsave("Simulation/Output/parameter_recovery/plots/figure8_parameter_recovery.pdf",
       fig8, width = 12, height = 8)

fig8
