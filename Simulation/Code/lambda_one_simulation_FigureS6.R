
rm(list = ls())
library(cmdstanr)
library(tidyverse)
library(ggplot2)
library(ggdist)

# Fit model ---------------------------------------------------------------

run_stan <- function(iter_sampling = 2,  # DEMO use 2000 for full fit
                     iter_warmup   = 2,  # DEMO use 2000 for full fit
                     chains          = 4,
                     parallel_chains = 4) {
  load("Simulation/Data/Lambda_1_recovery/simulated_standata.rdata")
  model_path  <- "Simulation/Code/Stan_models/full_model.stan"
  output_name <- "Simulation/Output/lambda_1_recovery/modelfit_lambda_1_recovery.rds"
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

dir.create("Simulation/Output/lambda_1_recovery/plots", recursive = TRUE, showWarnings = FALSE)

fit <- run_stan()

# Figure S6 — posterior lambda (lambda = 1 recovery) ----------------------

draws <- as.numeric(fit$draws(variables = "mu_lambda", format = "draws_matrix"))
draws <- plogis(draws)

xlim_posterior <- function(x, pad = 0.20) {
  r <- range(x); span <- diff(r)
  c(r[1] - pad * span, r[2] + pad * span)
}

med_val <- median(draws)
pd_val  <- mean(draws > 0.5) * 100

df_plot <- data.frame(lambda = draws)

p_s6 <- ggplot(df_plot, aes(x = lambda, y = 0)) +
  stat_slab(fill = "gray80") +
  stat_pointinterval(
    .width     = c(0.80, 0.90),
    point_size = 3,
    linewidth  = c(2, 1)
  ) +
  geom_vline(xintercept = med_val, linetype = "dashed", colour = "grey65", linewidth = 0.4) +
  annotate(
    "text", x = med_val, y = Inf,
    label  = sprintf("[median = %.2f, pd = %.2f%%]", med_val, pd_val),
    hjust  = -0.05, vjust = 1.4, size = 3.2, colour = "grey40"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(xlim = xlim_posterior(draws), ylim = c(0, 1.3), clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid   = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    axis.line.x  = element_line(colour = "grey30")
  ) +
  labs(x = expression(lambda))

ggsave("Simulation/Output/lambda_1_recovery/plots/figureS6_lambda_posterior.pdf",
       p_s6, width = 6, height = 3)

p_s6
