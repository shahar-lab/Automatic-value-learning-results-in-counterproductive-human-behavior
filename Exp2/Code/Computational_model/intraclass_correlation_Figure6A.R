
rm(list = ls())
library(cmdstanr)
library(brms)
library(tidyverse)
library(bayestestR)
library(ggdist)
library(ggplot2)

fit_exp2 <- readRDS("Exp2/Output/computational_model/modelfit_empirical_exp2.rds")

# Intraclass correlation --------------------------------------------------

lambda1_sbj <- colMeans(fit_exp2$draws(variables = "lambda1_sbj", format = "draws_matrix"))
lambda2_sbj <- colMeans(fit_exp2$draws(variables = "lambda2_sbj", format = "draws_matrix"))
lambda3_sbj <- colMeans(fit_exp2$draws(variables = "lambda3_sbj", format = "draws_matrix"))

lambda_long <- data.frame(
  subject = rep(seq_along(lambda1_sbj), 3),
  session = rep(1:3, each = length(lambda1_sbj)),
  value   = c(lambda1_sbj, lambda2_sbj, lambda3_sbj)
)

fit_icc <-
  brm(
    value ~ 1 + (1 | subject),
    data    = lambda_long,
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(fit_icc, "Exp2/Output/computational_model/fit_icc.rds")

post <- as_draws_df(fit_icc) %>%
  mutate(
    var_subject  = sd_subject__Intercept^2,
    var_residual = sigma^2,
    ICC          = var_subject / (var_subject + var_residual)
  )

describe_posterior(post$ICC)

# Figure 6A ---------------------------------------------------------------

ci_6a_low  <- quantile(post$ICC, 0.025)
ci_6a_high <- quantile(post$ICC, 0.975)
p_6a <- ggplot(post, aes(x = ICC)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_6a_low, xend = ci_6a_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(post$ICC), y = 0, size = 2) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  labs(x = expression(lambda ~ "Intraclass Correlation Coefficient (ICC)")) +
  theme_classic() +
  theme(
    axis.title.y       = element_blank(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.line.y        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp2/Output/computational_model/plots/figure6A.pdf", p_6a, width = 6, height = 3)
