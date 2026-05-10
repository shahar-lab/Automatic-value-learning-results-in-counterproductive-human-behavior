
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_subject <- df_combined %>%
  group_by(subject) %>%
  summarise(
    avg_capacity = mean(avg_capacity, na.rm = TRUE),
    lambda       = mean(lambda,       na.rm = TRUE),
    beta         = mean(beta,         na.rm = TRUE),
    mean_accuracy = mean(accuracy,    na.rm = TRUE),
    .groups = "drop"
  )

# Fit model ---------------------------------------------------------------

m_lambda_wm_controlled <-
  brm(
    formula = avg_capacity ~ lambda + beta + mean_accuracy,
    data    = df_subject,
    warmup  = 2000, #fast fit
    iter    = 4000, 
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(m_lambda_wm_controlled, "Exp1_2_combined/output/regression/m_lambda_wm_controlled.rds")

# Figure 7D ---------------------------------------------------------------

c_eff <- conditional_effects(m_lambda_wm_controlled, effects = "lambda")

p_7d <- plot(c_eff, plot = FALSE)[[1]] +
  labs(
    x = expression(lambda),
    y = "WM capacity "
  ) +
  theme_bw()
ggsave("Exp1_2_combined/output/regression/plots/figure7D.pdf", p_7d, width = 5, height = 4)
