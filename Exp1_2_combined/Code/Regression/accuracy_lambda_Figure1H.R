
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

lambda_accuracy <- df_combined %>%
  group_by(subject, experiment) %>%
  summarise(
    average_lambda = mean(lambda,   na.rm = TRUE),
    accuracy       = mean(accuracy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    z_lambda   = scale(average_lambda),
    z_accuracy = scale(accuracy)
  )

# Fit model ---------------------------------------------------------------

m_lambda_accuracy <-
  brm(
    formula = z_accuracy ~ z_lambda,
    data    = lambda_accuracy,
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_lambda_accuracy, "Exp1_2_combined/Output/regression/m_lambda_accuracy.rds")

# Figure 1H ---------------------------------------------------------------

p_1h <- ggplot(lambda_accuracy,
               aes(x = average_lambda, y = accuracy)) +
  geom_point(aes(shape = experiment), alpha = 0.4, color = "black") +
  geom_smooth(method = "lm", color = "steelblue") +
  scale_shape_manual(
    values = c("Experiment 1" = 1, "Experiment 2" = 16),
    labels = c("Experiment 1" = "1", "Experiment 2" = "2")
  ) +
  xlim(0, 1) +
  ylim(0.4, 0.75) +
  labs(
    x     = expression("Individual-level " * lambda),
    y     = "Accuracy",
    shape = "Experiment"
  ) +
  theme_bw()
ggsave("Exp1_2_combined/Output/regression/plots/figure1H.pdf", p_1h, width = 5, height = 4)
