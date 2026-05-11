
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_subject <- df_combined %>%
  group_by(subject, experiment) %>%
  summarise(
    avg_capacity = mean(avg_capacity, na.rm = TRUE),
    lambda       = mean(lambda, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(avg_capacity),
    !is.na(lambda)
  ) %>%
  mutate(
    z_capacity = as.numeric(scale(avg_capacity)),
    z_lambda   = as.numeric(scale(lambda))
  )

# Fit model ---------------------------------------------------------------

m_lambda_wm <-
  brm(
    formula = z_lambda ~ z_capacity * experiment,
    data    = df_subject,
    warmup  = 2000, #fast fit
    iter    = 4000, 
    chains  = 4,
    cores   = 4,
    prior=myprior,
    backend = "cmdstanr"
  )

saveRDS(m_lambda_wm, "Exp1_2_combined/Output/regression/m_lambda_wm.rds")

# Figure 6C ---------------------------------------------------------------

p_6c <- ggplot(df_subject, aes(x = avg_capacity, y = lambda)) +
  geom_point(aes(shape = experiment), alpha = 0.2, color = "black") +
  geom_smooth(aes(group = 1), method = "lm") +
  scale_shape_manual(
    values = c("Experiment 1" = 1, "Experiment 2" = 16),
    labels = c("Experiment 1" = "1", "Experiment 2" = "2")
  ) +
  labs(
    x     = "WM capacity",
    y     = expression(lambda),
    shape = "Experiment"
  ) +
  theme_bw()
ggsave("Exp1_2_combined/Output/regression/plots/figure6C.pdf", p_6c, width = 5, height = 4)
