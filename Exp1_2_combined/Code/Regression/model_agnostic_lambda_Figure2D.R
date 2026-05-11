
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_summary <- df_combined %>%
  filter(!reoffer_ch & !reoffer_unch) %>%
  group_by(subject, reward_oneback) %>%
  summarise(
    prob_stay_key = mean(stay_key == 1, na.rm = TRUE),
    lambda        = first(lambda),
    experiment    = first(experiment),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = reward_oneback, values_from = prob_stay_key,
              names_prefix = "reward_") %>%
  mutate(diff_prob = reward_1 - reward_0) %>%
  group_by(experiment) %>%
  mutate(
    lambda_z    = scale(lambda)[, 1],
    diff_prob_z = scale(diff_prob)[, 1]
  ) %>%
  ungroup()

# Fit model ---------------------------------------------------------------

m_lambda_diff <-
  brm(
    formula = diff_prob_z ~ lambda_z,
    data    = df_summary %>% filter(experiment == "Experiment 1"),
    family  = gaussian(),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_lambda_diff, "Exp1_2_combined/Output/regression/m_lambda_stay_diff.rds")

# Figure 2D ---------------------------------------------------------------

p_2d <- ggplot(df_summary, aes(x = lambda, y = diff_prob)) +
  geom_point(aes(shape = experiment), alpha = 0.2, color = "black") +
  geom_smooth(aes(group = 1), method = "lm", color = "blue") +
  scale_shape_manual(
    values = c("Experiment 1" = 1, "Experiment 2" = 16),
    labels = c("Experiment 1" = "1", "Experiment 2" = "2")
  ) +
  scale_x_continuous(limits = c(0, 1),    breaks = seq(0, 1,    0.25)) +
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, 0.4)) +
  labs(
    x     = expression("Individual-level " * lambda),
    y     = "P(stay | rewarded) - P(stay | unrewarded)",
    shape = "Experiment"
  ) +
  theme_bw()
ggsave("Exp1_2_combined/Output/regression/plots/figure2D.pdf", p_2d, width = 5, height = 4)
