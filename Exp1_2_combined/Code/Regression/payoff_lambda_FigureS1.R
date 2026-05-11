
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_subject <- df_combined %>%
  group_by(subject, experiment) %>%
  summarise(
    mean_reward    = mean(reward, na.rm = TRUE),
    lambda         = mean(lambda, na.rm = TRUE),
    .groups = "drop"
  )

# Fit model ---------------------------------------------------------------

m_payoff <-
  brm(
    formula = mean_reward ~ lambda * experiment,
    data    = df_subject,
    family  = gaussian(),
    warmup  = 2, # DEMO use 2000 for full fit
    iter    = 4, # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_payoff, "Exp1_2_combined/Output/regression/m_payoff_lambda.rds")

# Figure S1 ---------------------------------------------------------------

p_s1 <- ggplot(df_subject, aes(x = lambda, y = mean_reward, color = experiment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    values = c("Experiment 1" = "grey20", "Experiment 2" = "grey60")
  ) +
  xlim(0, 1) +
  labs(
    x     = expression(lambda),
    y     = "Mean payoff",
    color = "Experiment"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    text            = element_text(size = 12)
  )

ggsave("Exp1_2_combined/Output/regression/plots/figureS1_payoff_lambda.pdf",
       p_s1, width = 5, height = 4)
