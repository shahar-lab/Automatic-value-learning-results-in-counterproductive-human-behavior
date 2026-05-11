
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_summary <- df_combined %>%
  group_by(subject) %>%
  summarise(
    lambda        = mean(lambda, na.rm = TRUE),
    beta          = mean(beta,   na.rm = TRUE),
    oil_signature = mean(stay_key[reward_oneback == 1], na.rm = TRUE) -
                    mean(stay_key[reward_oneback == 0], na.rm = TRUE),
    .groups = "drop"
  )

df_high_beta <- df_summary %>% filter(beta > median(beta))

# Fit model ---------------------------------------------------------------

m_oil_high_beta <-
  brm(
    formula = oil_signature ~ lambda,
    data    = df_high_beta,
    warmup  = 2, # DEMO use 2000 for full fit
    iter    = 4, # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_oil_high_beta, "Exp1_2_combined/output/regression/m_oil_high_beta.rds")

# Figure S8 ---------------------------------------------------------------

c_eff <- conditional_effects(m_oil_high_beta)

p_s8 <- plot(c_eff, plot = FALSE)[[1]] +
  xlim(0, 1) +
  labs(
    x = expression("Individual-level " * lambda),
    y = expression(Delta * "P(stay location) (Rew - Unrew)")
  ) +
  theme_bw()

ggsave("Exp1_2_combined/output/regression/plots/figureS8_oil_high_beta.pdf",
       p_s8, width = 5, height = 4)
