
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)
library(patchwork)

myprior <- prior(normal(0, 1), class = b)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_summary <- df_combined %>%
  filter(reoffer_unch == F, reoffer_ch == F) %>%
  group_by(subject) %>%
  summarise(
    lambda        = mean(lambda, na.rm = TRUE),
    beta          = mean(beta,   na.rm = TRUE),
    oil_signature = mean(stay_key[reward_oneback == 1], na.rm = TRUE) -
                    mean(stay_key[reward_oneback == 0], na.rm = TRUE),
    .groups = "drop"
  )

# Fit model ---------------------------------------------------------------

m_oil_signature <-
  brm(
    formula = oil_signature ~ lambda + beta,
    data    = df_summary,
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(m_oil_signature, "Exp1_2_combined/Output/regression/m_oil_signature.rds")

# Figure 7B-C -------------------------------------------------------------

c_eff <- conditional_effects(m_oil_signature)

p_7b <- plot(c_eff, plot = FALSE)[[1]] +
  xlim(0, 1) +
  labs(
    x = expression(lambda),
    y = expression(Delta * P(stay[location]) ~ (Rew - Unrew))
  ) +
  theme_bw()

p_7c <- plot(c_eff, plot = FALSE)[[2]] +
  labs(
    x = expression(beta),
    y = expression(Delta * P(stay[location]) ~ (Rew - Unrew))
  ) +
  theme_bw()

p_7bc <- p_7b + p_7c
ggsave("Exp1_2_combined/Output/regression/plots/figure7B_C.pdf", p_7bc, width = 8, height = 4)
