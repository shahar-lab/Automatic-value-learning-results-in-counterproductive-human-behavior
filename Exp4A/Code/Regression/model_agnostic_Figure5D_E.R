
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggdist)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df <- read.csv("Exp4A/Data/Filtered/RL.csv")

df <- df %>%
  mutate(
    reward_oneback = factor(reward_oneback),
    reward_oneback = relevel(reward_oneback, ref = "0")
  )

# Fit models --------------------------------------------------------------

fit_ch <-
  brm(
    stay_card ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch == TRUE),
    family  = bernoulli(),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(fit_ch, "Exp4A/output/regression/fit_ch.rds")

fit_key <-
  brm(
    stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE),
    family  = bernoulli(),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(fit_key, "Exp4A/output/regression/fit_key.rds")

posterior_theme <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y  = element_blank()
)

# Figure 5D — stay location -----------------------------------------------

p_5d_cond <- plot(conditional_effects(fit_key), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay location)") +
  theme_bw()
ggsave("Exp4A/output/regression/plots/figure5D_conditional.pdf",
       p_5d_cond, width = 4, height = 4)

p_5d_post <- ggplot(as_draws_df(fit_key), aes(x = b_reward_oneback1, y = 0)) +
  stat_halfeye(fill = "grey60", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-1, 1) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp4A/output/regression/plots/figure5D_posterior.pdf",
       p_5d_post, width = 5, height = 4)

# Figure 5E — stay card ---------------------------------------------------

p_5e_cond <- plot(conditional_effects(fit_ch), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay card)") +
  theme_bw()
ggsave("Exp4A/output/regression/plots/figure5E_conditional.pdf",
       p_5e_cond, width = 4, height = 4)

p_5e_post <- ggplot(as_draws_df(fit_ch), aes(x = b_reward_oneback1, y = 0)) +
  stat_halfeye(fill = "grey60", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-2, 2) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp4A/output/regression/plots/figure5E_posterior.pdf",
       p_5e_post, width = 5, height = 4)
