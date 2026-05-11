
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggdist)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df <- read.csv("Exp4B/Data/Filtered/RL.csv")

df <- df %>%
  mutate(
    reward_oneback = factor(reward_oneback),
    reward_oneback = relevel(reward_oneback, ref = "0")
  )

# Fit models --------------------------------------------------------------

fit_card <-
  brm(
    stay_card ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch_key == FALSE, reoffer_unch_key == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(fit_card, "Exp4B/Output/regression/fit_card.rds")

fit_key <-
  brm(
    stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch_key == TRUE),
    family  = bernoulli(),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(fit_key, "Exp4B/Output/regression/fit_key.rds")

posterior_theme <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y  = element_blank()
)

# Figure 5F - stay card ---------------------------------------------------

p_5f_cond <- plot(conditional_effects(fit_card), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay color)") +
  theme_bw()
ggsave("Exp4B/Output/regression/plots/figure5F_conditional.pdf",
       p_5f_cond, width = 4, height = 4)

draws_5f <- as.numeric(as_draws_df(fit_card)$b_reward_oneback1)
ci_5f_low  <- quantile(draws_5f, 0.025)
ci_5f_high <- quantile(draws_5f, 0.975)
p_5f_post <- ggplot(data.frame(x = draws_5f), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_5f_low, xend = ci_5f_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_5f), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-1, 1) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp4B/Output/regression/plots/figure5F_posterior.pdf",
       p_5f_post, width = 5, height = 4)

# Figure 5G - stay key ----------------------------------------------------

p_5g_cond <- plot(conditional_effects(fit_key), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay location)") +
  theme_bw()
ggsave("Exp4B/Output/regression/plots/figure5G_conditional.pdf",
       p_5g_cond, width = 4, height = 4)

draws_5g <- as.numeric(as_draws_df(fit_key)$b_reward_oneback1)
ci_5g_low  <- quantile(draws_5g, 0.025)
ci_5g_high <- quantile(draws_5g, 0.975)
p_5g_post <- ggplot(data.frame(x = draws_5g), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_5g_low, xend = ci_5g_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_5g), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-2, 2) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp4B/Output/regression/plots/figure5G_posterior.pdf",
       p_5g_post, width = 5, height = 4)
