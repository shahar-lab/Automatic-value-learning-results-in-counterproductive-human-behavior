#The panel letters in Figure 5 were mixed.
#the model-agnostic results should be E-F for Exp4A and G-H for Exp4B. 
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

saveRDS(fit_ch, "Exp4A/Output/regression/fit_ch.rds")

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

saveRDS(fit_key, "Exp4A/Output/regression/fit_key.rds")

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
ggsave("Exp4A/Output/regression/plots/figure5D_conditional.pdf",
       p_5d_cond, width = 4, height = 4)

draws_5d <- as.numeric(as_draws_df(fit_key)$b_reward_oneback1)
ci_5d_low  <- quantile(draws_5d, 0.025)
ci_5d_high <- quantile(draws_5d, 0.975)
p_5d_post <- ggplot(data.frame(x = draws_5d), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_5d_low, xend = ci_5d_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_5d), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-1, 1) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp4A/Output/regression/plots/figure5D_posterior.pdf",
       p_5d_post, width = 5, height = 4)

# Figure 5E — stay card ---------------------------------------------------

p_5e_cond <- plot(conditional_effects(fit_ch), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay card)") +
  theme_bw()
ggsave("Exp4A/Output/regression/plots/figure5E_conditional.pdf",
       p_5e_cond, width = 4, height = 4)

draws_5e <- as.numeric(as_draws_df(fit_ch)$b_reward_oneback1)
ci_5e_low  <- quantile(draws_5e, 0.025)
ci_5e_high <- quantile(draws_5e, 0.975)
p_5e_post <- ggplot(data.frame(x = draws_5e), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_5e_low, xend = ci_5e_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_5e), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-2, 2) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp4A/Output/regression/plots/figure5E_posterior.pdf",
       p_5e_post, width = 5, height = 4)
