
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(bayestestR)
library(ggdist)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df <- read.csv("Exp3/Data/Filtered/RL.csv")

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
    family  = bernoulli(),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(fit_card, "Exp3/output/regression/fit_card.rds")

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

saveRDS(fit_key, "Exp3/output/regression/fit_key.rds")

posterior_theme <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y  = element_blank()
)

# Figure 4C — stay color: conditional effects -----------------------------

p_4c_cond <- plot(conditional_effects(fit_card), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay color)") +
  theme_bw()
ggsave("Exp3/output/regression/plots/figure4C_conditional.pdf",
       p_4c_cond, width = 4, height = 4)

# Figure 4C — stay color: posterior slope ---------------------------------

draws_4c <- as.numeric(as_draws_df(fit_card)$b_reward_oneback1)
ci_4c_low  <- quantile(draws_4c, 0.025)
ci_4c_high <- quantile(draws_4c, 0.975)
p_4c_post <- ggplot(data.frame(x = draws_4c), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_4c_low, xend = ci_4c_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_4c), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-1, 1) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp3/output/regression/plots/figure4C_posterior.pdf",
       p_4c_post, width = 5, height = 4)

# Figure 4D — stay location: conditional effects --------------------------

p_4d_cond <- plot(conditional_effects(fit_key), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("0" = "Unrewarded", "1" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(stay location)") +
  theme_bw()
ggsave("Exp3/output/regression/plots/figure4D_conditional.pdf",
       p_4d_cond, width = 4, height = 4)

# Figure 4D — stay location: posterior slope ------------------------------

draws_4d <- as.numeric(as_draws_df(fit_key)$b_reward_oneback1)
ci_4d_low  <- quantile(draws_4d, 0.025)
ci_4d_high <- quantile(draws_4d, 0.975)
p_4d_post <- ggplot(data.frame(x = draws_4d), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_4d_low, xend = ci_4d_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_4d), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-2, 2) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Exp3/output/regression/plots/figure4D_posterior.pdf",
       p_4d_post, width = 5, height = 4)
