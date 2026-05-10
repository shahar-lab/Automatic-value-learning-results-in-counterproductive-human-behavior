
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df <- read.csv("Exp1/Data/Analysis/df/df.csv")

df <- df %>%
  arrange(subject, block, trial) %>%
  group_by(subject, block) %>%
  mutate(
    unch_card      = if_else(ch_card == card_left, card_right, card_left),
    reward_oneback = factor(lag(reward, 1)),
    reoffer_ch     = lag(ch_card, 1) == card_left | lag(ch_card, 1) == card_right,
    reoffer_unch   = lag(unch_card, 1) == card_left | lag(unch_card, 1) == card_right
  ) %>%
  ungroup()

df_filt <- df %>%
  mutate(
    ch_key      = factor(ch_key),
    prev_ch_key = lag(ch_key)
  ) %>%
  filter(reoffer_ch == FALSE, reoffer_unch == FALSE)

contrasts(df_filt$prev_ch_key)    <- c(-1, 1)
contrasts(df_filt$reward_oneback) <- c(-1, 1)

# Fit models --------------------------------------------------------------

m_prev_reduced <-
  brm(
    ch_key ~ prev_ch_key + (1 + prev_ch_key | subject),
    data    = df_filt,
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_prev_reduced, "Exp1/output/regression/m_prev_reduced.rds")

m_prev <-
  brm(
    ch_key ~ prev_ch_key * reward_oneback + (1 + prev_ch_key * reward_oneback | subject),
    data    = df_filt,
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_prev, "Exp1/output/regression/m_prev.rds")

# Figure 2G ---------------------------------------------------------------

df_full <- plot(conditional_effects(m_prev), plot = FALSE)[[3]]$data %>%
  mutate(
    reward_oneback = factor(reward_oneback,
                            levels = c(0, 1),
                            labels = c("Unrewarded", "Rewarded")),
    x_pos = ifelse(prev_ch_key == 0, 1, 1.35) +
      ifelse(reward_oneback == "Rewarded", 0.04, -0.04)
  )

df_reduced <- plot(conditional_effects(m_prev_reduced), plot = FALSE)[[1]]$data %>%
  mutate(x_pos = ifelse(prev_ch_key == 0, 1, 1.35))

p_2g <- ggplot() +
  geom_errorbar(
    data = df_full,
    aes(x = x_pos, ymin = lower__, ymax = upper__, color = reward_oneback),
    width = 0.04, linewidth = 0.7
  ) +
  geom_point(
    data = df_full,
    aes(x = x_pos, y = estimate__, color = reward_oneback),
    size = 3
  ) +
  geom_errorbar(
    data = df_reduced,
    aes(x = x_pos, ymin = lower__, ymax = upper__),
    width = 0.04, linewidth = 0.7, color = "darkgrey"
  ) +
  geom_point(
    data = df_reduced,
    aes(x = x_pos, y = estimate__),
    size = 3, shape = 21, fill = "darkgrey", color = "darkgrey"
  ) +
  scale_x_continuous(
    breaks = c(1, 1.35),
    labels = c("Left", "Right")
  ) +
  ylim(0.41, 0.575) +
  labs(
    x     = "Previous chosen location",
    y     = expression(P(choose ~ right ~ location)),
    color = "Previous outcome"
  ) +
  theme_bw()
ggsave("Exp1/output/regression/plots/figure2G.pdf", p_2g, width = 5, height = 4)
