
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(emmeans)
library(ggplot2)

df <- read.csv("Exp1/Data/Analysis/df/df.csv")

df <- df %>%
  arrange(subject, block, trial) %>%
  group_by(subject, block) %>%
  mutate(
    reward_pm1       = if_else(as.numeric(as.character(reward)) == 1, 1, -1), #treat unrewarded as -1 for the sum.
    # cumulative reward up to trial n-2
    cum_reward_left  = lag(lag(cumsum(if_else(ch_key == 0, reward_pm1, 0)), default = 0), default = 0),
    cum_reward_right = lag(lag(cumsum(if_else(ch_key == 1, reward_pm1, 0)), default = 0), default = 0)
  ) %>%
  ungroup() %>%
  mutate(cum_reward_diff = cum_reward_right - cum_reward_left)

# Fit model ---------------------------------------------------------------

m_cum_reward <-
  brm(
    formula = ch_key ~ cum_reward_diff * lambda,
    data    = df,
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_cum_reward, "Exp1/output/regression/m_cum_reward.rds")

# Figure 2F ---------------------------------------------------------------

emm <- emmeans(
  m_cum_reward,
  ~ cum_reward_diff | lambda,
  at = list(
    cum_reward_diff = seq(min(df$cum_reward_diff), max(df$cum_reward_diff), length.out = 40),
    lambda          = c(0.95, 0.80, 0.65)
  ),
  type = "response"
)

df_plot <- as.data.frame(emm)

p_2f <- ggplot(df_plot, aes(x = cum_reward_diff, y = response, color = factor(lambda))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD, fill = factor(lambda)),
              alpha = 0.15, color = NA) +
  labs(
    x     = "Cumulative reward difference (Right - Left)",
    y     = "P(choose right key)",
    color = "λ",
    fill  = "λ"
  ) +
  theme_bw()
ggsave("Exp1/output/regression/plots/figure2F.pdf", p_2f, width = 6, height = 4)

# Distribution of cum_reward_diff -----------------------------------------

p_2f_dist <- ggplot(df, aes(x = cum_reward_diff)) +
  geom_histogram(binwidth = 1, center = 0, fill = "grey60",
                 color = "white", alpha = 0.8) +
  labs(x = "Cumulative reward difference (Right - Left)", y = NULL) +
  theme_bw() +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp1/output/regression/plots/figure2F_distribution.pdf", p_2f_dist, width = 6, height = 3)
