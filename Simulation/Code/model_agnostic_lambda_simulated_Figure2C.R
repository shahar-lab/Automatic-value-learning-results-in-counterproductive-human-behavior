
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load simulated data -----------------------------------------------------

load("Simulation/Data/Parameter_recovery/artificial_data.Rdata")

df <- df %>%
  arrange(subject, block, trial) %>%
  group_by(subject, block) %>%
  mutate(
    unch_card      = if_else(ch_card == card_left, card_right, card_left),
    reward_oneback = factor(lag(reward)),
    stay_key       = 1 * (ch_key == lag(ch_key)),
    reoffer_ch     = if_else(lag(ch_card) == card_left | lag(ch_card) == card_right, 1, 0),
    reoffer_unch   = if_else(lag(unch_card) == card_left | lag(unch_card) == card_right, 1, 0)
  ) %>%
  ungroup()

df_summary <- df %>%
  filter(reoffer_ch == 0, reoffer_unch == 0) %>%
  group_by(subject, reward_oneback) %>%
  summarise(
    prob_stay_key = mean(stay_key == 1, na.rm = TRUE),
    lambda        = first(lambda),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = reward_oneback, values_from = prob_stay_key,
              names_prefix = "reward_") %>%
  mutate(diff_prob = reward_1 - reward_0)

# Figure 2C ---------------------------------------------------------------

dir.create("Simulation/Output/parameter_recovery/plots", recursive = TRUE, showWarnings = FALSE)

p_2c <- ggplot(df_summary, aes(x = lambda, y = diff_prob)) +
  geom_point(alpha = 0.2, color = "black") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_x_continuous(limits = c(0, 1),     breaks = seq(0, 1,    0.25)) +
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, 0.4)) +
  labs(
    x = expression("Individual-level " * lambda),
    y = expression(Delta * "P(stay location) (Rew - Unrew)")
  ) +
  theme_bw()

ggsave("Simulation/Output/parameter_recovery/plots/figure2C_simulated.pdf",
       p_2c, width = 5, height = 4)

p_2c
