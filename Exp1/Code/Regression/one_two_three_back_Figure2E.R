
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(bayestestR)
library(ggplot2)

df <- read.csv("Exp1/Data/Analysis/df/df.csv")

df$ch_key <- factor(df$ch_key)

df <- df %>%
  arrange(subject, block, trial) %>%
  group_by(subject, block) %>%
  mutate(
    unch_card        = if_else(ch_card == card_left, card_right, card_left),
    reward_oneback   = factor(lag(reward, 1)),
    reward_twoback   = factor(lag(reward, 2)),
    reward_threeback = factor(lag(reward, 3)),

    ch_key_oneback   = lag(ch_key, 1),
    ch_key_twoback   = lag(ch_key, 2),
    ch_key_threeback = lag(ch_key, 3),

    stay_key         = lag(ch_key, 1) == ch_key,
    stay_key_twoback   = lag(ch_key, 2) == ch_key,
    stay_key_threeback = lag(ch_key, 3) == ch_key,

    reoffer_ch          = lag(ch_card, 1) == card_left | lag(ch_card, 1) == card_right,
    reoffer_ch_twoback   = lag(ch_card, 2) == card_left | lag(ch_card, 2) == card_right,
    reoffer_ch_threeback = lag(ch_card, 3) == card_left | lag(ch_card, 3) == card_right,

    reoffer_unch          = lag(unch_card, 1) == card_left | lag(unch_card, 1) == card_right,
    reoffer_unch_twoback   = lag(unch_card, 2) == card_left | lag(unch_card, 2) == card_right,
    reoffer_unch_threeback = lag(unch_card, 3) == card_left | lag(unch_card, 3) == card_right
  ) %>%
  ungroup()

# Fit models --------------------------------------------------------------

m_1 <-
  brm(
    stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_1, "Exp1/output/regression/m_1.rds")

m_2 <-
  brm(
    stay_key_twoback ~ reward_twoback + (1 + reward_twoback | subject),
    data    = df %>% filter(reoffer_ch_twoback == FALSE, reoffer_unch_twoback == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_2, "Exp1/output/regression/m_2.rds")

m_3 <-
  brm(
    stay_key_threeback ~ reward_threeback + (1 + reward_threeback | subject),
    data    = df %>% filter(reoffer_ch_threeback == FALSE, reoffer_unch_threeback == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_3, "Exp1/output/regression/m_3.rds")

# Figure 2E ---------------------------------------------------------------

draws_1 <- as_draws_df(m_1) %>% select(matches("^b_.*reward"))
draws_2 <- as_draws_df(m_2) %>% select(matches("^b_.*reward"))
draws_3 <- as_draws_df(m_3) %>% select(matches("^b_.*reward"))

get_stats <- function(x) {
  tibble(
    beta  = median(x),
    lower = hdi(x, .95)$CI_low,
    upper = hdi(x, .95)$CI_high
  )
}

df_plot <- bind_rows(
  get_stats(draws_1[[1]]),
  get_stats(draws_2[[1]]),
  get_stats(draws_3[[1]])
) %>%
  mutate(lag = 1:3)

p_2e <- ggplot(df_plot, aes(x = lag, y = beta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 1) +
  scale_x_continuous(breaks = 1:3) +
  labs(
    x = "Lag (trials back)",
    y = expression(beta["N-Previous outcome"])
  ) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13))
ggsave("Exp1/output/regression/plots/figure2E.pdf", p_2e, width = 5, height = 4)
