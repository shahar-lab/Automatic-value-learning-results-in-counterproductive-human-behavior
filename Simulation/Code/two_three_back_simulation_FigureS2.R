
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(bayestestR)
library(ggplot2)

# Load data ---------------------------------------------------------------

load("D:/sync/Sync/Ido_Ben_Artzi/R/oil_framing_effect/data/stanmodel_two_trial_back_alpha1/artificial_data.Rdata")

df <- df %>%
  arrange(subject, block, trial) %>%
  group_by(subject, block) %>%
  mutate(
    unch_card        = if_else(ch_card == card_left, card_right, card_left),
    reward_oneback   = factor(lag(reward, 1)),
    reward_twoback   = factor(lag(reward, 2)),
    reward_threeback = factor(lag(reward, 3)),

    stay_key           = lag(ch_key, 1) == ch_key,
    stay_key_twoback   = lag(ch_key, 2) == ch_key,
    stay_key_threeback = lag(ch_key, 3) == ch_key,

    reoffer_ch             = lag(ch_card, 1) == card_left | lag(ch_card, 1) == card_right,
    reoffer_ch_twoback     = lag(ch_card, 2) == card_left | lag(ch_card, 2) == card_right,
    reoffer_ch_threeback   = lag(ch_card, 3) == card_left | lag(ch_card, 3) == card_right,

    reoffer_unch           = lag(unch_card, 1) == card_left | lag(unch_card, 1) == card_right,
    reoffer_unch_twoback   = lag(unch_card, 2) == card_left | lag(unch_card, 2) == card_right,
    reoffer_unch_threeback = lag(unch_card, 3) == card_left | lag(unch_card, 3) == card_right
  ) %>%
  ungroup()

# Fit models --------------------------------------------------------------

m_1 <-
  brm(
    formula = stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2,    # DEMO use 2000 for full fit
    iter    = 4,    # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_1, "Simulation/output/two_three_back/m_1.rds")

m_2 <-
  brm(
    formula = stay_key_twoback ~ reward_twoback + (1 + reward_twoback | subject),
    data    = df %>% filter(reoffer_ch_twoback == FALSE, reoffer_unch_twoback == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2,    # DEMO use 2000 for full fit
    iter    = 4,    # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_2, "Simulation/output/two_three_back/m_2.rds")

m_3 <-
  brm(
    formula = stay_key_threeback ~ reward_threeback + (1 + reward_threeback | subject),
    data    = df %>% filter(reoffer_ch_threeback == FALSE, reoffer_unch_threeback == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2,    # DEMO use 2000 for full fit
    iter    = 4,    # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_3, "Simulation/output/two_three_back/m_3.rds")

# Figure S2 ---------------------------------------------------------------

draws_1 <- as_draws_df(m_1) %>% select(matches("^b_.*reward"))
draws_2 <- as_draws_df(m_2) %>% select(matches("^b_.*reward"))
draws_3 <- as_draws_df(m_3) %>% select(matches("^b_.*reward"))

get_stats <- function(x) {
  x <- as.numeric(x)
  tibble(
    beta  = median(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975)
  )
}

df_plot <- bind_rows(
  get_stats(draws_1[[1]]),
  get_stats(draws_2[[1]]),
  get_stats(draws_3[[1]])
) %>%
  mutate(lag = 1:3)

p_s2 <- ggplot(df_plot, aes(x = lag, y = beta)) +
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

ggsave("Simulation/output/two_three_back/plots/figureS2.pdf", p_s2, width = 5, height = 4)

p_s2
