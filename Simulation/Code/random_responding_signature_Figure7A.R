rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)

# Load data ---------------------------------------------------------------

load("Simulation/Data/Random_responding/Signature/artificial_data_200_signature.Rdata")

df <- df %>% mutate(
  reward_oneback = factor(lag(reward)),
  stay_key       = 1 * (ch_key == lag(ch_key)),
  reoffer_ch     = if_else(lag(ch_card) == card_right | lag(ch_card) == card_left, 1, 0),
  unch_card      = if_else(ch_card == card_right, card_left, card_right),
  reoffer_unch   = if_else(lag(unch_card) == card_right | lag(unch_card) == card_left, 1, 0)
)

# Fit model ---------------------------------------------------------------

m_key <-
  brm(
    formula = stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch == F, reoffer_unch == F),
    family  = bernoulli(link = "logit"),
    warmup  = 2,    # DEMO use 2000 for full fit
    iter    = 4,    # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_key, "Simulation/Output/random_responding/signature/m_key.rds")

# Figure 7A ---------------------------------------------------------------

c_eff <- conditional_effects(m_key)
p <- plot(c_eff, plot = FALSE)[[1]] +
  theme_bw() +
  scale_x_discrete(
    name   = "Previous outcome",
    labels = c("0" = "Unrewarded", "1" = "Rewarded")
  ) +
  ylab("P(stay_location)")

ggsave("Simulation/Output/random_responding/signature/plots/figure7A.pdf", p, width = 6, height = 4)

p