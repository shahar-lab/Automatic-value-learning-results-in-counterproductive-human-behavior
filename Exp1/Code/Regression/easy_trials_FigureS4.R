
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df <- read.csv("Exp1/Data/Analysis/df/df.csv") %>%
  mutate(
    reward_oneback = factor(reward_oneback),
    stay_key       = as.integer(stay_key)
  )

df_ev <- df %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE, delta_exp_value > 0.3)
df_q  <- df %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE, absQcard_diff  > 0.3)

# Fit models --------------------------------------------------------------

fit_ev <-
  brm(
    formula = stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df_ev,
    family  = bernoulli(link = "logit"),
    warmup  = 2, # DEMO use 2000 for full fit
    iter    = 4, # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(fit_ev, "Exp1/Output/regression/fit_ev.rds")

fit_q <-
  brm(
    formula = stay_key ~ reward_oneback + (1 + reward_oneback | subject),
    data    = df_q,
    family  = bernoulli(link = "logit"),
    warmup  = 2, # DEMO use 2000 for full fit
    iter    = 4, # DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(fit_q, "Exp1/Output/regression/fit_q.rds")

# Figure S4 ---------------------------------------------------------------

make_panel <- function(fit, title) {
  c_eff <- conditional_effects(fit, effects = "reward_oneback")
  df_ce <- as.data.frame(c_eff[[1]])
  ggplot(df_ce, aes(x = reward_oneback, y = estimate__)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.15) +
    ggtitle(title) +
    labs(x = "Previous outcome", y = "P(stay location)") +
    theme_bw()
}

p_a <- make_panel(fit_ev, "|ΔEV| > 0.3")
p_b <- make_panel(fit_q,  expression("|ΔQ"[color]*"| > 0.3"))

p_s4 <- cowplot::plot_grid(p_a, p_b, labels = c("A", "B"), nrow = 1)

ggsave("Exp1/Output/regression/plots/figureS4_easy_trials.pdf",
       p_s4, width = 8, height = 4)
