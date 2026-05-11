
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(tidybayes)
library(ggplot2)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

df_combined <- df_combined %>%
  arrange(subject, block, trial) %>%
  group_by(subject, block) %>%
  mutate(prev_ch_key = lag(ch_key)) %>%
  ungroup() %>%
  mutate(
    ch_key      = factor(ch_key),
    prev_ch_key = factor(prev_ch_key),
    reward_oneback = factor(reward_oneback)
  )

df_filt <- df_combined %>%
  filter(reoffer_ch == FALSE, reoffer_unch == FALSE)

contrasts(df_filt$prev_ch_key)    <- c(-1, 1)
contrasts(df_filt$reward_oneback) <- c(-1, 1)

# Fit model ---------------------------------------------------------------

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

saveRDS(m_prev, "Exp1_2_combined/Output/regression/m_prev.rds")

# Figure 2H ---------------------------------------------------------------

subj_coefs <- m_prev %>%
  spread_draws(r_subject[subject, term], b_prev_ch_key1) %>%
  filter(term == "prev_ch_key1") %>%
  mutate(coef_prev_ch_key = b_prev_ch_key1 + r_subject) %>%
  group_by(subject) %>%
  summarise(coef_prev_ch_key = mean(coef_prev_ch_key))

subj_explore <- df_combined %>%
  group_by(subject) %>%
  summarise(explore_key_mean = mean(explore_key, na.rm = TRUE))

compare_df <- subj_coefs %>%
  left_join(subj_explore, by = "subject") %>%
  left_join(df_combined %>% select(subject, experiment) %>% distinct(), by = "subject")

p_2h <- ggplot(compare_df,
               aes(x = explore_key_mean, y = coef_prev_ch_key)) +
  geom_point(aes(shape = experiment), alpha = 0.2, color = "black") +
  geom_smooth(aes(group = 1), method = "lm") +
  scale_shape_manual(
    values = c("Experiment 1" = 1, "Experiment 2" = 16),
    labels = c("Experiment 1" = "1", "Experiment 2" = "2")
  ) +
  labs(
    x     = expression("Individual-level " * rho[spatial]),
    y     = "Individual-level location-switching",
    shape = "Experiment"
  ) +
  theme_bw()
ggsave("Exp1_2_combined/Output/regression/plots/figure2H.pdf", p_2h, width = 5, height = 4)
