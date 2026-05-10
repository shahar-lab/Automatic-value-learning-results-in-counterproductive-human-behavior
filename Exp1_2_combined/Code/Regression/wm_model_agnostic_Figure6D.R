
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(bayestestR)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df_combined <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")



# Fit model ---------------------------------------------------------------
df_combined$reward_oneback <- factor(df_combined$reward_oneback)
m_key <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback * experiment +
      (1 + reward_oneback | subject),
    data    = df_combined %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE,
                                     trial != 1),
    family  = bernoulli(link = "logit"),
    warmup  = 2000, #fast fit
    iter    = 4000, #fast fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(m_key, "Exp1_2_combined/output/regression/m_key.rds")

# Extract subject-level coefficients --------------------------------------
capacity1 <- df_combined %>%filter(experiment=="Experiment 1")%>%
  select(subject,experiment, avg_capacity) %>%
  distinct() %>%
  arrange(subject) %>%
  mutate(
    subject_num = row_number()
  )

capacity2 <- df_combined %>%filter(experiment=="Experiment 2") %>%
  select(subject,experiment, avg_capacity) %>%
  distinct() %>%
  arrange(subject) %>%
  mutate(
    subject_num = row_number() + 504,
  )

capacity_all <- bind_rows(capacity1, capacity2) %>%
  filter(!is.na(avg_capacity))

subj_coef <- coef(m_key)$subject

coefs_1 <- subj_coef[, "Estimate", "reward_oneback1"][1:504]

coefs_2 <- subj_coef[, "Estimate", "reward_oneback1"][505:662] +
  fixef(m_key)[4,1]

coef_df <- bind_rows(
  data.frame(
    subject_num = 1:504,
    coefs = as.numeric(coefs_1)
  ),
  data.frame(
    subject_num = 505:662,
    coefs = as.numeric(coefs_2)
  )
)

coef_all <- coef_df %>%
  inner_join(capacity_all, by = "subject_num") %>%
  mutate(
    experiment = factor(experiment),
    scaled_coefs = as.numeric(scale(coefs)),
    scaled_capacity = as.numeric(scale(avg_capacity))
  )

table(coef_all$experiment)
nrow(coef_all)

model_scaled <- brm(
  formula = scaled_coefs ~ scaled_capacity * experiment,
  data = coef_all,
  warmup = 2000,
  iter = 4000,
  chains = 4,
  cores = 4,
  prior = myprior,
  backend = "cmdstanr"
)

saveRDS(model_scaled, "Exp1_2_combined/output/regression/m_key_capacity.rds")

# Figure 6D ---------------------------------------------------------------

p_6d <- ggplot(coef_all, aes(x = avg_capacity, y = coefs)) +
  geom_point(aes(shape = experiment), alpha = 0.2, color = "black") +
  geom_smooth(aes(group = 1), method = "lm") +
  scale_shape_manual(
    values = c("Experiment 1" = 1, "Experiment 2" = 16),
    labels = c("Experiment 1" = "1", "Experiment 2" = "2")
  ) +
  labs(
    x     = "WM capacity",
    y     = "Individual beta previous outcome",
    shape = "Experiment"
  ) +
  theme_bw()
ggsave("Exp1_2_combined/output/regression/plots/figure6D.pdf", p_6d, width = 5, height = 4)
