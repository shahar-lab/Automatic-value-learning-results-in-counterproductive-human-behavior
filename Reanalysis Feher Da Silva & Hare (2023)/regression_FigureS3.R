
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggdist)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

# get csv from repo of the original paper:
# https://github.com/carolfs/fmri_magic_carpet/tree/main/code/analysis/beh_noslow.csv

df <- read.csv("Reanalysis Feher Da Silva & Hare (2023)/data/beh_noslow.csv")

df <- df %>%
  mutate(
    reward         = factor(reward, levels = c(0, 1), labels = c("unrewarded", "rewarded")),
    reward_oneback = lag(reward),
    key1           = (choice1 == isymbol_lft) * 1,
    key2           = (choice2 == fsymbol_lft) * 1,
    stay_key2_to_1 = (lag(key2) == key1) * 1
  ) %>%
  filter(condition == "story")

model <- brm(
  stay_key2_to_1 ~ reward_oneback + (reward_oneback | participant),
  data    = df,
  family  = bernoulli("logit"),
  warmup  = 2, #DEMO use 2000 for full fit
  iter    = 4, #DEMO use 4000 for full fit
  chains  = 4,
  cores   = 4,
  prior   = myprior,
  backend = "cmdstanr"
)

saveRDS(model, "Reanalysis Feher Da Silva & Hare (2023)/output/model.rds")

# Figure S3B — conditional effects ----------------------------------------

p_s3b <- plot(conditional_effects(model), plot = FALSE)[[1]] +
  scale_x_discrete(labels = c("unrewarded" = "Unrewarded", "rewarded" = "Rewarded")) +
  labs(x = "Previous outcome", y = "P(Stay location 2 to 1)") +
  theme_bw()
ggsave("Reanalysis Feher Da Silva & Hare (2023)/output/plots/figureS3B.pdf",
       p_s3b, width = 4, height = 4)

# Figure S3C — posterior slope --------------------------------------------

posterior_theme <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y  = element_blank()
)

draws_s3c <- as.numeric(as_draws_df(model)$b_reward_onebackrewarded)
ci_s3c_low  <- quantile(draws_s3c, 0.025)
ci_s3c_high <- quantile(draws_s3c, 0.975)
p_s3c <- ggplot(data.frame(x = draws_s3c), aes(x = x)) +
  geom_density(fill = "grey60", alpha = 0.7, color = NA) +
  annotate("segment", x = ci_s3c_low, xend = ci_s3c_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(draws_s3c), y = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-0.5, 0.5) +
  labs(x = expression(beta["Previous outcome"]), y = NULL) +
  theme_bw() +
  posterior_theme
ggsave("Reanalysis Feher Da Silva & Hare (2023)/output/plots/figureS3C.pdf",
       p_s3c, width = 5, height = 4)
