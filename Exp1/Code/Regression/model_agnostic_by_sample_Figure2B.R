
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df <- read.csv("Exp1/Data/Analysis/df/df.csv")
df$reward_oneback <- factor(df$reward_oneback)

# effect by sample --------------------------------------------------------

m_key_sample <-
  brm(
    formula = stay_key ~ 0 + Intercept +
      sample * reward_oneback + (1 + reward_oneback | subject),
    data    = df %>% filter(reoffer_ch == FALSE, reoffer_unch == FALSE),
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(m_key_sample, "Exp1/output/regression/m_key_sample.rds")

# Figure 2B ---------------------------------------------------------------

effects <- conditional_effects(m_key_sample)

p_2b <- plot(effects, plot = FALSE)[[3]] +
  theme_bw() +
  labs(
    x     = "Sample",
    y     = "P(choose same location)",
    color = "Previous outcome"
  ) +
  scale_color_discrete(labels = c("Unrewarded", "Rewarded")) +
  scale_x_discrete(labels = c("abstract" = "Abstract", "story" = "Story")) +
  guides(
    fill  = "none",
    shape = "none",
    color = guide_legend(title = "Previous outcome")
  )
ggsave("Exp1/output/regression/plots/figure2B.pdf", p_2b, width = 5, height = 4)
