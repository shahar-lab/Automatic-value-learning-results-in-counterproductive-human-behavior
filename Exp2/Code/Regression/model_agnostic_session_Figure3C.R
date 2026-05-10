
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

df <- read.csv("Exp2/Data/Analysis/df/df.csv")
df$session <- factor(df$session)

m_oil_session <-
  brm(
    formula = stay_key ~ 0 + Intercept + reward_oneback * session + (1 + reward_oneback * session | subject),
    data    = df %>% filter(reoffer_ch == F, reoffer_unch == F),
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    backend = "cmdstanr"
  )

saveRDS(m_oil_session, "Exp2/output/regression/m_oil_session.rds")

# Figure 3C ---------------------------------------------------------------

session_colors <- c(
  "Session 1" = "#ffab91",
  "Session 2" = "#81d4fa",
  "Session 3" = "#a5d6a7"
)

plot_data <- plot(conditional_effects(m_oil_session), plot = FALSE)[[3]]$data %>%
  mutate(session = factor(session,
                          levels = c(1, 2, 3),
                          labels = c("Session 1", "Session 2", "Session 3")))

pd <- position_dodge(width = 0.3)

p_3c <- ggplot(plot_data,
               aes(x = factor(reward_oneback), y = estimate__,
                   color = session, fill = session)) +
  geom_point(position = pd, shape = 21, size = 3) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.3, position = pd) +
  scale_fill_manual(values  = session_colors) +
  scale_color_manual(values = session_colors) +
  labs(
    x     = "Previous outcome",
    y     = "P(stay)",
    fill  = "Session",
    color = "Session"
  ) +
  theme_bw()
ggsave("Exp2/output/regression/plots/figure3C.pdf", p_3c, width = 5, height = 4)
