
rm(list = ls())
library(cmdstanr)
library(tidyverse)
library(bayestestR)
library(ggdist)
library(ggplot2)

fit <- readRDS("Exp2/output/computational_model/modelfit_empirical_exp2.rds")

# Extract posterior draws -------------------------------------------------

lambda1 <- as_draws_df(fit$draws("mu_lambda1"))$mu_lambda1
lambda2 <- as_draws_df(fit$draws("mu_lambda2"))$mu_lambda2
lambda3 <- as_draws_df(fit$draws("mu_lambda3"))$mu_lambda3

# Posteriors on probability scale
describe_posterior(plogis(lambda1))
describe_posterior(plogis(lambda2))
describe_posterior(plogis(lambda3))

# Contrasts
describe_posterior(plogis(lambda1) - plogis(lambda2), test = "pd", ci = 0.95)
describe_posterior(plogis(lambda1) - plogis(lambda3), test = "pd", ci = 0.95)

# Figure 3B ---------------------------------------------------------------

posterior_lambda <- bind_rows(
  tibble(value = lambda1, session = "Session 1"),
  tibble(value = lambda2, session = "Session 2"),
  tibble(value = lambda3, session = "Session 3")
) %>%
  mutate(session = factor(session, levels = c("Session 1", "Session 2", "Session 3")))

session_colors <- c(
  "Session 1" = "#ffab91",
  "Session 2" = "#81d4fa",
  "Session 3" = "#a5d6a7"
)

p_3b <- ggplot(posterior_lambda, aes(x = plogis(value), fill = session, color = session)) +
  geom_density(alpha = 0.4, position = "identity") +
  scale_fill_manual(values  = session_colors) +
  scale_color_manual(values = session_colors) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(x = expression(lambda), fill = "Session", color = "Session") +
  theme_classic() +
  theme(
    axis.title.y       = element_blank(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.line.y        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp2/output/computational_model/plots/figure3B.pdf", p_3b, width = 6, height = 4)
