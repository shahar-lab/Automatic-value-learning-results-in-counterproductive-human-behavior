
rm(list = ls())
library(cmdstanr)
library(bayestestR)
library(ggplot2)

fit <- readRDS("Exp4B/output/computational_model/modelfit_empirical_exp4B.rds")

# Extract posterior draws -------------------------------------------------

lambda <- fit$draws("mu_lambda", format = "draws_matrix")

hdi(plogis(lambda))
mean(plogis(lambda))
median(plogis(lambda))

# Figure 5D ---------------------------------------------------------------

df <- data.frame(value = as.numeric(lambda))

ci <- hdi(plogis(df$value), ci = 0.95)

p_5d <- ggplot(df, aes(x = plogis(value))) +
  geom_density(fill = "#6F6E6F", alpha = 0.7, color = NA) +
  annotate("segment", x = ci$CI_low, xend = ci$CI_high, y = 0, yend = 0,
           linewidth = 1) +
  annotate("point", x = median(plogis(df$value)), y = 0, size = 2) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(x = expression(lambda), y = NULL) +
  theme_bw() +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.line.y        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp4B/output/computational_model/plots/figure5D.pdf",
       p_5d, width = 6, height = 4)
