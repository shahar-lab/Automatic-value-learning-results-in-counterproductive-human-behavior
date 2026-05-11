
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Load simulated data -----------------------------------------------------

load("Simulation/Data/Parameter_recovery/artificial_data.Rdata")

df <- df %>%
  mutate(accuracy = (exp_val_ch > exp_val_unch) * 1)

lambda_accuracy <- df %>%
  group_by(subject) %>%
  summarise(
    average_lambda = mean(lambda,   na.rm = TRUE),
    accuracy       = mean(accuracy, na.rm = TRUE),
    .groups = "drop"
  )

# Figure 1G ---------------------------------------------------------------

dir.create("Simulation/Output/parameter_recovery/plots", recursive = TRUE, showWarnings = FALSE)

p_1g <- ggplot(lambda_accuracy, aes(x = average_lambda, y = accuracy)) +
  geom_point(alpha = 0.4, color = "black") +
  geom_smooth(method = "lm", color = "steelblue") +
  xlim(0, 1) +
  labs(
    x = expression("Individual-level " * lambda),
    y = "Accuracy"
  ) +
  theme_bw()

ggsave("Simulation/Output/parameter_recovery/plots/figure1G_simulated.pdf",
       p_1g, width = 5, height = 4)

p_1g
