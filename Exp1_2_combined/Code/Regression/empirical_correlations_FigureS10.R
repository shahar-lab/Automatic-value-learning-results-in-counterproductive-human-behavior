rm(list = ls())
library(tidyverse)
library(corrplot)

# Load data ---------------------------------------------------------------

df <- read.csv("Exp1_2_combined/Data/df_combined_exp1_2.csv")

# Average parameters per subject ------------------------------------------

df_subj <- df %>%
  group_by(subject) %>%
  summarise(
    alpha         = mean(alpha,             na.rm = TRUE),
    beta          = mean(beta,              na.rm = TRUE),
    lambda        = mean(lambda,            na.rm = TRUE),
    rho_visual    = mean(explore_card,      na.rm = TRUE),
    rho_spatial   = mean(explore_key,       na.rm = TRUE),
    eta_visual    = mean(decay_explore_card, na.rm = TRUE),
    eta_spatial   = mean(decay_explore_key,  na.rm = TRUE)
  )

# Correlation matrix ------------------------------------------------------

cor_mat <- cor(
  df_subj %>% select(alpha, beta, lambda, rho_visual, rho_spatial, eta_visual, eta_spatial),
  use = "pairwise.complete.obs"
)

col_labels <- expression(
  alpha, beta, lambda,
  rho[visual], rho[spatial],
  eta[visual], eta[spatial]
)

# Figure S10 --------------------------------------------------------------

pdf("Exp1_2_combined/output/regression/plots/figureS10.pdf", width = 6, height = 5.5)
corrplot(
  cor_mat,
  method      = "color",
  type        = "upper",
  col         = colorRampPalette(c("#b2182b", "white", "#2166ac"))(200),
  addCoef.col = "black",
  number.cex  = 0.75,
  tl.col      = "black",
  tl.srt      = 0,
  tl.cex      = 0.95,
  cl.ratio    = 0.15,
  diag        = TRUE,
  mar         = c(0, 0, 0, 0),
  addgrid.col = NA,
  col.lim     = c(-1, 1),
  is.corr     = TRUE
)
dev.off()
