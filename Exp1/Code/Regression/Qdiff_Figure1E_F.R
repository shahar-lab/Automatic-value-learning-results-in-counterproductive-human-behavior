
rm(list = ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(ggplot2)

myprior <- prior(normal(0, 1), class = b)

df <- read.csv("Exp1/Data/Analysis/df/df.csv")

# effect by Qdiff ---------------------------------------------------------

m_qcard <-
  brm(
    formula = ch_key ~ 0 + Intercept + Qcard_diff * lambda,
    data    = df,
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(m_qcard, "Exp1/output/regression/m_qcard.rds")

m_qkey <-
  brm(
    formula = ch_key ~ 0 + Intercept + Qkey_diff * lambda,
    data    = df,
    family  = bernoulli(link = "logit"),
    warmup  = 2, #DEMO use 2000 for full fit
    iter    = 4, #DEMO use 4000 for full fit
    chains  = 4,
    cores   = 4,
    prior   = myprior,
    backend = "cmdstanr"
  )

saveRDS(m_qkey, "Exp1/output/regression/m_qkey.rds")

lambda_colors <- c("0.95" = "#F8766D", "0.8" = "#00BA38", "0.65" = "#619CFF")

# Figure 1E — Qcard_diff conditional effects ------------------------------

newdata_qcard <- expand.grid(
  Qcard_diff = seq(-1, 1, length.out = 100),
  lambda     = c(0.95, 0.80, 0.65)
)
preds_qcard <- fitted(m_qcard,
                      newdata    = newdata_qcard,
                      re_formula = NA,
                      summary    = TRUE)
newdata_qcard$estimate <- preds_qcard[, "Estimate"]
newdata_qcard$lower    <- preds_qcard[, "Q2.5"]
newdata_qcard$upper    <- preds_qcard[, "Q97.5"]
newdata_qcard <- newdata_qcard %>%
  mutate(lambda = factor(lambda, levels = c(0.95, 0.80, 0.65)))

p_1e <- ggplot(newdata_qcard,
       aes(x = Qcard_diff, y = estimate, color = lambda, fill = lambda)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_color_manual(values = lambda_colors) +
  scale_fill_manual(values = lambda_colors) +
  labs(x = "Qcard_diff", y = "P(right key)", color = "λ", fill = "λ") +
  theme_bw()
ggsave("Exp1/output/regression/plots/figure1E.pdf", p_1e, width = 6, height = 4)

# Figure 1F — Qkey_diff conditional effects -------------------------------

newdata_qkey <- expand.grid(
  Qkey_diff = seq(-1, 1, length.out = 100),
  lambda    = c(0.95, 0.80, 0.65)
)
preds_qkey <- fitted(m_qkey,
                     newdata    = newdata_qkey,
                     re_formula = NA,
                     summary    = TRUE)
newdata_qkey$estimate <- preds_qkey[, "Estimate"]
newdata_qkey$lower    <- preds_qkey[, "Q2.5"]
newdata_qkey$upper    <- preds_qkey[, "Q97.5"]
newdata_qkey <- newdata_qkey %>%
  mutate(lambda = factor(lambda, levels = c(0.95, 0.80, 0.65)))

p_1f <- ggplot(newdata_qkey,
       aes(x = Qkey_diff, y = estimate, color = lambda, fill = lambda)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_color_manual(values = lambda_colors) +
  scale_fill_manual(values = lambda_colors) +
  labs(x = "Qkey_diff", y = "P(right key)", color = "λ", fill = "λ") +
  theme_bw()
ggsave("Exp1/output/regression/plots/figure1F.pdf", p_1f, width = 6, height = 4)

