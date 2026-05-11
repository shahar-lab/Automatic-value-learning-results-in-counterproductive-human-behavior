
rm(list = ls())
library(cmdstanr)
library(tidyverse)
library(purrr)
library(HDInterval)
library(posterior)
library(gt)

# Load fit ----------------------------------------------------------------

fit <- readRDS("Exp2/Output/computational_model/modelfit_empirical_exp2.rds")

# Extract mu_ draws -------------------------------------------------------

draws_df <- fit$draws(format = "draws_df") %>%
  as_draws_df() %>%
  mutate(.draw = row_number())

long_df <- draws_df %>%
  select(.draw, starts_with("mu_")) %>%
  pivot_longer(-.draw, names_to = "full_param", values_to = "value") %>%
  mutate(
    param   = gsub("^mu_(.*)([123])$", "\\1", full_param),
    session = gsub("^mu_(.*)([123])$", "\\2", full_param)
  )

# Session summaries -------------------------------------------------------

fmt <- function(med, lo, hi) sprintf("%.2f [%.2f, %.2f]", med, lo, hi)

session_summaries <- long_df %>%
  group_by(param, session) %>%
  summarise(
    med   = median(value),
    hdi_l = hdi(value, credMass = 0.95)[1],
    hdi_u = hdi(value, credMass = 0.95)[2],
    .groups = "drop"
  ) %>%
  mutate(summary = fmt(med, hdi_l, hdi_u)) %>%
  select(param, session, summary) %>%
  pivot_wider(names_from = session, values_from = summary,
              names_prefix = "Session ")

# Session difference columns ----------------------------------------------

diffs <- long_df %>%
  pivot_wider(names_from = session, values_from = value,
              id_cols = c(.draw, param), names_prefix = "s") %>%
  group_by(param) %>%
  summarise(
    d21 = list(s2 - s1),
    d32 = list(s3 - s2),
    .groups = "drop"
  ) %>%
  mutate(
    `ΔSession 2 - 1` = map_chr(d21, ~fmt(median(.x), hdi(.x, 0.95)[1], hdi(.x, 0.95)[2])),
    `ΔSession 3 - 2` = map_chr(d32, ~fmt(median(.x), hdi(.x, 0.95)[1], hdi(.x, 0.95)[2]))
  ) %>%
  select(param, `ΔSession 2 - 1`, `ΔSession 3 - 2`)

# Combine and label -------------------------------------------------------

param_order  <- c("alpha", "beta", "decay_explore_card", "decay_explore_key",
                  "explore_card", "explore_key", "lambda")
param_labels <- c(
  alpha              = "α",
  beta               = "β",
  decay_explore_card = "ηvisual",
  decay_explore_key  = "ηspatial",
  explore_card       = "ρvisual",
  explore_key        = "ρspatial",
  lambda             = "λ"
)

final_table <- session_summaries %>%
  left_join(diffs, by = "param") %>%
  filter(param %in% param_order) %>%
  mutate(
    param = factor(param, levels = param_order, labels = param_labels[param_order])
  ) %>%
  arrange(param) %>%
  rename(Parameter = param)

# Table S1 ----------------------------------------------------------------

tbl_s1 <- final_table %>%
  gt() %>%
  tab_header(
    title    = "Table S1. Posterior parameter estimates across three sessions of Experiment 2."
  ) %>%
  tab_footnote(
    footnote = "Values represent unconstrained (i.e., before logistic transformation) posterior medians with 95% HDIs in brackets."
  ) %>%
  cols_align(align = "left", columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gtsave(tbl_s1, "Exp2/Output/computational_model/tableS1_all_params.html")

tbl_s1
