
rm(list = ls())
library(cmdstanr)
library(bayestestR)
library(ggplot2)
library(ggdist)
library(tidyverse)

# Load fits ---------------------------------------------------------------

fit_story    <- readRDS("Exp1/Output/computational_model/modelfit_empirical_story_full.rds")
fit_abstract <- readRDS("Exp1/Output/computational_model/modelfit_empirical_abstract_full.rds")

# Population-level --------------------------------------------------------

lambda_story    <- fit_story$draws(variables = "mu_lambda", format = "draws_matrix")
lambda_abstract <- fit_abstract$draws(variables = "mu_lambda", format = "draws_matrix")

# Individual-level --------------------------------------------------------

lambda_sbj_story    <- colMeans(fit_story$draws(variables = "lambda_sbj", format = "draws_matrix"))
lambda_sbj_abstract <- colMeans(fit_abstract$draws(variables = "lambda_sbj", format = "draws_matrix"))


# Save --------------------------------------------------------------------

save(lambda_story, lambda_abstract,
     lambda_sbj_story, lambda_sbj_abstract,
     file = "Exp1/Output/computational_model/parameters.RData")

# Figure 1D — population-level posteriors ---------------------------------

df_pop <- data.frame(
  value  = c(lambda_abstract, lambda_story),
  sample = c(rep("abstract", length(lambda_abstract)),
             rep("story",    length(lambda_story)))
)

p_1d <- ggplot(df_pop, aes(x = plogis(value), fill = sample)) +
  geom_density(alpha = 0.6, position = "identity", color = NA) +
  labs(x = expression(lambda), y = NULL, fill = "Sample") +
  scale_fill_manual(values = c("abstract" = "#6F6E6F", "story" = "#81c784")) +
  scale_x_continuous(breaks = seq(0, 1.0, by = 0.1), limits = c(0, 1.0)) +
  theme_bw() +
  theme(
    axis.title.y       = element_blank(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.line.y        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp1/Output/computational_model/plots/figure1D_population.pdf", p_1d, width = 6, height = 4)

# Individual-level dot plots ----------------------------------------------

df_lambdas <- tibble(
  lambda = c(lambda_sbj_abstract, lambda_sbj_story),
  sample = factor(
    c(rep("abstract", length(lambda_sbj_abstract)),
      rep("story",    length(lambda_sbj_story))),
    levels = c("abstract", "story")
  )
)

p_abstract <- ggplot(df_lambdas %>% filter(sample == "abstract"),
                     aes(x = lambda, y = 1)) +
  geom_dots(side = "top", dotsize = 1, binwidth = 0.01, alpha = 0.9,
            shape = 16, fill = "#6F6E6F", color = "#6F6E6F") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.title.y       = element_blank(),
    axis.line.y        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp1/Output/computational_model/plots/figure1D_lambda_abstract.pdf", p_abstract, width = 6, height = 3)

p_story <- ggplot(df_lambdas %>% filter(sample == "story"),
                  aes(x = lambda, y = 1)) +
  geom_dots(side = "top", dotsize = 1, binwidth = 0.01, alpha = 0.9,
            shape = 16, fill = "#207537", color = "#207537") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.title.y       = element_blank(),
    axis.line.y        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("Exp1/Output/computational_model/plots/figure1D_lambda_story.pdf", p_story, width = 6, height = 3)
