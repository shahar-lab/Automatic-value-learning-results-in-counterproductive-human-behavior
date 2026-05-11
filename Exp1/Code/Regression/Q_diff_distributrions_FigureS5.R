
rm(list = ls())
library(tidyverse)
library(ggplot2)

df <- read.csv("Exp1/Data/Analysis/df/df.csv")

# Figure S5 ---------------------------------------------------------------

p_key <- ggplot(df, aes(x = absQkey_diff)) +
  geom_histogram(bins = 50, fill = "gray70", color = "black") +
  labs(
    x = expression("|Q"[left] - "Q"[right]*"|"),
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

p_card <- ggplot(df, aes(x = absQcard_diff)) +
  geom_histogram(bins = 50, fill = "gray70", color = "black") +
  labs(
    x = expression("|Q"[colorA] - "Q"[colorB]*"|"),
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("Exp1/Output/regression/plots/figureS5_Qdiff_key.pdf",   p_key,  width = 5, height = 4)
ggsave("Exp1/Output/regression/plots/figureS5_Qdiff_card.pdf",  p_card, width = 5, height = 4)
