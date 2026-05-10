
##### Setup --------------------
# Install required packages if missing
required_packages <- c("brms", "cmdstanr", "tidyverse", "bayestestR")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

rm(list=ls())
library(brms)
library(cmdstanr)
library(tidyverse)
library(bayestestR)

myprior = prior(normal(0, 1),  class = b)

# Experiment 1 ------------------------------------------------------------
df_abstract <- read.csv("data/Exp1/ready_for_brms_regression/df_abstract.csv")
df_story <- read.csv("data/Exp1/ready_for_brms_regression/df_story.csv")
df=rbind(df_abstract,df_story)
df$reward_oneback <- factor(df$reward_oneback)
# Model-agnostic regression signature by sample --------------------------------------------------------
m_key_sample <-
  brm(
    formula=stay_key~0+Intercept+sample*reward_oneback+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 2, #DEMO use 2000 for full fit
    iter = 4, #DEMO use 4000 for full fit
    chains = 4,
    cores = 4,
    prior=myprior,
    backend = "cmdstanr"
  )


# Reanalysis of Feher Da Silva et al. (2023) ------------------------------

#get csv from repo of the original paper https://github.com/carolfs/fmri_magic_carpet/tree/main/code/analysis/beh_noslow.csv'

df = df%>%
  mutate(reward        = factor(reward, levels = c(0,1), labels = c("unrewarded", "rewarded")),
         reward_oneback = lag(reward),
         key1 = (choice1==isymbol_lft)*1, #1 for left, 0 for right
         key2 = (choice2==fsymbol_lft)*1, #1 for left, 0 for right
         stay_key2_to_1 = (lag(key2)==key1)*1)
#only story condition
df=df%>%filter(condition=='story')

model<-brm(stay_key2_to_1 ~ reward_oneback + (reward_oneback | participant) , 
           data   = df,
           warmup = 2, #DEMO use 2000 for full fit
           iter = 4, #DEMO use 4000 for full fit
           cores  = 4,
           chains = 4,
           prior = myprior,
           family = bernoulli("logit"),
           backend='cmdstan')

# Experiment 2 ------------------------------------------------------------
df <- read.csv("data/Exp2/ready_for_brms_regression/df.csv")
df$session=factor(df$session)
#outcome-irrelevant learning by session
m_oil_session <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*session+(1+reward_oneback*session|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 2, #DEMO use 2000 for full fit
    iter = 4, #DEMO use 4000 for full fit
    chains = 4,
    cores = 4,
    backend = "cmdstanr"
  )

# Combined analyses -------------------------------------------------------
df_combined <- read.csv("data/Combined/df_combined_exp1_2.csv")

# lambda_mean_accuracy ---------------------------------------------------------
lambda_accuracy_wm <- df_combined %>%
  group_by(subject, experiment) %>%
  summarise(
    average_lambda = mean(lambda, na.rm = TRUE),
    accuracy = mean(accuracy, na.rm = TRUE),
    capacity = mean(avg_capacity, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    z_capacity = scale(capacity),
    z_lambda = scale(average_lambda),
    z_accuracy = scale(accuracy)
  )

m_lambda_accuracy <-
  brm(
    formula=z_accuracy~z_lambda*experiment,
    data = lambda_accuracy_wm,
    warmup = 2, #DEMO use 2000 for full fit
    iter = 4, #DEMO use 4000 for full fit
    chains = 4,
    cores = 4,
    backend = "cmdstanr"
  )

# lambda by wm ------------------------------------------------------------
m_lambda_wm_combined=brm(
  formula=z_lambda~z_capacity*experiment,
  data = lambda_accuracy_wm,
  warmup = 2, #DEMO use 2000 for full fit
  iter = 4, #DEMO use 4000 for full fit
  chains = 4,
  cores = 4,
  prior=myprior,
  backend = "cmdstanr"
)

# regression coef by wm ---------------------------------------------------
m_coef_wm <- brm(
  formula = stay_key ~ 0 + Intercept + reward_oneback*experiment +(1 + reward_oneback | subject),
  data = df_combined%>%filter(reoffer_ch==F,reoffer_unch==F,trial!=1), 
  family = bernoulli(link = "logit"),
  warmup = 2, #DEMO use 2000 for full fit
  iter = 4, #DEMO use 4000 for full fit
  chains = 4,
  cores = 4,
  prior = myprior,
  backend = "cmdstanr"
)
