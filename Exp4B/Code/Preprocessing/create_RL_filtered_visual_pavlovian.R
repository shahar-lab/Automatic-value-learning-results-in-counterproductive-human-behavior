rm(list = ls())
RL_raw_visual_pavlovian=read.csv(file="Exp4B/Data/Raw/RL_raw.csv")


library(tidyverse)
#filter
filter_subject_id_data <- function(subject, data) {
  df <- data %>%
    filter(subject == !!subject) %>%
    filter(rt > 300, rt < 4000,ch_card!="null",ch_key!="null",reward!="null",reward_oneback!="null")
  df <- df %>%
    mutate(
      exclude_trial_omission = if_else(n() / 200 < 0.8, TRUE, FALSE)
    )
  
  df <- df %>%
    mutate(
      exclude_inattention = if_else(number_inattention > 1, TRUE, FALSE)
    )
  
  return(df)
}

df <- unique(RL_raw_visual_pavlovian$subject) %>%
  lapply(function(subject) filter_subject_id_data(subject, RL_raw_visual_pavlovian)) %>%
  bind_rows()


filter=df%>%group_by(subject)%>%summarise(exclude_trial_omission=mean(exclude_trial_omission),exclude_inattention=mean(exclude_inattention))

df=df%>%filter(exclude_trial_omission==0,exclude_inattention == 0 | is.na(exclude_inattention)) #exclude subjects

# save(df,file="data/data_filtered/visual_pavlovian/RL.rdata")
# write.csv(df,file="data/data_filtered/visual_pavlovian/RL.csv")

