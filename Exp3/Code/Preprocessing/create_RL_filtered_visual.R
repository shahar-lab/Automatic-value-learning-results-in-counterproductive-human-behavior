rm(list = ls())
load(file="data/data_raw/visual/RL_raw.rdata")
library(tidyverse)
#filter
filter_subject_id_data <- function(subject_id, data) {
  df <- data %>%
    filter(subject_id == !!subject_id) %>%
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

df <- unique(RL_raw_visual$subject_id) %>%
  lapply(function(subject_id) filter_subject_id_data(subject_id, RL_raw_visual)) %>%
  bind_rows()



#count removed trials per sample
# ntrials_before=RL_raw_visual%>%group_by(sample)%>%summarise(n())
# ntrials_after=df%>%group_by(sample)%>%summarise(n())

filter=df%>%group_by(subject_id)%>%summarise(exclude_trial_omission=mean(exclude_trial_omission),exclude_inattention=mean(exclude_inattention))
filter %>%
  summarise(
    n_trial_omission = sum(exclude_trial_omission == 1),
    n_inattention_only = sum(
      exclude_inattention == 1 &
        exclude_trial_omission == 0
    )
  )
df=df%>%filter(exclude_trial_omission==0,exclude_inattention == 0 | is.na(exclude_inattention)) #exclude subjects

save(df,file="data/data_filtered/visual/RL.rdata")
write.csv(df%>%select(-subject_id),file="data/data_filtered/visual/RL.csv")
