rm(list = ls())
load(file="data/data_raw/no_choice_feedback/RL_raw.rdata")
library(tidyverse)
#filter
filter_subject_id_data <- function(subject_id, data) {
  df <- data %>%
    filter(subject_id == !!subject_id) %>%
    filter(rt > 300, rt < 4000,!is.na(ch_key),!is.na(ch_card),!is.na(reward))
  df <- df %>%
    mutate(
      exclude_trial_omission = if_else(n() / 200 < 0.8, TRUE, FALSE),
      exclude_key_rep = if_else(mean(stay_key,na.rm=T) > 0.7|mean(stay_key,na.rm=T)<0.3, TRUE, FALSE)
    )
  
  df <- df %>%
    mutate(
      exclude_inattention = if_else(number_inattention > 1, TRUE, FALSE)
    )
  
  return(df)
}

df <- unique(RL_raw_no_choice_feedback$subject_id) %>%
  lapply(function(subject_id) filter_subject_id_data(subject_id, RL_raw_no_choice_feedback)) %>%
  bind_rows()



#count removed trials per sample
ntrials_before=RL_raw_no_choice_feedback%>%summarise(n())
ntrials_after=df%>%summarise(n())
1-ntrials_after/ntrials_before
filter=df%>%group_by(subject_id)%>%summarise(exclude_trial_omission=mean(exclude_trial_omission),exclude_key_rep=mean(exclude_key_rep),exclude_inattention=mean(exclude_inattention))
filter %>%
  summarise(
    n_trial_omission = sum(exclude_trial_omission == 1),
    n_key_rep_only = sum(exclude_key_rep == 1 & exclude_trial_omission == 0),
    n_inattention_only = sum(
      exclude_inattention == 1 &
        exclude_trial_omission == 0 &
        exclude_key_rep == 0
    )
  )
df=df%>%filter(exclude_trial_omission==0,exclude_key_rep==0,exclude_inattention == 0 | is.na(exclude_inattention)) #exclude subjects

save(df,file="data/data_filtered/no_choice_feedback/RL.rdata")
write.csv(df%>%select(-subject_id),file="data/data_filtered/no_choice_feedback/RL.csv")
