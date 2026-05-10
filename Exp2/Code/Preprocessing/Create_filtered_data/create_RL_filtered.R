
rm(list = ls())
df_raw=read.csv(file="Exp2/Data/Raw/RL/RL_raw.csv")
library(tidyverse)
#filter
filter_subject_id_data <- function(subject, data) {
  df <- data %>%
    filter(subject == !!subject) %>%
    filter(rt > 300, rt < 4000,!is.na(ch_key),!is.na(ch_card),!is.na(reward))
  
  df=df %>%
    mutate(
      trials = n(),  # Count trials in each session
      exclude_trials = if_else(trials / 600 < 0.8 , TRUE, FALSE),
      exclude_inattention = if_else(number_inattention > 1, TRUE, FALSE)
    )%>%
    group_by(subject) %>%  # Group by subject to check missing sessions
    mutate(
      missing_sessions = paste0(setdiff(c(1, 2, 3), unique(session)), collapse = ", "),  # List missing sessions
      missing_session_flag = if_else(missing_sessions != "", TRUE, FALSE)  # Flag if any session is missing
    ) %>%
    ungroup()%>%
    mutate(exclude_key_rep = if_else(mean(stay_key,na.rm=T) > 0.7|mean(stay_key,na.rm=T)<0.3, TRUE, FALSE))
  
  return(df)
}

df <- unique(df_raw$subject) %>%
  lapply(function(subject) filter_subject_id_data(subject, df_raw)) %>%
  bind_rows()

df%>%group_by(subject)%>%summarise(mean(exclude_key_rep))%>%
  summarise(
    remove_key_rep = sum(`mean(exclude_key_rep)` > 0, na.rm = TRUE)
  )

#fix this
filter=df%>%group_by(subject)%>%summarise(missing_sessions=unique(missing_sessions),
                                                     exclude_trials=max(exclude_trials),
                                                     exclude_key_rep=max(exclude_key_rep),
                                                     exclude_inattention=max(exclude_inattention),
                                                     mean_motivation=mean(mean_motivation))



df=df%>%filter(exclude_trials==FALSE,exclude_key_rep==FALSE,
               exclude_inattention==FALSE) #exclude subjects
df=df%>%group_by(subject)%>%mutate(missing_sessions = paste0(setdiff(c(1, 2, 3), unique(session)), collapse = ", "),
                                   exclude_missing_sessions=if_else(nchar(missing_sessions) >1, TRUE, FALSE))%>%ungroup()

# save(df,file="data/empirical_data/data_filtered/RL.rdata")
# write.csv(df,file="data/empirical_data/data_filtered/RL.csv")
# 
