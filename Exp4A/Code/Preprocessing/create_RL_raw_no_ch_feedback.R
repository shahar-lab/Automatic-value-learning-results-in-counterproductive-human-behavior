
library(tidyverse)
rm(list = ls())

# preprocess --------------------------------------------------------------

process_RL <- function(file_path) {
  df <- read.csv(file_path)
  
  #get attention by mouse movement
  if(any(colnames(df)=="event")){
    number_inattention = df %>%
      filter(trial_index >= 15) %>%
      mutate(prev_event = lag(event)) %>%
      filter(event == "blur" & prev_event != "blur") %>%
      summarise(number_inattention = n()) %>%
      pull(number_inattention)
  }else{
    number_inattention=0 
  }
  
  
  instructions_mistakes <- sum(str_detect(df$stimulus %||% "", "You made a mistake\\."), na.rm = TRUE)
  
  #get ratings of instructions
  responses=df%>%filter(trial_type=="html-slider-response")%>%select(response)
  #get rt
  rt=as.numeric(df%>%filter(phase=="exp",trial_name=="cards1")%>%pull(rt))
  df=df%>%filter(trial_name=="reward2")%>%
    select(subject_id,block,trial_num,left_card,right_card,card_selected,key_selected,reward,prob1,prob2,prob3,prob4)%>%
    rename(trial=trial_num,ch_card=card_selected,ch_key=key_selected,card_left=left_card,card_right=right_card)%>%
    mutate(block=factor(block+1),trial=factor(trial+1),rt=rt,
           reward=factor(reward),reward_oneback=lag(reward),
           unch_card=if_else(ch_card==card_right,card_left,card_right),
           ch_card=ch_card+1,unch_card=unch_card+1,
           ch_key=if_else(ch_key==-1,NA,ch_key),
           card_left=card_left+1,card_right=card_right+1,
           reoffer_ch=lag(ch_card)==card_right|lag(ch_card)==card_left,
           reoffer_unch=lag(unch_card)==card_right|lag(unch_card)==card_left,
           stay_key=ch_key==lag(ch_key),
           stay_card=ch_card==lag(ch_card),
           stay_unch_card=lag(unch_card)==ch_card,
           prob_ch=case_when(ch_card==1~prob1,
                             ch_card==2~prob2,
                             ch_card==3~prob3,
                             ch_card==4~prob4),
           prob_unch=case_when(unch_card==1~prob1,
                               unch_card==2~prob2,
                               unch_card==3~prob3,
                               unch_card==4~prob4),
           accuracy=(prob_ch>prob_unch)*1,
           delta_exp_value=abs(prob_ch-prob_unch),
           prob_left = case_when(
             card_left == 1 ~ prob1,
             card_left == 2 ~ prob2,
             card_left == 3 ~ prob3,
             card_left == 4 ~ prob4,
             TRUE ~ NA_real_  # Default value 
           ),prob_right = case_when(
             card_right == 1 ~ prob1,
             card_right == 2 ~ prob2,
             card_right == 3 ~ prob3,
             card_right == 4 ~ prob4,
             TRUE ~ NA_real_  # Default value 
           ),better_loc=if_else(prob_left>prob_right,0,1),
           prev_loc_better=if_else(lag(ch_key)==better_loc,1,0),
                                   bonus=if_else(reward==1,0.0025,0),
                                   number_inattention=number_inattention,
           instructions_mistakes=instructions_mistakes)
  
  return (df)
}
# Get all CSV files in the directory
files <- list.files("data/data_collected/no_choice_feedback", pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
RL_raw_no_choice_feedback <- do.call(rbind, lapply(files, process_RL))%>%
  mutate(subject = as.numeric(factor(subject_id)))%>%relocate(subject) 
save(RL_raw_no_choice_feedback,file="data/data_raw/no_choice_feedback/RL_raw.rdata")


#save without subject_id in csv
write.csv(RL_raw_no_choice_feedback%>%select(-subject_id),file="data/data_raw/no_choice_feedback/RL_raw.csv")


