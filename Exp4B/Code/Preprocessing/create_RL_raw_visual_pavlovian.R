
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
  rt=as.numeric(df%>%filter(phase=="exp",trial_name=="stimuli")%>%pull(rt))
  #get_key_response
  response_key=df%>%filter(phase=="exp",trial_name=="stimuli")%>%pull(response)
  
  df=df %>%
    filter(trial_name == "reward",phase=="exp") %>%
    select(
      subject_id,                   # new location of condition
      block, trial_num,
      card_in_left,card_in_middle_left,card_in_middle_right,card_in_right,
      card_offered_1, card_offered_2, card_selected,
      location_offered_1,location_offered_2,location_selected,
      reward,prob_reward_selected, prob_reward_not_selected,
      counterbalance_reward,
      prob_reward_1,prob_reward_2,prob_reward_3,prob_reward_4,
      # Try to select optional columns if they exist
      any_of(c("first_row_location", "second_row_location", "third_row_location", "fourth_row_location")),
      any_of(c("first_row_color", "second_row_color", "third_row_color", "fourth_row_color")
      )) %>%rename(key1=location_offered_1,key2=location_offered_2,
                   card1=card_offered_1,card2=card_offered_2)%>%
    mutate(
      block = as.factor(block + 1L),
      trial = trial_num + 1L,
      response_key=response_key,
      ch_card = card_selected,
      ch_key = location_selected,
      prob_ch = as.numeric(prob_reward_selected),
      prob_unch = as.numeric(prob_reward_not_selected),
      reward_oneback = lag(reward),
      unch_card = if_else(ch_card == card1, card2, card1),
      unch_key = if_else(ch_key == key1, key2, key1),
      stay_card= 1*(ch_card==lag(ch_card)),
      stay_key = 1*(ch_key == lag(ch_key)),
      stay_unch=1*(ch_key==lag(unch_key)),
      reoffer_ch_card=1*(card1==lag(ch_card)|card2==lag(ch_card)),
      reoffer_unch_card=1*(card1==lag(unch_card)|card2==lag(unch_card)),
      reoffer_ch_key=1*(key1==lag(ch_key)|key2==lag(ch_key)),
      reoffer_unch_key=1*(key1==lag(unch_key)|key2==lag(unch_key)),
      accuracy = as.integer(prob_ch > prob_unch),
      delta_exp_value = abs(prob_ch - prob_unch),
      number_inattention = number_inattention,
      instructions_mistakes = instructions_mistakes,
      counterbalance_stimuli = if_else(any(card1=="blue"),1,2),
      rt = rt,
      bonus = case_when(
        reward == 1  ~ 0.005,
        reward == 0  ~ 0
      )
    ) %>%
    select(
      subject_id,
      block, trial,rt,card1,card2, ch_card,unch_card,key1,key2,response_key,ch_key,unch_key,
      reward, reward_oneback,
      prob_ch, prob_unch, stay_card, stay_key,stay_unch, 
      reoffer_ch_card,reoffer_unch_card,reoffer_ch_key,reoffer_unch_key,
      accuracy, delta_exp_value,
      number_inattention, instructions_mistakes, bonus,
      card_in_left,card_in_middle_left,card_in_middle_right,card_in_right,
      counterbalance_reward,counterbalance_stimuli,prob_reward_1,prob_reward_2,prob_reward_3,prob_reward_4,
      # Try to select optional columns if they exist
      any_of(c("first_row_location", "second_row_location", "third_row_location", "fourth_row_location")),
      any_of(c("first_row_color", "second_row_color", "third_row_color", "fourth_row_color"))
    )
  return (df)
}
# Get all CSV files in the directory
files <- list.files("data/data_collected/visual_pavlovian", pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
RL_raw_visual_pavlovian <- do.call(rbind, lapply(files, process_RL))%>%
  mutate(subject = as.numeric(factor(subject_id)))%>%relocate(subject)
save(RL_raw_visual_pavlovian,file="data/data_raw/visual_pavlovian/RL_raw.rdata")


#save without subject_id in csv
write.csv(RL_raw_visual_pavlovian%>%select(-subject_id),file="data/data_raw/visual_pavlovian/RL_raw.csv")

