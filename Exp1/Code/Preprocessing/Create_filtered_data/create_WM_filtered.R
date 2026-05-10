rm(list=ls())
# exclude subjects --------------------------------------------------------
load("data/data_raw/WM_raw.rdata")

remove_subjects <- WM_raw %>%
  group_by(subject_id) %>%
  summarise(
    mean_exclude_4 = mean(exclude_4, na.rm = TRUE),
    mean_exclude_chance = mean(exclude_chance, na.rm = TRUE),
    mean_exclude_inattention = mean(exclude_inattention, na.rm = TRUE)
  )
remove_subjects%>%
  summarise(
    n_exclude_chance = sum(mean_exclude_chance == 1),
    n_exclude_4 = sum(mean_exclude_chance == 0 & mean_exclude_4 == 1),
    n_inattention_only = sum(
      mean_exclude_inattention == 1 &
        mean_exclude_4 == 0 &
        mean_exclude_chance == 0
    )
  )
WM=WM_raw%>%filter(exclude_4==FALSE,exclude_chance==FALSE,exclude_inattention==FALSE)
save(WM,file="data/data_filtered/WM.rdata")

write.csv(WM%>%select(-subject_id),file="data/data_filtered/WM.csv")

#stats exclusion
WM_raw%>%group_by(subject_id)%>%summarise(mean(exclude_4),mean(exclude_chance),mean(exclude_inattention))%>%
  summarise(
    remove_exclude_4 = sum(`mean(exclude_4)` > 0, na.rm = TRUE),
    remove_exclude_chance = sum(`mean(exclude_chance)` > 0, na.rm = TRUE),
    remove_inattention = sum(`mean(exclude_inattention)` > 0, na.rm = TRUE)
  )
