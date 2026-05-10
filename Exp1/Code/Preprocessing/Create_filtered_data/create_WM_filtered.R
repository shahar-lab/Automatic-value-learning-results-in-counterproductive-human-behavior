rm(list=ls())
# exclude subjects --------------------------------------------------------
WM_raw=read.csv(file="Exp1/Data/Raw/WM/WM_raw.csv")

remove_subjects <- WM_raw %>%
  group_by(subject) %>%
  summarise(
    mean_exclude_4 = mean(exclude_4, na.rm = TRUE),
    mean_exclude_chance = mean(exclude_chance, na.rm = TRUE),
    mean_exclude_inattention = mean(exclude_inattention, na.rm = TRUE)
  )

WM=WM_raw%>%filter(exclude_4==FALSE,exclude_chance==FALSE,exclude_inattention==FALSE)

# save(WM,file="data/data_filtered/WM.rdata")
# write.csv(WM,file="data/data_filtered/WM.csv")
