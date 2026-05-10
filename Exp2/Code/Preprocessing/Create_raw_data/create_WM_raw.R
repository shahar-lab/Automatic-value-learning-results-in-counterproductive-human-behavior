library(tidyverse)
library(stringr)

process_wm <- function(file_path) {
  wm <- read.csv(file_path)
  subject_id <- unique(wm$subject_id)
  
  # Check inattention via "blur" events
  if ("event" %in% colnames(wm)) {
    number_inattention <- wm %>%
      filter(trial_index >= 15) %>%
      mutate(prev_event = lag(event)) %>%
      filter(event == "blur" & prev_event != "blur") %>%
      summarise(number_inattention = n()) %>%
      pull(number_inattention)
  } else {
    number_inattention <- 0
  }
  
  # Memory stimuli
  memory <- wm %>%
    filter(block_type == "exp", trial_name == "memory") %>%
    mutate(
      locations = str_extract_all(stimulus, "class=['\"](.*?)['\"]") %>%
        map_chr(~ .x %>%
                  str_remove_all("class=|['\"]") %>%
                  discard(~ .x == "fixation") %>%
                  paste(collapse = ",")),
      colors = str_extract_all(stimulus, "(?<=squares/)(.*?)(?=\\.png)") %>%
        map_chr(~ paste(.x, collapse = ","))
    ) %>%
    select(locations, colors)
  
  # Test trials
  wm <- wm %>%
    filter(block_type == "exp", trial_name == "test_squares") %>%
    select(subject_id, trial_num, set_size, rt, accuracy,
           response, correct_response, mapping, condition, stimulus) %>%
    mutate(
      location = str_extract(stimulus, "class=['\"](.*?)['\"]") %>% str_remove_all("class=|['\"]"),
      color = str_extract(stimulus, "(?<=squares/)(.*?)(?=\\.png)")
    )
  
  # Add memory info
  wm$locations <- memory$locations
  wm$colors <- memory$colors
  
  # Calculate accuracy and capacity
  mean_accuracy4 <- wm %>% filter(set_size == 4) %>% summarise(acc4 = mean(accuracy == "true", na.rm = TRUE)) %>% pull(acc4)
  mean_accuracy8 <- wm %>% filter(set_size == 8) %>% summarise(acc8 = mean(accuracy == "true", na.rm = TRUE)) %>% pull(acc8)
  
  capacity4 <- 4 * (2 * mean_accuracy4 - 1)
  capacity8 <- 8 * (2 * mean_accuracy8 - 1)
  
  wm <- wm %>%
    mutate(
      mean_accuracy4 = mean_accuracy4,
      mean_accuracy8 = mean_accuracy8,
      mean_accuracy_combined = (mean_accuracy4 + mean_accuracy8) / 2,
      capacity4 = capacity4,
      capacity8 = capacity8,
      capacity_combined = (capacity4 + capacity8) / 2,
      trial = trial_num + 1
    ) %>%
    select(-stimulus)
  
  # Exclusion flags
  wm$exclude_chance <- mean_accuracy4 < 0.5
  wm$exclude_4 <- mean_accuracy4 < mean_accuracy8
  wm$exclude_inattention <- number_inattention > 1
  
  return(wm)
}

# -------------------------
# Path to screening data
input_dir <- "data/empirical_data/data_collected/screening"

files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

WM_raw <- map_dfr(files, process_wm)

# Save
save(WM_raw, file = "data/empirical_data/data_raw/WM_raw.rdata")
write.csv(WM_raw %>% select(-subject_id),
          file = "data/empirical_data/data_raw/WM_raw.csv", row.names = FALSE)

cat("Processed", length(files), "WM files.\n")
