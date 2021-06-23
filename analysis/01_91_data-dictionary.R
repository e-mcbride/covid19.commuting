# Create data dictionary
here::i_am("analysis/01_91_data-dictionary.R")
library(here)
library(tidyverse)

# 1. Start with `clean_question_list` created in 01_02
dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds") |>
  select(-short_row1, -short_row2)

# 2.  Get all the possible unique values for each question

# 3.



# raw_data <- read_rds("analysis/data/derived_data/raw-data.rds")

# dictionary <- raw_data |>
#   # slice(1) |>
#   select(11:ncol(raw_data)) |>
#   pivot_longer(cols = everything(), names_to = "name1") |>
#   unique()
#   group_by(name1) |>
#   summarise(n())

#
# pivot_test <- raw_data_exp |>
#   slice(1:2) |>
#   # mutate(across(everything(), ~as.character(.x))) |>
#   pivot_longer(cols = everything(), names_to = "name1")


