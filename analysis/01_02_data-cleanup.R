# data cleanup

here::i_am("analysis/01_02_data-cleanup.R")
library(here)
library(tidyverse)
# raw_data <- readr::read_rds("analysis/data/derived_data/raw-data.rds")
raw_data_cells <- read_rds("analysis/data/derived_data/raw-data-cells.rds")



# task: combine 1st and 2nd rows into column names (refer to Qatar project "Access2_AutomateTAZChars.Rmd")


# 1. Find the word "Response" from 2nd name row and remove it (because it is not necessary for it to be part of the variable names)
# clear_response <-
raw_data_cells |>

  #
  mutate(noresp =
           if_else(
             condition = (row == 2 & value == "Response"),
             true = NA_character_,
             false = value
           )) |>
  # mutate(abc = na_if())
  View()


# 2. Shorten the long sentence variable names
#
# Remove columns with no data at all ()


# task: create data dictionary (actual question asked w/ variable name)
raw_data <- read_rds("analysis/data/derived_data/raw-data.rds")

# dictionary <- raw_data |>
#   # slice(1) |>
#   select(11:ncol(raw_data)) |>
#   pivot_longer(cols = everything(), names_to = "name1") |>
#   unique()
#   group_by(name1) |>
#   summarise(n())

raw_data_expanded <- read_csv("analysis/data/raw_data/raw-covid19-survey-expanded.csv", col_names = FALSE)

pivot_test <- raw_data_expanded |>
  slice(1:2) |>
  # mutate(across(everything(), ~as.character(.x))) |>
  pivot_longer(cols = everything(), names_to = "name1")


# explore

raw_data_expanded_cells <- read_rds("analysis/data/derived_data/raw-data-expanded-cells.rds")
