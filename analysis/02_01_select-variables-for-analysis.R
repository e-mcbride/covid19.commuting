# decide variables to use in analysis
here::i_am("analysis/02_01_select-variables-for-analysis.R")
library(here)
library(tidyverse)

data <- readr::read_rds(here("analysis/data/derived_data/data-newnames-completeonly.rds"))
dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds")

# data_short <- data |>
#   select(-collectorid, -startdate, -enddate, -custom_data_1, -collector_type_source, -device, -contains("_mode_"))


# make a non-vehicle category? Like combine bicycling, walking, transit?

# make the time, dist, n(trips) by mode 0 for those who did not travel by those modes

vars_to_change <- c("_time_", "_dist_", "n_trips")

pickvars <- data |>
  select(pid, contains(vars_to_change),
         starts_with("gender"),
         year_born_open, age,
         hh_inc, hh_inc_survey_monkey)

travel_repl_na <- data |>
  mutate(across(contains(vars_to_change),
         ~replace_na(.x, 0))) |>
  select(pid, contains(vars_to_change))


analysisvars <- data |>
  select(pid, contains(vars_to_change), own_car,  starts_with("prompt"))


