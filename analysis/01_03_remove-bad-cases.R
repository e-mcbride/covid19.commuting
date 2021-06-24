### Remove cases that are incomplete or unusable for whatever reason

# Imports -----------------------------------------------------------------
here::i_am("analysis/01_03_remove-bad-cases.R")
library(here)
library(tidyverse)

dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds")

# Compare pids in "complete only" vs "incomplete included" ----------------
data_all <- readr::read_rds(here("analysis/data/derived_data/data-newnames.rds"))
data_completeonly <- readr::read_rds(here("analysis/data/derived_data/data-newnames-completeonly.rds"))

inc_ids<- data_all$pid[!(data_all$pid %in% data_completeonly$pid)]

# Find out why the "incompletes" were removed OR just remove them and move on for speed
data_incomplete <-  data_all |> filter(pid %in% inc_ids)
# They stopped before finishing. Remove them

# Remove bad cases --------------------------------------------------------



# find people who cheated the system


## what did people put for the free-answer questions
data_completeonly |> select(car_year) |> unique() |> View()
data_completeonly |> filter(car_year == "mbnvnv") |> View()

data_completeonly |>
  mutate(car_make = str_to_lower(car_make)) |>
  select(car_make) |>
  unique()
  # ggplot(aes(car_make)) + geom_bar()

ggplot(data_completeonly, aes(car_make)) + geom_bar()

x <- data_completeonly

# check incomes against each other ----------------------------------------


# check age/year born against each other ----------------------------------




# check genders against each other ----------------------------------------


# check ZIPS and cities against each other --------------------------------






