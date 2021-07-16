### Remove cases that are incomplete or unusable for whatever reason


# Temporary removal of cases to get a model running.

# These will be removed more systematically after I get a model running

# Imports -----------------------------------------------------------------
here::i_am("analysis/01_02_remove-bad-cases.R")
library(here)
library(tidyverse)

dictionary <- read_rds(here("analysis/data/derived_data/clean-question-list.rds"))
data_newnames <- read_rds(here("analysis/data/derived_data/data-newnames-completeonly.rds"))

data_short <- data_newnames %>%
  # select(-collectorid, -startdate, -enddate, -custom_data_1, -collector_type_source, -device, -contains("_mode_"),
  #        -region_survey_monk, -united_states_region_survey_monk)
  select(-(collid:coll_type_source), -(device:length(.)), -contains("mod_"))
# select(contains("time"), contains("dst"), contains("ntr"))



# Get travel variable names -----------------------------------------------

trav_prefix <- c("time", "dst", "ntr") %>% str_c(collapse = "|")
trav_regx <- paste0("^(b4)([a-z0-9_]+)", "(", trav_prefix, ")")

trav_varnames <- data_short %>%
  # extract column names as vector
  colnames() %>%
  str_subset(trav_regx)

# mode_regx <- str_extract(trav_varnames, "([a-z0-9]+)$") %>% unique() %>% str_c(collapse = "|")


# Get pid's of mph > 80 or mph < 1 --------------------------------------------------

mph_bad <- data_short %>%
  select(pid, matches("b4_.dst"), matches("b4_.time")) %>%
  # pivot_longer(cols = contains("dst"), names_to = "dst_type")
  pivot_longer(!pid,
               names_to = c("w_s", "d_t", "mode"),
               names_pattern = c("b4_?(.)(.*)_(.*)")
  ) %>%
  pivot_wider(names_from = d_t, values_from = value) %>%
  group_by(pid, w_s, mode) %>%
  mutate(mph = (dst/time)*60) %>%
  filter(mph < 1 | mph > 80) %>%
  pull(pid) %>%
  unique()


# Elimination ------------------------------------------------

data_elim <- data_short %>%
  mutate(across(contains(trav_varnames),
                ~replace_na(.x, 0))) %>%
  # Obvious multiple answerer
  ###   (misses at least 2 cases of this person doing this)
  filter(!(car_make %in% "jhgfjhgfd")) %>%

  # Non-binary gender FOR NOW
  filter((gender %in%  c("Female", "Male"))) %>%
  filter(!(pid %in% mph_bad))


# Write data --------------------------------------------------------------
write_rds(data_elim, "analysis/data/derived_data/data-good-cases.rds")



