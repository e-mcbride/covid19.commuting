# Temporary removal of cases to get a model running.

# These will be removed more systematically after I get a model running

# Imports -----------------------------------------------------------------
here::i_am("analysis/temp_rm-cases-brute-force.R")
library(here)
library(tidyverse)

dictionary <- read_rds(here("analysis/data/derived_data/clean-question-list.rds"))
data_newnames <- read_rds(here("analysis/data/derived_data/data-newnames-completeonly.rds"))

data_short <- data_newnames |>
  select(-collectorid, -startdate, -enddate, -custom_data_1, -collector_type_source, -device, -contains("_mode_"),
         -region_survey_monk, -united_states_region_survey_monk)



# Elimination ------------------------------------------------

data_elim <- data_short %>%
  mutate(across(contains(trav_varnames),
                ~replace_na(.x, 0))) %>%
  # Obvious multiple answerer
  ###   (misses at least 2 cases of this person doing this)
  filter(!(car_make %in% "jhgfjhgfd")) %>%

  # Non-binary gender FOR NOW
  filter((gender %in%  c("Female", "Male")))


### Thought: could replace with surveymonkey gender


# Var exploration to decide what to select for analysis -----

### EMPLOYMENT

data_short %>%
  select(pid, be4_employ) %>%
  View(title = "B4 Employ Status")


### INCOME

data_short %>%
  select(pid, starts_with("hh_inc")) %>%
  View()

#   surveymonkey income is not very populated, use mine


# Var Selection ----

# vars_to_change <- c("time_", "dist_", "trips")

trav_prefix <- c("time_", "dist_", "trips") %>% str_c(collapse = "|")
trav_regx <- paste0("^(be4_)([a-z0-9_]+)", "(", trav_prefix, ")")


string_cnames <- data_elim %>% colnames()


trav_varnames <- data_elim %>%
  # extract column names as vector
  colnames() %>%
  str_subset(trav_regx)

mode_regx <- str_extract(trav_varnames, "([a-z0-9]+)$") %>% unique() %>% str_c(collapse = "|")
# mode_regx <- paste0("(", trav_mode, ")")


mode_totals <- data_elim %>%
  select(pid, contains(trav_varnames)) %>%
  gather(key = "colname", value = "value", -pid) %>%
  mutate(
    # grp = str_extract(colname, "time|dist|trips"),
    mode = str_extract(colname, "(time|dist|trips)_([a-z0-9_]+)")) %>%
  group_by(pid, mode) %>%
  summarise(be4_tot = sum(value)) %>%
  spread(key = mode, value = be4_tot)


# drive alone, bike/walk/transit, carpool
abbr_mode_totals <- data_elim %>%
  select(pid, contains(trav_varnames)) %>%
  gather(key = "colname", value = "value", -pid) %>%
  mutate(
    measure = str_extract(colname, "time|dist|trips"),
    travmode = str_extract(colname, paste0("(", trav_mode, ")")),
    # travmode = str_extract(colname, "([a-z0-9]+)$"),
    # measmode = str_extract(colname, "(time|dist|trips)_([a-z0-9_]+)"),
    abbrmode = case_when(
      str_detect(travmode, "bike|walk|transit") ~ "eco",
      str_detect(travmode, "dralone")           ~ "dral",
      str_detect(travmode, "drothers|pass")     ~ "shar",
      TRUE                                      ~ "oth"
    ),
    abbr_meas_mode = str_c(measure, abbrmode, sep = "_")
    ) %>%
  group_by(pid, abbr_meas_mode) %>%
  summarise(be4_tot = sum(value)) %>%
  spread(key = abbr_meas_mode, value = be4_tot)

# %>%
#   group_by(pid, mode) %>%
#   summarise(be4_tot = sum(value)) %>%
#   spread(key = mode, value = be4_tot)



# Building the dataset        ====================================
data_b4_analyze <- data_elim %>%
  select(pid,
         # SES/demographics
         gender, be4_employ, hh_inc,

         # attitude
         starts_with("prompt")
         ) %>%
  left_join(abbr_mode_totals, by = "pid") %>%
  rename_with(~ str_replace(.x, "prompt_", "q"), starts_with("prompt"))


write_csv(data_b4_analyze, here("analysis/data/derived_data/data-analysis.csv"))





