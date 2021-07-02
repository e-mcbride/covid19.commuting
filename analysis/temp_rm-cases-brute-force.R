# Temporary removal of cases to get a model running.

# These will be removed more systematically after I get a model running

# Imports -----------------------------------------------------------------
here::i_am("analysis/temp_rm-cases-brute-force.R")
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


# string_cnames <- data_short %>% colnames()


trav_varnames <- data_short %>%
  # extract column names as vector
  colnames() %>%
  str_subset(trav_regx)

trav_varnames

mode_regx <- str_extract(trav_varnames, "([a-z0-9]+)$") %>% unique() %>% str_c(collapse = "|")




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

paste0("(", trav_prefix,")", "_([a-z0-9_]+)")

# mode totals, ignoring work or school or everything else
mode_totals <- data_elim %>%
  select(pid, contains(trav_varnames)) %>%
  gather(key = "colname", value = "value", -pid) %>%
  mutate(
    # grp = str_extract(colname, "time|dist|trips"),
    mode = str_extract(colname, paste0("(", trav_prefix,")", "_([a-z0-9_]+)"))) %>%
  group_by(pid, mode) %>%
  summarise(b4_tot = sum(value)) %>%
  spread(key = mode, value = b4_tot)


# drive alone, bike/walk/transit, carpool
eco <- "bik|wlk|trn" #eco-friendly: bike, walk, transit
dal <- "dal" # drive alone
shr <- "dot|pass" # sharing/carpooling

abbr_mode_totals <- data_elim %>%
  select(pid, contains(trav_varnames)) %>%
  gather(key = "colname", value = "value", -pid) %>%
  mutate(
    measure = str_extract(colname, trav_prefix),
    travmode = str_extract(colname, paste0("(", mode_regx, ")")),
    # travmode = str_extract(colname, "([a-z0-9]+)$"),
    # measmode = str_extract(colname, "(time|dist|trips)_([a-z0-9_]+)"),
    abbrmode = case_when(
      str_detect(travmode, eco) ~ "eco",
      str_detect(travmode, dal) ~ "dal",
      str_detect(travmode, shr) ~ "shr",
      TRUE                      ~ "oth"
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
         gender, b4_emp, hinc,

         # attitude
         starts_with("q")
         ) %>%
  left_join(abbr_mode_totals, by = "pid")
  # rename_with(~ str_replace(.x, "prompt_", "q"), starts_with("prompt"))

#  Mplus only allows 8 char, so any colnames > 8 char?
((data_b4_analyze %>% colnames() %>% nchar()) > 8) %>% any()

# print column names to copy/paste into Mplus VARIABLE argument
noquote(colnames(data_b4_analyze))
an_cn <- colnames(data_b4_analyze)
str_c(an_cn, collapse = " ") %>% noquote()


write_csv(data_b4_analyze, here("analysis/data/derived_data/data-analysis.csv"))





