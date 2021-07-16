# decide variables to use in analysis
here::i_am("analysis/02_01_data-prep-analysis.R")
library(here)
library(tidyverse)
library(tidymodels)

# data <- readr::read_rds(here("analysis/data/derived_data/data-newnames-completeonly.rds"))
dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds")
data_elim <- read_rds("analysis/data/derived_data/data-good-cases.rds")

# Get travel variable names -----------------------------------------------

trav_prefix <- c("time", "dst", "ntr") %>% str_c(collapse = "|")
trav_regx <- paste0("^(b4)([a-z0-9_]+)", "(", trav_prefix, ")")
# paste0("(", trav_prefix,")", "_([a-z0-9_]+)")


trav_varnames <- data_elim %>%
  # extract column names as vector
  colnames() %>%
  str_subset(trav_regx)

mode_regx <- str_extract(trav_varnames, "([a-z0-9]+)$") %>% unique() %>% str_c(collapse = "|")





# Var Selection ----

# mode totals, ignoring work or school or everything else
mode_totals <- data_elim %>%
  select(pid, contains(trav_varnames)) %>%
  gather(key = "colname", value = "value", -pid) %>%
  mutate(
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


# Building the dataset, prepping for Mplus====================================
data_mplus_ready <- data_elim %>%
  select(pid,
         # SES/demographics
         gender, hinc,
         b4_emp, b4_wdays, b4_wfh, b4_vmeet, b4_wschd,
         stu,
         own_car,

         # # Variables for other timepoints
         # w_now,w_chg, w_ind,
         # ndays_wnow, ndays_wfh, ndays_vmeet,
         # ndays_aft_wfh, ndays_aft_vmeet, ndays_aft_comm,

         # attitude
         starts_with("q")
  ) %>%
  left_join(abbr_mode_totals, by = "pid") %>%

  # Identify factor variables
  mutate(across(where(is.character), forcats::as_factor)) %>%
  mutate(hinc = fct_relevel(hinc, c("$0",
                                    "Under $9,999",
                                    "$10,000 to $24,999",
                                    "$25,000 to $34,999",
                                    "$35,000 to $49,999",
                                    "$50,000 to $74,999",
                                    "$75,000 to $99,999",
                                    "$100,000 to $149,999",
                                    "$150,000 to $199,999",
                                    "$200,000 to $249,999",
                                    "$250,000 or more"))) %>%
  mutate(across(starts_with("q_"), fct_relevel, c("Strongly Disagree",
                                               "Disagree",
                                               "Neither Agree nor Disagree",
                                               "Agree",
                                               "Strongly Agree"))) %>%
  mutate(across(where(is.factor), as.numeric, .names = "score_{.col}")) %>%
  select(-(where(is.factor))) %>%
  mutate(across(starts_with("score_"), ordered)) %>%
  rename_with(~gsub("score_", "", .x, fixed = TRUE))


# check if the releveling is working
data_mplus_ready %>%
  dplyr::select(where(is.factor)) %>%
  map(levels)

#  Mplus only allows 8 char, so any colnames > 8 char?

# ((data_dum %>% colnames() %>% nchar()) > 8) %>% which()
((data_mplus_ready %>% colnames() %>% nchar()) > 8) %>% which()




# Output =========================================================

# print column names to copy/paste into Mplus VARIABLE argument
an_coln <- colnames(data_mplus_ready) %>% str_c(collapse = " ") %>% noquote()
an_coln

write_rds(data_mplus_ready, here("analysis/data/derived_data/data-mplus-ready.rds"))



