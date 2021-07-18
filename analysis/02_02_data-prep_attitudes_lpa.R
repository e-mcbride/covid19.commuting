# 02_02 Analyze latent class-type model with Likert scale as continuous for the attitudes

here::i_am("analysis/02_02_data-prep_attitudes_lpa.R")
library(here)
library(tidyverse)
# library(tidymodels)


data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-mplus-ready.rds"))

data_attitudes <- data_mplus_ready
# dictionary <- read_rds("analysis/data/derived_data/clean-question-list.rds")
# data_elim <- read_rds("analysis/data/derived_data/data-good-cases.rds")
#
#
# data_likert_cont <- data_elim %>%
#   select(pid,
#          gender, hinc,
#          b4_emp, b4_wdays, b4_wfh, b4_vmeet, b4_wschd,
#          stu,
#          own_car,
#
#          # # variables for other timepoints
#          # w_now,w_chg, w_ind,
#          # ndays_wnow, ndays_wfh, ndays_vmeet,
#          # ndays_aft_wfh, ndays_aft_vmeet, ndays_aft_comm,
#
#
#          # attitude
#          starts_with("q")
#   ) %>%
#   # left_join(abbr_mode_totals, by = "pid") %>%
#
#   # Identify factor variables
#   mutate(across(where(is.character), forcats::as_factor)) %>%
#   mutate(hinc = fct_relevel(hinc, c("$0",
#                                     "Under $9,999",
#                                     "$10,000 to $24,999",
#                                     "$25,000 to $34,999",
#                                     "$35,000 to $49,999",
#                                     "$50,000 to $74,999",
#                                     "$75,000 to $99,999",
#                                     "$100,000 to $149,999",
#                                     "$150,000 to $199,999",
#                                     "$200,000 to $249,999",
#                                     "$250,000 or more"))) %>%
#   mutate(across(starts_with("q_"), fct_relevel, c("Strongly Disagree",
#                                                   "Disagree",
#                                                   "Neither Agree nor Disagree",
#                                                   "Agree",
#                                                   "Strongly Agree"))) %>%
#   mutate(across(where(is.factor), as.numeric, .names = "score_{.col}")) %>%
#   select(-(where(is.factor))) %>%
#   mutate(across(starts_with("score_"), ordered)) %>%
#   rename_with(~gsub("score_", "", .x, fixed = TRUE))

# print column names to copy/paste into Mplus syntax
colnames(data_attitudes) %>% str_c(collapse = " ") %>% noquote()

# write_file(an_coln, here("analysis/data/derived_data/analysis-colnames.txt"))


write_csv(data_attitudes, na = "-9999", col_names = FALSE, here("analysis/03_Mplus/data-likert-cont.csv"))
