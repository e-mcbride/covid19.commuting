# decide variables to use in analysis
here::i_am("analysis/02_01_vars-for-analysis.R")
library(here)
library(tidyverse)

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



# Building the dataset, prepping for Mplus====================================
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
# noquote(colnames(data_b4_analyze))
an_coln <- colnames(data_b4_analyze) %>% str_c(collapse = " ") %>% noquote()

write_file(an_coln, here("analysis/data/derived_data/analysis-colnames.txt"))


write_csv(data_b4_analyze, here("analysis/data/derived_data/data-analysis.csv"))


