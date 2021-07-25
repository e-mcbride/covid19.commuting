# More elimination
# Will combine with the other data cleaning script if/when the model is run

library(tidyverse)
library(here)
library(janitor)


data_elim <- read_rds("analysis/data/derived_data/data-good-cases.rds")
data_comp <- read_rds("analysis/data/derived_data/data-newnames-completeonly.rds")
dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds")

# use janitor::get_dupes() ==========================

dupes_pid <- data_elim %>%
  # filter "No" for b4_emp or stu cuz ppl who said "No" to both will all have the same
  filter(stu != "No" | b4_emp != "No") %>%
  get_dupes(starts_with("b4")) %>%
  pull(pid)

dupes2_pid <- data_elim %>%
  select(pid, w_now:du_wsch) %>%
  filter(w_now != "No, I'm not working") %>%
  get_dupes(-pid) %>%
  pull(pid)


# age ================
age_pid <- data_elim %>%
  filter(age == "< 18") %>%
  pull(pid)

age2_pid <- data_comp %>%
  mutate(agecalc = 2020 - yrborn_op) %>%
  filter(agecalc < 18) %>%
  pull(pid)


# if walking/biking speed is faster than x mph ==============================
# walk: 6 (to give some leeway)
# bike: 30

mph_bad2 <- data_elim %>%
  select(pid, matches("b4_.dst"), matches("b4_.time")) %>%
  # pivot_longer(cols = contains("dst"), names_to = "dst_type")
  pivot_longer(!pid,
               names_to = c("w_s", "d_t", "mode"),
               names_pattern = c("b4_?(.)(.*)_(.*)")
  ) %>%
  pivot_wider(names_from = d_t, values_from = value) %>%
  group_by(pid, w_s, mode) %>%
  mutate(mph = (dst/time)*60) %>%
  # filter(mph < 1 | mph > 80) %>%
  mutate(biketoofast = if_else((mode == "bik") & (mph > 30), true = TRUE, false = FALSE)) %>%
  mutate(walktoofast = if_else((mode == "wlk") & (mph > 6), true = TRUE, false = FALSE)) %>%
  filter(biketoofast | walktoofast) %>%
  pull(pid) %>%
  unique()


# time to get to work is longer than 3 hours
toolong_pid <- data_elim %>%
  select(pid, matches("b4_.time")) %>%
  filter(if_any(-pid, ~ .x > 180)) %>%
  pull(pid)

# manually chosen/found to be bad ===========================
manual <- c(11598150120, 11607464996, 11605132339, 11598947615, 11598919471, 11617002199, 11616864430)


# Build the list ###################################################################
pid_list <- c(dupes_pid, dupes2_pid, age_pid, age2_pid, mph_bad2, toolong_pid, manual) %>% unique()

dupers <- data_elim %>%
  filter(pid %in% pid_list)



# WRITE FILE ###################################################################

nondupers <- data_elim %>%
  filter(!(pid %in% pid_list))

nonduper_pids <- nondupers %>%
  pull(pid)




write_rds(nondupers, "analysis/data/derived_data/data-good-cases2.rds")

