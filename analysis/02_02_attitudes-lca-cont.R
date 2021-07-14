# 02_02 Analyze latent class-type model with Likert scale as continuous for the attitudes

here::i_am("analysis/02_02_attitudes-lca-cont.R")
library(here)
library(tidyverse)
# library(tidymodels)

dictionary <- read_rds("analysis/data/derived_data/clean-question-list.rds")
data_elim <- read_rds("analysis/data/derived_data/data-good-cases.rds")


data_likert_cont <- data_elim %>%
  select(pid,
         # attitude
         starts_with("q")
  ) %>%
  # left_join(abbr_mode_totals, by = "pid") %>%

  # Dummy Variables (so far, everything "character" is a dummy variable)
  mutate(across(where(is.character), forcats::as_factor)) %>%
  # mutate(hinc = fct_relevel(hinc, c("$0",
  #                                   "Under $9,999",
  #                                   "$10,000 to $24,999",
  #                                   "$25,000 to $34,999",
  #                                   "$35,000 to $49,999",
  #                                   "$50,000 to $74,999",
  #                                   "$75,000 to $99,999",
  #                                   "$100,000 to $149,999",
  #                                   "$150,000 to $199,999",
  #                                   "$200,000 to $249,999",
  #                                   "$250,000 or more"))) %>%
  mutate(across(starts_with("q_"), fct_relevel, c("Strongly Disagree",
                                                  "Disagree",
                                                  "Neither Agree nor Disagree",
                                                  "Agree",
                                                  "Strongly Agree"))) %>%
  mutate(across(where(is.factor), as.numeric, .names = "score_{.col}")) %>%
  select(-(where(is.factor))) %>%
  mutate(across(starts_with("score_"), ordered)) %>%
  rename_with(~gsub("score_", "", .x, fixed = TRUE))

an_coln <- colnames(data_likert_cont) %>% str_c(collapse = " ") %>% noquote()
an_coln

write_file(an_coln, here("analysis/data/derived_data/analysis-colnames.txt"))


write_csv(data_likert_cont, na = "-9999", col_names = FALSE, here("analysis/03_Mplus/data-likert-cont.csv"))
