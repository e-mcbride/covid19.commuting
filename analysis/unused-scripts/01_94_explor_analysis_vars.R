# Exploratory analysis to select variables for analysis

# Imports -----------------------------------------------------------------
here::i_am("analysis/01_94_explor_analysis_vars.R")
library(here)
library(tidyverse)



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
