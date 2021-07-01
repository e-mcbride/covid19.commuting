# 01_92 Exploratory for data cleanup

# Imports -----------------------------------------------------------------
here::i_am("analysis/01_03_remove-bad-cases.R")
library(here)
library(tidyverse)

# Compare pids in "complete only" vs "incomplete included" ----------------
data_all <- readr::read_rds(here("analysis/data/derived_data/data-newnames.rds"))
data_completeonly <- readr::read_rds(here("analysis/data/derived_data/data-newnames-completeonly.rds"))

inc_ids<- data_all$pid[!(data_all$pid %in% data_completeonly$pid)]

# Find out why the "incompletes" were removed OR just remove them and move on for speed
data_incomplete <-  data_all |> filter(pid %in% inc_ids)
# They stopped before finishing. Remove them
