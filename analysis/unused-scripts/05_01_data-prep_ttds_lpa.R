# build dataset for travel time and distance combo
library(tidyverse)
library(MplusAutomation)
library(here)

#import my function to write mplus data to file in the right file location
devtools::load_all()

data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-mplus-ready.rds"))

# vignette("vignette",package="MplusAutomation")

ttds_mplus <- data_mplus_ready %>%
  select(pid, starts_with("time"),starts_with("dst") ) %>%
  data.frame()


write_mplus_data(df = ttds_mplus,
                 wd_for_analysis = here("analysis/03_Mplus/trav-beh/time_dist/"),
                 filename = "ttds-data-mplus-ready.dat",
                 writeData = "ifmissing",
                 hashfilename = TRUE)

