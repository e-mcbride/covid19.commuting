# build dataset for attitudes
library(tidyverse)
library(MplusAutomation)
library(here)

#import my function to write mplus data to file in the right file location
devtools::load_all()

data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-mplus-ready.rds"))

# vignette("vignette",package="MplusAutomation")

att_mplus <- data_mplus_ready %>%
  select(pid, starts_with("q_")) %>%
  data.frame()


write_mplus_data(df = att_mplus,
                 wd_for_analysis = here("analysis/03_Mplus/attitudes/"),
                 filename = "attitude-data-mplus-ready.dat",
                 writeData = "ifmissing",
                 hashfilename = TRUE)

