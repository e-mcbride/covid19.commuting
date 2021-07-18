# Running Mplus model using MplusAutomation
library(tidyverse)
library(MplusAutomation)
library(here)
# here::here("analysis/03_01_mplus-travbeh-time.R")

#import my function to write mplus data to file in the right file location
source("R/write_mplus_data.R")

data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-mplus-ready.rds"))

# vignette("vignette",package="MplusAutomation")

time_mplus <- data_mplus_ready %>%
  select(pid, starts_with("time")) %>%
  data.frame()


write_mplus_data(df = time_mplus,
                 wd_for_analysis = here("analysis/03_Mplus/trav-beh/time/"),
                 filename = "time-data-mplus-ready.dat",
                 writeData = "ifmissing",
                 hashfilename = TRUE)

createModels(templatefile = here("analysis/03_Mplus/trav-beh/time/LPA-time-template.txt"))

# optimal seeds for each model (number is the number of classes)
optseed1 <- 350608


# runModels(
#   here("analysis/03_Mplus/trav-beh/time/6-class_LPA_time.inp"),
#   recursive=FALSE,
#   replaceOutfile="modifiedDate")



allOut <- readModels(
  here("analysis/03_Mplus/trav-beh/time/"),
  recursive = TRUE,
  replaceOutFile="modifiedDate")

