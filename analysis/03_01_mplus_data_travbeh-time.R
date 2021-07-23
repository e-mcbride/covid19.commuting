# Running Mplus model using MplusAutomation
library(tidyverse)
library(MplusAutomation)
library(here)

#import my function to write mplus data to file in the right file location
devtools::load_all()

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



# box plots
bxplt_dt <- time_mplus %>%
  pivot_longer(cols = -pid, names_to = "mode")

ggplot(bxplt_dt, aes(x = mode, y = value)) +
  # geom_boxplot()
  geom_violin()

ggplot(time_mplus) + geom_bar(aes(x = time_dal))
