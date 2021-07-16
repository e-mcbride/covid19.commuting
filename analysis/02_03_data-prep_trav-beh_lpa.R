# 02_03 Data prep: mplus travel behavior LPA

library(tidyverse)

here::here("analysis/02_03_data-prep_trav-beh_lpa.R")
library(here)

data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-mplus-ready.rds"))

data_travbeh <- data_mplus_ready

# save file to `analysis` folder bc that means mplus script can find it with no PATH necessary
write_csv(data_travbeh, na = "-9999", col_names = FALSE, here("analysis/03_Mplus/b4-data-analysis_att-cont.csv"))
