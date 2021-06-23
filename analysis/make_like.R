# Make-like file
here::i_am("analysis/make_like.R")

# take raw data, write to RDS
source(here("analysis/01_01_write-raw-data-to-rds.R"))

#
source(here("analysis/01_02_data-cleanup.R"))


source(here("analysis/01_03_remove-bad-cases.R"))

