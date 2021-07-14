# Make-like file
here::i_am("analysis/make_like.R")
library(here)

# take raw data, clean up variable names and such
source(here("analysis/01_01_data-cleanup.R"))

rm(list = ls())

# Remove bad responses
source(here("analysis/01_02_remove-bad-cases.R"))
rm(list = ls())


# Build variables for analysis with Mplus
source(here("analysis/02_01_vars-for-analysis.R"))

