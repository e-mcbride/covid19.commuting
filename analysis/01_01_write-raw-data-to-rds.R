# Raw data import and saving as .rds file

here::i_am("analysis/01_01_raw-data-import-to-rds.R")
library(here)

# melt_csv imports data where each cell is given its own row with columns of metadata
readr::melt_csv(here("analysis/data/raw_data/raw-covid19-survey.csv")) |>

  # write file as .rds for faster import in future
  readr::write_rds(here("analysis/data/derived_data/raw-data-cells.rds"))


readr::read_csv(here("analysis/data/raw_data/raw-covid19-survey.csv")) |>
  readr::write_rds(here("analysis/data/derived_data/raw-data.rds"))


