# Raw data import and saving as .rds file

here::i_am("analysis/01_01_write-raw-data-to-rds.R")
library(here)

csv_to_rds <- function(csvin, rdsout) {
  readr::read_csv(csvin) |>
    readr::write_rds(rdsout)
}


# melt_csv imports data where each cell is given its own row with columns of metadata
readr::melt_csv(here("analysis/data/raw_data/raw-covid19-survey.csv")) |>
  # write file as .rds for faster import in future
  readr::write_rds(here("analysis/data/derived_data/raw-data-cells.rds"))


readr::read_csv(here("analysis/data/raw_data/raw-covid19-survey.csv")) |>
  readr::write_rds(here("analysis/data/derived_data/raw-data.rds"))



readr::read_csv(here("analysis/data/raw_data/raw-covid19-survey-expanded.csv"), col_names = FALSE) |>
  readr::write_rds(here("analysis/data/derived_data/raw-data-expanded.rds"))

readr::melt_csv(here("analysis/data/raw_data/raw-covid19-survey-expanded.csv")) |>
  readr::write_rds(here("analysis/data/derived_data/raw-data-expanded-cells.rds"))


