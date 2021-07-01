# data cleanup

here::i_am("analysis/01_02_data-cleanup.R")
library(here)
library(tidyverse)
library(tidyxl)
library(unpivotr)

# task: combine 1st and 2nd rows into column names (refer to Qatar project "Access2_AutomateTAZChars.Rmd")

# data_cells <- tidyxl::xlsx_cells(here("analysis/data/raw_data/raw-condensed-completeonly-xl.xlsx")) # testing if it works yet




#####
# Using excel
#####
data_cells <- tidyxl::xlsx_cells(here("analysis/data/raw_data/raw-condensed-completeonly-xl2.xlsx")) |>
# data_cells <- tidyxl::xlsx_cells(here("analysis/data/raw_data/raw-condensed-xl2.xlsx")) |>

  #Remove "Response" from 2nd row of column names cuz it's unnecessary
  mutate(character =
           if_else(
             condition = (row == 2 & character == "Response"),
             true = NA_character_,
             false = character
           )) |>
  #hilarious find if car model is written as "infinity" it is converted to an error in excel format .xlsx
    # Fixed below:
  mutate(character =
           if_else(data_type == "error", "Infiniti", character),
         data_type =
           if_else(data_type == "error", "character", data_type),
         ) |>
  dplyr::filter(!is_blank) |>
  select(row, col, address, is_blank, data_type, numeric, date, character)
  # filter(data_type == "date")

headers_labeled <- data_cells |>
  behead("up-left", row1) |>
  behead("up", row2) |>

  # Reorder columns back to original order
  arrange(col)


clean_question_list <- headers_labeled |>
  select(row1, row2) |>
  distinct() |>

  # Shorten the long sentence variable names
  mutate(
    short_row1 =
      recode(row1,
             `Before the COVID-19 restrictions, were you employed?` = "be4_employ",
             `Before the COVID-19 restrictions...` = "be4",
             `In a typical work week (before COVID-19 restrictions) I worked on a...` = "be4_wSched",
             `In a typical work week (before COVID-19 restrictions)…` = "be4_w",
             `Please mark all modes you regularly used to travel to and from work before the COVID-19 restrictions:` = "be4_wMode",
             `Please estimate your typical home-to-work travel distance in miles before the COVID-19 restrictions by each of the modes you indicated previously:` = "be4_wDist",
             `Please estimate your typical home-to-work travel time in minutes before the COVID-19 restrictions by each of the modes you indicated previously:` = "be4_wTime",
             `Are you working now?` = "wNow",
             `Did you change jobs as a result of COVID-19?` = "wChange",
             `Are you employed in one of the following industries?` = "wIndustry",
             `Has your workload changed since the beginning of the COVID-19 restrictions in your area?` = "wLoadChange",
             `Please estimate the number of days in a week for each of the following questions:` = "nDays",
             `In a typical work week currently (during COVID-19 restrictions) I work on a...` = "dur_wSched",
             `Are you a student?` = "stu",
             `What school grade or level do you attend?` = "sLevel",
             `Please mark all modes you regularly used to travel to and from school before the COVID-19 restrictions:` = "be4_sMode",
             `Please estimate your typical home-to-school travel distance in miles before the COVID-19 restrictions by each of the modes you indicated previously:` = "be4_sDist",
             `Please estimate your typical home-to-school travel time in minutes before the COVID-19 restrictions by each of the modes you indicated previously:` = "be4_sTime",
             `How were your classes affected by COVID-19?` = "sImpact",
             `Did you move residences during the COVID-19 restrictions, even temporarily?` = "move",
             `Have you moved permanently to the residence you are/were residing in during the Stay at Home order?` = "move_perm",
             `What influenced your decision to change residences? Select all that apply.` = "move_reason",
             `In what ZIP code was the residence you moved from located? (enter 5-digit ZIP code; for example, 00544 or 94305)` = "moveO_ZIP",
             `What city did you live in before moving? If your city is not listed, please select "Not listed".` = "moveO_city",
             `Which best describes your home before the Stay at Home order?` = "moveO_htype",
             `Indicate how many people from each age category, including yourself, permanently lived in your previous home before the COVID-19 restrictions:` = "moveO_HHnper",
             `Including yourself, how many residents of your previous home...` = "moveO_res",
             `Please indicate how many of each of the following modes were in working condition and available to your household before the COVID-19 restrictions:` = "moveO_modes",
             `Indicate how many people from each age category, including yourself, lived in your residence during the COVID-19 Stay at Home order:` = "resDur_HHnper",
             `Including yourself, how many residents of your residence during the COVID-19 Stay at Home order...` = "resDur_res",
             `Think of your residence during the COVID-19 Stay at Home order. Before the COVID-19 restrictions, how many people lived there? Include yourself if you lived there at that time.` = "resDur_be4_HHnper",
             `Please indicate how many of each of the following modes were in working condition and available to your residence during the COVID-19 Stay at Home order:` = "resDur_modes",
             `Please mark any means of transportation you use to get to work, school, shopping, or any other places you need to visit. Exclude things like going on a walk, "joyrides", or a recreational bicycle ride.` = "gen_modes",
             `Do you have a valid driver's license?` = "driverLic",
             `On a typical week (including weekends) before COVID-19 restrictions, please estimate how many trips you make by each of the modes you indicated previously. A trip is a one-way movement from an origin to a destination.` = "be4_nTrips",
             `In the past seven days, approximately how many trips did you make by each of the modes you indicated previously? A trip is a one-way movement from an origin to a destination.` = "now_nTrips",
             `Do you have one or more personal cars owned and used mostly by you?` = "ownCar",
             `What is the year, make, and model of your automobile?` = "car",
             `Please respond to the following prompts` = "prompt",
             `What best describes your gender?` = "gender",
             `In what year were you born? (Format YYYY)` = "yearBorn",
             `Annual household income last year (2019)` = "hhInc",
             `Do you expect your household income to decrease because of COVID-19?` = "hhInc_decrease",
             `In what ZIP code is your current home located? (enter 5-digit ZIP code; for example, 00544 or 94305)` = "resDur_ZIP",
             `What city do you currently live in?` = "resDur_city",
             `Age` = "age",
             `Device Type` = "device",
             `Household Income` = "hhInc_surveyMonk",
             `Gender` = "gender_surveyMonk",
             `Region` = "region_surveyMonk",
             `United States Region` = "united_states_region_surveyMonk",
             `Respondent ID` = "pid",
             `Collector ID` = "collectorid",
             `Start Date` = "startdate",
             `End Date` = "enddate",
             `Custom Data 1` = "custom-data-1",
             `collector_type_source` = "collector_type_source",
             .default = "WARNING: NEEDS DEFINITION"
             )) |>
  mutate(
    short_row2 =
      recode(row2,
             .default = "WARNING: NEEDS DEFINITION",
             `How many days per week did you typically work before COVID-19 restrictions were in place?` = "wDays",
             `How many days did you work from home in a typical week?` = "wfh",
             `How many days did you conduct/participate in online meetings for work purposes in a typical week?` = "vidMeet",
             `Car alone` = "dralone",
             `Walking` = "walk",
             `Transit` = "transit",
             `Car driving others` = "drothers",
             `Passenger in a car` = "pass",
             `I do not travel to work` = "wfh",
             `Something else` = "oth",
             `Bicycle` = "bike",
             `I am employed in another industry (please explain)` = "oth_emp",
             `How many days per week do you typically work now?` = "wDaysNow",
             `How many days did you work from home?` = "wfh",
             `How many days did you conduct/participate in online meetings for work purposes?` = "vidMeet",
             `After COVID-19 restrictions are lifted, how often might you work from home in a week?` = "aft_wfh",
             `After COVID-19 restrictions are lifted, how often might you conduct online meetings for work purposes in a week?` = "aft_vidMeet",
             `After COVID-19 restrictions are lifted, how often are you going to commute in a week?` = "aft_commute",
             `Other (please specify)` = "other_open",
             `I did not travel to school (home school or online classes)` = "schfh",
             `My classes changed in another way (including deciding to take time off). Please specify:` = "otherchange_open",
             `Assist family or friends` = "assist",
             `Necessity (was already moving)` = "necessity",
             `Protect family or friends` = "protect",
             `Social needs` = "social",
             `Comfort, access to resources` = "comfort",
             `Eviction` = "evict",
             `Open-Ended Response` = "open",
             `13 to 15 years old` = "13_15",
             `18 to 65 years old` = "18_65",
             `16 to 17 years old` = "16_17",
             `Under 5 years old` = "00_05",
             `5 to 12 years old` = "05_12",
             `Over 65 years old` = "65_99",
             `Are employed, either full-time or part-time?` = "employed",
             `Are enrolled in any type of school , including daycare, technical school, or university?` = "school",
             `Motor vehicles` = "vehicles",
             `Bicycles` = "bikes",
             `Year` = "year",
             `Make (e.g., Ford)` = "make",
             `Model (e.g., Mustang)` = "model",
             `I like the freedom of driving my own car` = "drive_freedom",
             `Driving a car is a relaxing way to commute` = "drive_relax",
             `I enjoy driving my car even in heavy traffic` = "drive_enjoy",
             `I won’t rely on another person to get to work on time` = "carpool_rely",
             `My schedule is too erratic to be in a carpool` = "carpool_schedule",
             `Taking public transit does not fit my lifestyle` = "transit_lifestyle",
             `Prefer to self-describe` = "self-describe")) |>

  # Merge the two variable name rows to make unique variable names for each column
  mutate(varnames =
           str_c(str_replace_na(short_row1), str_replace_na(short_row2), sep = "_") |>
           str_replace_all("_NA", replacement = "")) |>
  mutate(varnames = janitor::make_clean_names(varnames))

readr::write_rds(clean_question_list, here("analysis/data/derived_data/clean-question-list.rds"))

#####
# Joining new variable names to beheaded dataset
#####

data_newnames <- headers_labeled |>

  # join new short header labels to table, remove old ones
  left_join(clean_question_list, by = c("row1", "row2")) |>
  select(-row1, -row2, -short_row1, -short_row2) |>

  # remove unnecessary rows (IMPORTANT to include `row` as a unique identifier)
  # also important to remove unnecessary rows (like `col`) to ensure `spatter` works as intended
  select(row, data_type, numeric, date, character, varnames) |>
  spatter(key = varnames) |>
  select(-row) |>
  # select(pid, collectorid, everything()) |>
  janitor::clean_names() |>
  select(clean_question_list$varnames)

# data_newnames
# colnames(data_newnames)
# y <- data_newnames |> select(clean_question_list$varnames)
# x <- data_newnames[,clean_question_list$varnames]

readr::write_rds(data_newnames, here("analysis/data/derived_data/data-newnames-completeonly.rds"))
# readr::write_rds(data_newnames, here("analysis/data/derived_data/data-newnames.rds"))

