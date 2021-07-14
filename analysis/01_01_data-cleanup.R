# data cleanup

here::i_am("analysis/01_01_data-cleanup.R")
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
             `Before the COVID-19 restrictions, were you employed?`                                     = "b4_emp",
             `Before the COVID-19 restrictions...`                                                      = "b4",
             `In a typical work week (before COVID-19 restrictions) I worked on a...`                   = "b4_wschd",
             `In a typical work week (before COVID-19 restrictions)…`                                   = "b4_w",
             `Please mark all modes you regularly used to travel to and from work before the COVID-19 restrictions:`
                                                                                                        = "b4_wmod",
             `Please estimate your typical home-to-work travel distance in miles before the COVID-19 restrictions by each of the modes you indicated previously:`
                                                                                                        = "b4_wdst",
             `Please estimate your typical home-to-work travel time in minutes before the COVID-19 restrictions by each of the modes you indicated previously:`
                                                                                                        = "b4_wtime",
             `Are you working now?`                                                                     = "wNow",
             `Did you change jobs as a result of COVID-19?`                                             = "wChg",
             `Are you employed in one of the following industries?`                                     = "wInd",
             `Has your workload changed since the beginning of the COVID-19 restrictions in your area?` = "wLd",
             `Please estimate the number of days in a week for each of the following questions:`        = "ndays",
             `In a typical work week currently (during COVID-19 restrictions) I work on a...`           = "du_wsch",
             `Are you a student?`                                                                       = "stu",
             `What school grade or level do you attend?`                                                = "slvl",
             `Please mark all modes you regularly used to travel to and from school before the COVID-19 restrictions:`
                                                                                                        = "b4_smod",
             `Please estimate your typical home-to-school travel distance in miles before the COVID-19 restrictions by each of the modes you indicated previously:`
                                                                                                        = "b4_sdst",
             `Please estimate your typical home-to-school travel time in minutes before the COVID-19 restrictions by each of the modes you indicated previously:`
                                                                                                        = "b4_stime",
             `How were your classes affected by COVID-19?`                                              = "sImp",
             `Did you move residences during the COVID-19 restrictions, even temporarily?`              = "mv",
             `Have you moved permanently to the residence you are/were residing in during the Stay at Home order?`
                                                                                                        = "mv_prm",
             `What influenced your decision to change residences? Select all that apply.`               = "mv_why",
             `In what ZIP code was the residence you moved from located? (enter 5-digit ZIP code; for example, 00544 or 94305)` = "mo_ZIP",
             `What city did you live in before moving? If your city is not listed, please select "Not listed".` = "mo_cit",
             `Which best describes your home before the Stay at Home order?` = "mo_htyp",
             `Indicate how many people from each age category, including yourself, permanently lived in your previous home before the COVID-19 restrictions:` = "mo_npr",
             `Including yourself, how many residents of your previous home...` = "mo_res",
             `Please indicate how many of each of the following modes were in working condition and available to your household before the COVID-19 restrictions:` = "mo_mod",
             `Indicate how many people from each age category, including yourself, lived in your residence during the COVID-19 Stay at Home order:` = "du_npr",
             `Including yourself, how many residents of your residence during the COVID-19 Stay at Home order...` = "du_res",
             `Think of your residence during the COVID-19 Stay at Home order. Before the COVID-19 restrictions, how many people lived there? Include yourself if you lived there at that time.` = "dub4_npr",
             `Please indicate how many of each of the following modes were in working condition and available to your residence during the COVID-19 Stay at Home order:` = "du_mod",
             `Please mark any means of transportation you use to get to work, school, shopping, or any other places you need to visit. Exclude things like going on a walk, "joyrides", or a recreational bicycle ride.` = "gen_mod",
             `Do you have a valid driver's license?` = "drlic",
             `On a typical week (including weekends) before COVID-19 restrictions, please estimate how many trips you make by each of the modes you indicated previously. A trip is a one-way movement from an origin to a destination.` = "b4_ntr",
             `In the past seven days, approximately how many trips did you make by each of the modes you indicated previously? A trip is a one-way movement from an origin to a destination.` = "now_ntr",
             `Do you have one or more personal cars owned and used mostly by you?` = "ownCar",
             `What is the year, make, and model of your automobile?` = "car",
             `Please respond to the following prompts` = "q",
             `What best describes your gender?` = "gender",
             `In what year were you born? (Format YYYY)` = "yrborn",
             `Annual household income last year (2019)` = "hinc",
             `Do you expect your household income to decrease because of COVID-19?` = "hinc_dec",
             `In what ZIP code is your current home located? (enter 5-digit ZIP code; for example, 00544 or 94305)` = "du_zip",
             `What city do you currently live in?` = "du_cit",
             `Age` = "age",
             `Device Type` = "device",
             `Household Income` = "hinc_sm",
             `Gender` = "gender_sm",
             `Region` = "region_sm",
             `United States Region` = "us_region_sm",
             `Respondent ID` = "pid",
             `Collector ID` = "collid",
             `Start Date` = "start",
             `End Date` = "end",
             `Custom Data 1` = "cust-data-1",
             `collector_type_source` = "coll_type_source",
             .default = "WARNING: NEEDS DEFINITION"
             )) |>
  mutate(
    short_row2 =
      recode(row2,
             .default = "WARNING: NEEDS DEFINITION",
             `How many days per week did you typically work before COVID-19 restrictions were in place?` = "wdays",
             `How many days did you work from home in a typical week?` = "wfh",
             `How many days did you conduct/participate in online meetings for work purposes in a typical week?` = "vmeet",
             `Car alone` = "dal",
             `Walking` = "wlk",
             `Transit` = "trn",
             `Car driving others` = "dot",
             `Passenger in a car` = "pas",
             `I do not travel to work` = "wfh",
             `Something else` = "oth",
             `Bicycle` = "bik",
             `I am employed in another industry (please explain)` = "oth_emp",
             `How many days per week do you typically work now?` = "wnow",
             `How many days did you work from home?` = "wfh",
             `How many days did you conduct/participate in online meetings for work purposes?` = "vmeet",
             `After COVID-19 restrictions are lifted, how often might you work from home in a week?` = "aft_wfh",
             `After COVID-19 restrictions are lifted, how often might you conduct online meetings for work purposes in a week?` = "aft_vmeet",
             `After COVID-19 restrictions are lifted, how often are you going to commute in a week?` = "aft_comm",
             `Other (please specify)` = "oth_open",
             `I did not travel to school (home school or online classes)` = "sfh",
             `My classes changed in another way (including deciding to take time off). Please specify:` = "othchnge",
             `Assist family or friends` = "assist",
             `Necessity (was already moving)` = "neces",
             `Protect family or friends` = "prot",
             `Social needs` = "soc",
             `Comfort, access to resources` = "comf",
             `Eviction` = "evct",
             `Open-Ended Response` = "op",
             `13 to 15 years old` = "13_15",
             `18 to 65 years old` = "18_65",
             `16 to 17 years old` = "16_17",
             `Under 5 years old` = "00_05",
             `5 to 12 years old` = "05_12",
             `Over 65 years old` = "65_99",
             `Are employed, either full-time or part-time?` = "emply",
             `Are enrolled in any type of school , including daycare, technical school, or university?` = "sch",
             `Motor vehicles` = "veh",
             `Bicycles` = "bik",
             `Year` = "yr",
             `Make (e.g., Ford)` = "make",
             `Model (e.g., Mustang)` = "modl",
             `I like the freedom of driving my own car` = "dfr",
             `Driving a car is a relaxing way to commute` = "drx",
             `I enjoy driving my car even in heavy traffic` = "dnj",
             `I won’t rely on another person to get to work on time` = "crl",
             `My schedule is too erratic to be in a carpool` = "csc",
             `Taking public transit does not fit my lifestyle` = "tlf",
             `Prefer to self-describe` = "slfdesc")) |>

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

