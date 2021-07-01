### Remove cases that are incomplete or unusable for whatever reason

# Imports -----------------------------------------------------------------
here::i_am("analysis/01_03_remove-bad-cases.R")
library(here)
library(tidyverse)

dictionary <- read_rds("analysis/data/derived_data/clean-question-list.rds")


# Remove bad cases --------------------------------------------------------
data_newnames <- read_rds("analysis/data/derived_data/data-newnames-completeonly.rds")


# find people who cheated the system


## what did people put for the free-answer questions -----------------------
data_newnames |> select(car_year) |> unique() |> View()
data_newnames |> filter(car_year == "mbnvnv") |> View()

data_newnames |>
  mutate(car_make = str_to_lower(car_make)) |>
  select(car_make) |>
  unique()
  # ggplot(aes(car_make)) + geom_bar()

ggplot(data_newnames, aes(car_make)) + geom_bar()


data_newnames |> select(car_make, car_model) |> View()

# check incomes against each other ----------------------------------------


# check age/year born against each other ----------------------------------




# check genders against each other ----------------------------------------
bad_gen <- data_newnames |> select(pid, gender, gender_survey_monk) |>
  filter(!(gender %in% "Prefer not to say")) |>
  filter(gender != gender_survey_monk) |>
  pull(pid)

x <- data_newnames |>
  filter(pid %in% bad_gen)

data_newnames

data_newnames |> filter(!is.na(gender_self_describe)) |> View()

# a person only put the number 3 in for every possible place, put disagree for every prompt (pid = 11598634657)


# select ppl who put the same answer for every `prompt_` -----

col_comb <- data_newnames %>% select(starts_with("prompt")) %>% colnames() %>%  combn(2, simplify = FALSE)



# list of true/false: is this

prompt_cheat_ids <-
  # map_dfr(prompt_comb, ~ list(pid = two_case[["pid"]], compair = two_case[.x[1]] == two_case[.x[2]])) %>%
  map_dfr(col_comb, ~ list(pid = data_newnames[["pid"]], compair = data_newnames[.x[1]] == data_newnames[.x[2]])) %>%
  # map_dfr(col_comb, ~ list(pid = data_newnames[["pid"]], compair = data_newnames[.x[1]] )) %>%
  # map_dfr(col_comb, ~ list(pid = data_newnames[["pid"]], compair = str_detect(data_newnames[.x[1]], data_newnames[.x[2]]))) %>%
  group_by(pid) %>%
  summarise(compare_all = sum(compair)) %>%
  mutate(allsame = compare_all == 15) %>%
  filter(allsame) %>%
  pull(pid)

data_short <- data_newnames |>
  select(-collectorid, -startdate, -enddate, -custom_data_1, -collector_type_source, -device, -contains("_mode_"),
         -region_survey_monk, -united_states_region_survey_monk)

prompt_cheat_data <- data_short %>% filter(pid %in% prompt_cheat_ids) %>% slice(19:length(.))





# Getting the string distances to attempt to find duplicates -----

# try with this list of makes/models from Hadley
# devtools::install_github("hadley/fueleconomy")
# library(tidystringdist)
#
# veh_makelist <- fueleconomy::common %>% pull(make) %>% unique()
#
# tidy_comb(data_newnames, veh_makelist, car_make)
#

# I could combine a bunch of columns into a single string first...
# library(tidystringdist)
library(stringdist)

# try with this list of makes/models from Hadley
# devtools::install_github("hadley/fueleconomy")
# library(tidystringdist)
#
veh_makelist <- fueleconomy::common %>% pull(make) %>% unique() %>% str_to_upper()



# code from https://amunategui.github.io/stringdist/

unq_make <- data_newnames %>%
  select(car_make) %>%
  mutate(car_make = str_to_upper(car_make) %>% str_trim(side = "both")) %>%
  filter(!is.na(car_make)) %>%
  pull(car_make) %>%
  unique()

distmake <- stringdistmatrix(unq_make, unq_make, method = "jw")

# distmake <- stringdistmatrix(unq_make, veh_makelist, method = "jw")

rownames(distmake) <- unq_make

colnames(distmake) <- veh_makelist


hc <- hclust(as.dist(distmake))

plot(hc)
rect.hclust(hc,k = 50)

dfClust <- data.frame(unq_make, cutree(hc, k = 50))
names(dfClust) <- c('modelname','cluster')

print(paste('Average number of models per cluster:', mean(table(dfClust$cluster))))

# order the data by cluster size
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','modelname')
head(dfClust[c('cluster','modelname')],50)


# -----


veh_strdist <- data_newnames %>%
  mutate(car_make = str_to_upper(car_make) %>% str_trim(side = "both")) %>%
  tidy_comb_all(car_make) %>%
  tidy_stringdist(method = "jw")




vh_dist <- as.dist(veh_strdist)


veh_repl <- veh_strdist %>%

  #  base the replacement on the number of occurrences
  left_join(make_freq, by = c("V1" = "car_make")) %>%
  rename(n_cases_v1 = n_cases) %>%
  left_join(make_freq, by = c("V2" = "car_make")) %>%
  rename(n_cases_v2 = n_cases) %>%

  # remove honda/hyundai for now
  filter((str_detect(V1, "^H") & str_detect(V2, "^H"))) %>%

  #BEFORE using soundex, gotta take care of honda/hyundai
  mutate(repl_v1 = if_else(
    condition = jw > 0.16 & !soundex,
    true = if_else(n_cases_v1 >= n_cases_v2,
                   true = V1,
                   false = V2),
    false = V1
    )) %>%
  filter(!soundex)


  mutate(soundex_repl = if_else(soundex == 0, V1, V2))

# maybe base the replacement on the number of occurrences


make_freq <- data_newnames %>%
  select(car_make) %>%
  mutate(car_make = car_make %>%
           str_trim() %>%
           str_to_upper()) %>%
  group_by(car_make) %>%
  summarize(n_cases = n())

# -----


data_newnames %>%
  mutate(same = str_detect(prompt_drive_freedom, prompt_drive_relax)) %>%
  mutate(if_else(
    condition = str_detect(prompt_drive_freedom, prompt_drive_relax),
    false = FALSE,
    true =
  ))
  select(pid, prompt_drive_freedom, prompt_drive_relax, same) %>%
  filter(!same) %>%
  # select(pid, starts_with("prompt"), same) %>%
  View()

head(data_newnames$prompt_drive_enjoy)
head(data_newnames$prompt_drive_freedom)

str_detect(data_newnames$prompt_drive_enjoy, data_newnames$prompt_drive_freedom)

# To get duplicate cases, try selecting only character columns? -----

# Numeric columns contain numeric responses? -----

# check ZIPS and cities against each other --------------------------------



# Check ppl who put same prompt response for all --------------------------



# Build final dataset -----------------------------------------------------

data_rmcases <- data_newnames |>
  # obvious ones
  filter(car_make == "jhgfjhgfd")

# Write cleaned data ------------------------------------------------------

write_rds(data_cases, "analysis/data/derived_data/01_03_data-good-cases.rds")



