# 06_02 Crosstabs

# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
library(janitor)
devtools::load_all()

allOut_mode <- readModels(
  here("analysis/03_Mplus/trav-beh/modeUsed/"),
  recursive = FALSE)

allOut_att <- readModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive = FALSE)

# Extract selected models ===============================
mode5 <- allOut_mode$X5.class_lca_modeused_elim.out
att5 <- allOut_att$X5.class_lpa_att.out

counts_m5 <- mode5$class_counts$mostLikely
counts_a5 <- att5$class_counts$mostLikely

# pull class label for select model
classes_m5 <- mode5$savedata %>% select(pid = PID, mode_c= C)
classes_a5 <- att5$savedata %>% select(pid = PID, att_c = C)


# Cross classify, chi sq test =============================
bothclass <- classes_a5 %>%
  left_join(classes_m5, by = "pid") %>%
  # fill in NAs with #6
  #     (class for ppl who were not included in mode model
  #       bc they don't work or go to school)
  mutate(mode_c = if_else(is.na(mode_c), true = 6, false = mode_c))

xc <- janitor::tabyl(bothclass, mode_c, att_c, show_na = F)

xc %>% adorn_percentages() %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

janitor::chisq.test(xc, tabyl_results = TRUE)


dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds")

# largest group: mode_c = 5, att_c = 5
# get pids
solit_lov <- bothclass %>%
  filter(mode_c == 5 & att_c == 5) %>%
  pull(pid)

data_elim <- read_rds("analysis/data/derived_data/data-good-cases2.rds") %>%
  left_join(bothclass, by = "pid")
elim_pids <- data_elim %>%
  pull(pid)
data_comp <- read_rds("analysis/data/derived_data/data-newnames-completeonly.rds") %>%
  filter(pid %in% elim_pids) %>%
  left_join(bothclass, by = "pid")

mplsready <- read_rds("analysis/data/derived_data/data-mplus-ready.rds")

data_elim %>% select(age) %>% unique()

data_sumready <- data_elim %>%
  mutate_if(is.character, factor) %>%
  mutate(age = factor(age, levels = c("18-29", "30-44", "45-60", "> 60")),
         mode_c = factor(mode_c),
         att_c = factor(att_c))
# %>%
#   select(pid, b4_wdays, b4_wfh, b4_wschd, b4_w, b4_wdays, ndays_wnow, stu, slvl, mv, drlic, gender, age, mode_c, att_c)

data_sumready %>% select(-pid) %>% summary()

sumstats %>% View()

# summary stats
library(gtsummary)





varlist <- c('b4_wdays', 'b4_wfh', 'b4_wschd', 'b4_w', 'b4_wdays', 'ndays_wnow', 'stu', 'slvl', 'mv', 'drlic', 'gender', 'age', 'mode_c', 'att_c', 'ndays_wfh')

# get the full descriptions
dictionary %>% filter(varnames %in% varlist) %>% pull(row1)
dictionary %>% filter(varnames %in% varlist) %>%
  select(varnames, row1, row2)

# pull(row1, row2)


lablist <- list(b4_wdays  =  "How many days per week did you typically work before COVID-19 restrictions were in place?",
                b4_wfh  =  "Before the COVID-19 restrictions how many days did you work from home in a typical week?",
                b4_wschd  =  "In a typical work week (before COVID-19 restrictions) I worked on a..." ,
                b4_w  =  "In a typical work week (before COVID-19 restrictions)…",
                ndays_wnow  =  "How many days per week do you typically work now?",
                stu  =   "Are you a student?",
                slvl  =  "What school grade or level do you attend?" ,
                mv  =   "Did you move residences during the COVID-19 restrictions, even temporarily?",
                drlic  =  "Do you have a valid driver's license?",
                gender  =  "What best describes your gender?",
                age  =  "Age",
                mode_c  =  "Latent classes for travel mode",
                att_c  =  "Latent classes for attitudes",
                ndays_wfh = "How many days do you work from home now?")

labform <- list(b4_wdays  ~  "How many days per week did you typically work before COVID-19 restrictions were in place?",
                b4_wfh  ~  "Before the COVID-19 restrictions how many days did you work from home in a typical week?",
                b4_wschd  ~  "In a typical work week (before COVID-19 restrictions) I worked on a..." ,
                b4_w  ~  "In a typical work week (before COVID-19 restrictions)…",
                ndays_wnow  ~  "How many days per week do you typically work now?",
                stu  ~   "Are you a student?",
                slvl  ~  "What school grade or level do you attend?" ,
                mv  ~   "Did you move residences during the COVID-19 restrictions, even temporarily?",
                drlic  ~  "Do you have a valid driver's license?",
                gender  ~  "What best describes your gender?",
                age  ~  "Age",
                mode_c  ~  "Latent classes for travel mode",
                att_c  ~  "Latent classes for attitudes")



# factor
facvars <- c('b4_wschd', 'b4_w', 'stu', 'slvl', 'gender', 'age', 'mv', 'drlic')
sumstats_fac <- data_sumready %>%
  select(# mode_c, att_c,
    all_of(facvars)) %>%
  tbl_summary(label = lablist[facvars])

sumstats_fac


sumstats_fac %>% as_flex_table() %>%
  flextable::save_as_docx(path = "analysis/figures/sumstats_fac.docx")


# count/continous

contvars <- c('b4_wdays', 'b4_wfh', 'b4_wdays', 'ndays_wnow')

sumstats_num <- data_sumready %>%
  select(# mode_c, att_c,
    all_of(contvars)) %>%
    # b4_wdays, b4_wfh, b4_wdays, ndays_wnow, stu, slvl, mv, drlic) %>%
  tbl_summary(label = lablist[contvars])

sumstats_num

sumstats_num %>% as_flex_table() %>%
  flextable::save_as_docx(path = "analysis/figures/sumstats_num.docx")

# allvars
shorterlist <- list(
  b4_wschd  =  "In a typical work week (before COVID-19 restrictions) I worked on a..." ,
  b4_w  =  "In a typical work week (before COVID-19 restrictions)…",
  b4_wfh  =  "Before the COVID-19 restrictions how many days did you work from home in a typical week?",
  ndays_wfh = "How many days do you work from home now?",
  stu  =   "Are you a student?",
  slvl  =  "What school grade or level do you attend?" ,
  mv  =   "Did you move residences during the COVID-19 restrictions, even temporarily?",
  drlic  =  "Do you have a valid driver's license?",
  gender  =  "What best describes your gender?",
  age  =  "Age",
  mode_c  =  "Latent classes for travel mode",
  att_c  =  "Latent classes for attitudes"
)


allvars <- names(shorterlist)

sumstats_all <- data_sumready %>%
  select(# mode_c, att_c,
    all_of(allvars)) %>%
  # b4_wdays, b4_wfh, b4_wdays, ndays_wnow, stu, slvl, mv, drlic) %>%
  tbl_summary(label = lablist)

sumstats_all

sumstats_all %>% as_flex_table() %>%
  flextable::save_as_docx(path = "analysis/figures/sumstats_all.docx")

#
dictionary %>% filter(short_row1 == "mv_why" | short_row1 == "mv_prm") %>%
  select(varnames, row1, row2)

movelist <- list(mv_prm = "Have you moved permanently to the residence you are/were residing in during the Stay at Home order?",
                 mv_why_assist = "What influenced your decision to change residences? Select all that apply... Assist family or friends",
                 mv_why_prot = "What influenced your decision to change residences? Select all that apply... Protect family or friends",
                 mv_why_soc = "What influenced your decision to change residences? Select all that apply... Social needs",
                 mv_why_comf = "What influenced your decision to change residences? Select all that apply...Comfort, access to resources",
                 mv_why_evct = "What influenced your decision to change residences? Select all that apply... Eviction",
                 mv_why_neces = "What influenced your decision to change residences? Select all that apply...Necessity (was already moving)"
                 # mv_why_oth_open = "What influenced your decision to change residences? Select all that apply... Other (please specify)"

)

movevars <- names(movelist)


sumstats_move <- data_sumready %>%
  filter(mv == "Yes") %>%
  select(all_of(movevars)) %>%
  tbl_summary(label = movelist, missing = "no" )

sumstats_move  %>% as_flex_table() %>%
  flextable::save_as_docx(path = "analysis/figures/sumstats_move.docx")



# Later: Check how much the travel behavior model categories line up --------
