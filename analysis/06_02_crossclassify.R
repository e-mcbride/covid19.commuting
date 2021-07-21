# 06_02 Crosstabs

# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
library(janitor)
devtools::load_all()

allOut_time <- readModels(
  here("analysis/03_Mplus/trav-beh/time/"),
  recursive = FALSE)

allOut_att <- readModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive = FALSE)

# Extract selected models ===============================
time4 <- allOut_time$X4.class_lpa_time.out
att5 <- allOut_att$X5.class_lpa_att.out

classes_t4 <- time4$savedata %>% select(pid = PID, time_c= C)
classes_a5 <- att5$savedata %>% select(pid = PID, att_c = C)

bothclass <- classes_t4 %>% left_join(classes_a5)

xc <- janitor::tabyl(bothclass, att_c, time_c, show_na = F)

xc

janitor::chisq.test(xc, tabyl_results = TRUE)

# classes_tt4 <- readModels(here("analysis/03_Mplus/trav-beh/time/4-class_lpa_time.out"), what="savedata")

# classes_att5 <- readModels(here("analysis/03_Mplus/attitudes/5-class_lpa_att.out"), what="savedata")

# Later: Check how much the travel behavior model categories line up --------
