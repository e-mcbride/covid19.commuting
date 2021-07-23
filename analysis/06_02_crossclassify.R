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

# pull class label for select model
classes_t4 <- time4$savedata %>% select(pid = PID, time_c= C)
classes_a5 <- att5$savedata %>% select(pid = PID, att_c = C)

# pids of the small classes
smallclasspids <- classes_t4 %>%
  filter(time_c == 2 | time_c == 3) %>%
  pull(pid)

# Cross classify, chi sq test =============================
bothclass <- classes_t4 %>% left_join(classes_a5)

xc <- janitor::tabyl(bothclass, att_c, time_c, show_na = F)

xc

janitor::chisq.test(xc, tabyl_results = TRUE)


#
# Later: Check how much the travel behavior model categories line up --------
