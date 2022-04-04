# remove the weirdos and run models w/o them
library(tidyverse)
library(here)
library(MplusAutomation)
library(janitor)
devtools::load_all()

allOut_time <- readModels(
  here("analysis/Mplus/trav-beh/time/"),
  recursive = FALSE)


# Extract selected models ===============================
time4 <- allOut_time$X4.class_lpa_time.out


# pull class label for select model
classes_t4 <- time4$savedata %>% select(pid = PID, time_c= C)



# pids of the small classes
smallclasspids <- classes_t4 %>%
  filter(time_c == 2 | time_c == 3) %>%
  pull(pid)


# Build new dataset without them ========

data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-mplus-ready.rds"))

# vignette("vignette",package="MplusAutomation")

timeb_mplus <- data_mplus_ready %>%
  select(pid, starts_with("time")) %>%
  mutate(across(-pid, ~ if_else(.x < 1, true = NA_real_, false = .x) )) %>%
  # filter(!(pid %in% smallclasspids)) %>%
  data.frame()



write_mplus_data(df = timeb_mplus,
                 wd_for_analysis = here("analysis/Mplus/trav-beh/time2/"),
                 filename = "timeb-data-mplus-ready.dat",
                 writeData = "ifmissing",
                 hashfilename = TRUE)

# Create travel time B models =============

#
# createModels(templatefile = here("analysis/Mplus/trav-beh/time2/timeb_lpa-template.txt"))#, recursive = F)
#
# # below runModles is commented out because
# # ERROR when using the following method in `runModels()`:
# #     replaceOutfile="modifiedDate"
#
# runModels(
#   here("analysis/Mplus/trav-beh/time2/"),
#   recursive=F)
#


# Analyze travel time models, update and re-run ===================
# Imports =====

allOut_t2 <- readModels(
  here("analysis/Mplus/trav-beh/time2/"),
  recursive = FALSE)

# Get the table of values =====
tt2_outs <- allOut_t2 %>%
  enframe() %>%
  transmute(name,
            LLRepTbl = map(value, LLrep_to_table),
            summaries = map(value, ~ .x$summaries),
            nclasses = map(summaries, "NLatentClasses"),
            Loglikelihood = map(summaries, "LL"),
            BIC = map(summaries, "BIC"),
            ABIC = map(summaries, "aBIC"),
            BLRT_pval = map(summaries, "BLRT_PValue"),
            VLMRT_pval = map(summaries, "T11_VLMR_PValue"),
            Entropy = map(summaries, "Entropy"),
            llnreps = map(value, LLreplication),
            optseed = map(LLRepTbl,
                          ~ .x %>% slice(1) %>% pull(seed)),
            seedused = map(value, ~ .x$input$analysis$optseed),
            t11_km1ll = map(summaries, "T11_KM1LL")
  )

ggplot(tt2_outs, aes(x = as.numeric(nclasses))) +
  geom_line(aes(y = as.numeric(ABIC), color = "red")) +
  geom_line(aes(y = as.numeric(BIC), color = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("ABIC", "BIC"))

fitstats <- tt2_outs %>%
  select(-name, -LLRepTbl, -summaries, -llnreps, -optseed, -seedused, -t11_km1ll) %>%
  unnest(cols = c(nclasses, Loglikelihood, BIC, ABIC, BLRT_pval, VLMRT_pval,
                  Entropy))
