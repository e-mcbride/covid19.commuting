# mode models with cleaner data

# remove the no-work and no-school ppl and run models w/o them
library(tidyverse)
library(here)
library(MplusAutomation)
library(janitor)
devtools::load_all()



# Extract pids of ppl who were not working or in school before pandemic
data_elim <- read_rds("analysis/data/derived_data/data-good-cases2.rds")

pids_no_WS <- data_elim %>%
  filter(b4_emp == "No" & stu == "No") %>%
  pull(pid)



# Build new dataset without them ========

# this is a new dataset where eco has been separated into train and bike/walk
# data_mplus_ready <- read_rds(here("analysis/data/derived_data/data_4mode-mplus-ready2.rds"))

data_mplus_ready <- read_rds(here("analysis/data/derived_data/data-modebinary.rds")) #EM trying binary modes

# take mplus-ready data, filter out the non-working or schooling ppl
modeWS_mplus <- data_mplus_ready %>%
  # select(pid, starts_with("time")) %>% # EM commented BC trying binary modes
  filter(!(pid %in% pids_no_WS)) %>%
  # filter(!(pid %in% smallclasspids)) %>%
  data.frame()



write_mplus_data(df = modeWS_mplus,
                 wd_for_analysis = here("analysis/03_Mplus/trav-beh/modeUsed/"),
                 filename = "modeWS-data-mplus-ready2.dat",
                 writeData = "ifmissing",
                 hashfilename = TRUE)

# create models from the template ==============================
# createModels(templatefile = here("analysis/03_Mplus/trav-beh/modeUsed/lpa_modeUsed_template.txt"))

# Run models ================================================================
# runModels(here("analysis/03_Mplus/trav-beh/modeUsed/"))

# Analyze travel mode models, update and re-run ===================


allOut_mode <- readModels(
  here("analysis/03_Mplus/trav-beh/modeUsed/"),
  recursive = FALSE)

# Get the table of values =====
tmode_outs <- allOut_mode %>%
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


# run after getting some BLRT and VLMRT results
fitstats <- tmode_outs %>%
  select(-name, -LLRepTbl, -summaries, -llnreps, -optseed, -seedused, -t11_km1ll) %>%
  unnest(cols = c(nclasses, Loglikelihood, BIC, ABIC, BLRT_pval, VLMRT_pval,
                  Entropy)) # %>%
  # filter(nclasses < 6) EM: trying binary

write_csv(fitstats, here("analysis/figures/fitstats_LCA_mode.csv"))


ggplot(tmode_outs, aes(x = as.numeric(nclasses))) +
  geom_line(aes(y = as.numeric(ABIC), color = "red")) +
  geom_line(aes(y = as.numeric(BIC), color = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("ABIC", "BIC"))
