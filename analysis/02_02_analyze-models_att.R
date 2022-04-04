# Analyze attitude, update and re-run
# Imports =====
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

allOut_att <- readModels(
  here("analysis/Mplus/attitudes/"),
  recursive = FALSE)

# Get the table of values =====
tt_outs_att <- allOut_att %>%
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

ggplot(tt_outs_att, aes(x = as.numeric(nclasses))) +
  geom_line(aes(y = as.numeric(ABIC), color = "red")) +
  geom_line(aes(y = as.numeric(BIC), color = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("ABIC", "BIC"))

fitstats_att <- tt_outs_att %>%
  select(-name, -LLRepTbl, -summaries, -llnreps, -optseed, -seedused, -t11_km1ll) %>%
  unnest(cols = c(nclasses, Loglikelihood, BIC, ABIC, BLRT_pval, VLMRT_pval,
                  Entropy)) %>%
  filter(nclasses < 7)

write_csv(fitstats_att, here("analysis/figures/fitstats_LPA-att.csv"))
