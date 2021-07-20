# MAYBE LATER: Run model updates from R
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

allOut <- readModels(
  here("analysis/03_Mplus/trav-beh/time/"),
  recursive = FALSE)

lpa2tt <- allOut$X2.class_lpa_time.out

# Extract "INP" from file to put into update.model
lpa2ttinp <- lpa2tt[["input"]] %>% model_to_mplusObj()

# check the syntax
createSyntax(lpa2ttinp) %>% cat()

tech11 = paste0("TYPE = MIXTURE;\n PROCESSORS = 12(STARTS); \n STARTS = 0; \n OPTSEED = ", ttoptseed2)
tech11

lpa2ttinp2 <- update(lpa2ttinp,
                     rdata = tt_mplus,
                     DATA = ~ NULL,
                     ANALYSIS = ~ "TYPE = MIXTURE;\n PROCESSORS = 12(STARTS); \n STARTS = 0; \n OPTSEED = 972430;",
                     OUTPUT = ~ "TECH11"
)
lpa2ttinp2 <- update(lpa2ttinp,
                     rdata = tt_mplus,
                     DATA = ~ NULL,
                     ANALYSIS = ~ "TYPE = MIXTURE;\n PROCESSORS = 12(STARTS); \n STARTS = 0; \n OPTSEED = 972430;",
                     OUTPUT = ~ "TECH11"
)
# hihi <- lpa2tt %>% append(list(modelout = here("analysis/03_Mplus/trav-beh/time/2-class_LPA_time.inp")))
newmod2 <- function(mpinp, run_wd, nruns) {
  setwd(run_wd)
  mobj <- mplusModeler(mpinp,
                       run = nruns,
                       modelout = "2-class_LPA_time2HI.inp",
                       # modelout = here("analysis/03_Mplus/trav-beh/time/2-class_LPA_time2.inp"),
                       dataout = "timedat2.dat",
  )
  setwd(here())
  return(mobj)
}

tt2_t11 <- newmod2(lpa2ttinp2, run_wd = here("analysis/03_Mplus/trav-beh/time/"), nruns = 1)
tt2_t11$results$summaries %>% View(
)
