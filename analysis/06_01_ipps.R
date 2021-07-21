# Create item probability plots for all LPA models
# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

# Item Probability Plots (IPPs)==================

## Fn: Create IPPs -------------------------------
create_ipps <- function(outfile) {
  outfile %>%
    plotMixtures(parameter = "Means", ci = NULL) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title, scales = "free_y", ncol = 2) +
    theme(strip.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          text = element_text(size = 6, family = "serif"))
}

## Exec IPP creation -------------------------------

### Travel time -------------------------------------------------------------

allOut_time <- readModels(
  here("analysis/03_Mplus/trav-beh/time/"),
  recursive = FALSE)

ipps_time <- create_ipps(allOut_time)

ggsave(plot = ipps_time,"analysis/figures/ipps_time.png", width = 6.5, height = 4.5)

### Attitudes ---------------------------------------------------------------
allOut_att <- readModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive = FALSE)

ipps_att <- create_ipps(allOut_att)

ggsave(plot = ipps_att,"analysis/figures/ipps_att.png", width = 6.5, height = 4.5)

### Travel time, distance --------------------------------------------------
allOut_ttds <- readModels(
  here("analysis/03_Mplus/trav-beh/time_dist/"),
  recursive = FALSE)

ipps_ttdsx <- create_ipps(allOut_ttds)

ggsave(plot = ipps_ttds,"analysis/figures/ipps_ttds.png", width = 6.5, height = 4.5)




