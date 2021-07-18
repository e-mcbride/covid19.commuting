# Notes from MplusAutomation vignette

library(MplusAutomation)
library(here)

##### NOTE TO FUTURE SCRIPT RUNNERS:
#####   MUST UN-COMMENT AND RUN THE FOLLOWING FOR `MplusAutomation` TO WORK
# install.packages("BiocManager")
# BiocManager::install("rhdf5")
# install.packages("Rcpp")
# install.packages("relimp")
#####

# runModels() ================================================
#
## * recursive running of models: option is "recursive=TRUE"
##    + this will run all .inp in the directory, including sub-directories
##    + SO with following structure:
##        .../Analysis/attitudes/1-class_LPA.inp
##        .../Analysis/attitudes/2-class_LPA.inp
##        .../Analysis/trav-beh/1-class_LPA.inp
##        .../Analysis/trav-beh/2-class_LPA.inp
##    + RUN: `runModels(".../Analysis", recursive = TRUE)`
## * option replaceOutfile="modifiedDate" will re-run model only if input file has been changed
##
##
# readModels() ================================================
#
## Extracts all supported data from Mplus output as a list object stored as mplus.model
##
##
# extractModelSummaries() ================================================
#
## * Designed to extract model summaries from a group of models located within a directory (or nested within subdirectories).
## * Returns a data.frame
##    + One row per model, columns representing fit statistics
# showSummaryTable() ================================================
##

allOutput <- readModels(here("analysis/03_Mplus"))
sumStats <- readModels(here("analysis/03_Mplus"), what="summaries", recursive = TRUE)

# THESE TWO HAVE THE SAME RESULTS (good to know)
showSummaryTable(sumStats, keepCols=c("Title", "LL"))
showSummaryTable(allOutput, keepCols = c("Title", "LL"))

showSummaryTable(allOutput, keepCols = c("Title", "LL"), )

paramOut <- allOutput$X03_03_lpa.trav.beh_time.out$parameters$unstandardized
