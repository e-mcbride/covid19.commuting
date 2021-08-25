# 04_02 create model syntax
library(tidyverse)
library(MplusAutomation)
library(here)

#import my function to write mplus data to file in the right file location
devtools::load_all()

# createModels(templatefile = here("analysis/03_Mplus/attitudes/lpa_attitudes_template.txt"))
## IN THE FUTURE: have the template files built by createModels in a separate folder, then moved to the "run" folder to protect work.
runModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive=TRUE)
