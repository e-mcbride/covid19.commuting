# 05_02 create model syntax for travel time and distance LPA

createModels(templatefile = here("analysis/03_Mplus/trav-beh/time_dist/lpa_ttds_template.txt"))

runModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive=TRUE)
