# 04_02 create model syntax

createModels(templatefile = here("analysis/03_Mplus/attitudes/lpa_attitudes_template.txt"))

runModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive=TRUE)
