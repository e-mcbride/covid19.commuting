# Create travel time models
createModels(templatefile = here("analysis/03_Mplus/trav-beh/time/LPA-time-template.txt"))

# below runModles is commented out because
# ERROR when using the following method in `runModels()`:
#     replaceOutfile="modifiedDate"
# runModels(
#   here("analysis/03_Mplus/trav-beh/time/"),
#   recursive=TRUE)

# runModels(
#   here("analysis/03_Mplus/trav-beh/time/2-class_LPA_time.inp"),
#   recursive=TRUE)


