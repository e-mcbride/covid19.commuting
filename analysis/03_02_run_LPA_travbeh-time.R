# Create travel time models
createModels(templatefile = here("analysis/03_Mplus/trav-beh/time/LPA-time-template.txt"))

# optimal seeds for each model (number is the number of classes)
ttoptseed1 <- 350608

# below runModles is commented out because
# ERROR when using the following method in `runModels()`:
#     replaceOutfile="modifiedDate"
# runModels(
#   here("analysis/03_Mplus/trav-beh/time/"),
#   recursive=TRUE)


