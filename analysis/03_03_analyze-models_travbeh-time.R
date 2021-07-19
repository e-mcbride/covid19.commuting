
allOut <- readModels(
  here("analysis/03_Mplus/trav-beh/time/"),
  recursive = FALSE)


# 2-class model =====================

# pull the model out
lpatime2 <- allOut$X2.class_lpa_time.out

# check number of ll replications
# Analyze and update models
ttnrep2 <- LLreplication(lpatime2)

# get OPTSEED value
ttoptseed2 <- LLrep_to_table(lpatime2) %>%
  dplyr::slice(1) %>%
  pull(seed)

# Extract "INP" from file to put into update.model
lpatimeinp2 <- model_to_mplusObj(lpatime2) %>%
  append(list(modelout = here("analysis/03_Mplus/trav-beh/time/2-class_LPA_time.inp")))

hihi <- lpatimeinp2 %>% append(list(modelout = here("analysis/03_Mplus/trav-beh/time/2-class_LPA_time.inp")))

mplusModeler(lpatimeinp2)


ff <- update(lpatimeinp2,)

