# Binary of modes

library(tidyverse)
library(here)

data_elim <- read_rds("analysis/data/derived_data/data-good-cases2.rds")
data_comp <- read_rds("analysis/data/derived_data/data-newnames-completeonly.rds")
dictionary <- readr::read_rds("analysis/data/derived_data/clean-question-list.rds")

elim_pids <- data_elim %>% pull(pid)

samp <- data_comp %>%
  filter(pid %in% elim_pids)


modebinary <- samp %>%
  select(pid, matches("b4_.mod")) %>%
  mutate(across(matches("b4_.mod"), ~ !is.na(.)
                )
         ) %>%
  transmute(pid,
            bik  = as.numeric(b4_wmod_bik | b4_smod_bik),
            wlk  = as.numeric(b4_wmod_wlk | b4_smod_wlk),
            trn  = as.numeric(b4_wmod_trn | b4_smod_trn),
            dal  = as.numeric(b4_wmod_dal | b4_smod_dal),
            dot  = as.numeric(b4_wmod_dot | b4_smod_dot),
            pas  = as.numeric(b4_wmod_pas | b4_smod_pas),
            oth  = as.numeric(b4_wmod_oth | b4_smod_oth),
            wsfh = as.numeric(b4_wmod_wfh | b4_smod_sfh)
            )

  # mutate(across(-pid, as.numeric))

write_rds(modebinary, here("analysis/data/derived_data/data-modebinary.rds"))
