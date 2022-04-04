# graveyard for 02_XX (data prep for mplus analysis)


# dummy var creation from 02_01 -------------------------------------------


# data_dum <- data_b4_analyze %>%
#   recipe(dst_dal ~ .) %>%
#   step_dummy(starts_with("score_"), one_hot = TRUE) %>%
#   prep() %>%
#   bake(data_b4_analyze) %>%
#   rename_with(~gsub("score_", "", .x, fixed = TRUE))

# data_b4_analyze %>% select(q_d_enjy) %>% unique()
# x <- data_b4_analyze %>% select(starts_with("q_")) %>%
#   gather() %>%
#   filter(is.na(value))
# data_b4_analyze$hinc %>% levels()

# an_coln <- colnames(data_dum) %>% str_c(collapse = " ") %>% noquote()
# an_coln


# write_file(an_coln, here("analysis/data/derived_data/analysis-colnames.txt"))

# save file to `analysis` folder bc that means mplus script can find it with no PATH necessary
# write_csv(data_dum, na = "-9999", col_names = FALSE, here("analysis/Mplus/b4-data-analysis.csv"))
