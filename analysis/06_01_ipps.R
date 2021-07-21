# Create item probability plots for all LPA models
# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
library(ggrepel)
devtools::load_all()


# Item Probability Plots (IPPs)==================

## Fn: Create IPPs ------------------------------------------
create_ipps <- function(outfile) {
  outfile %>%
    plotMixtures(parameter = "Means", ci = NULL) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title, scales = "free_y", ncol = 2) +
    theme(strip.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          text = element_text(size = 8, family = "serif"))
}

## Fn: add counts of estimates ---------------------------------
add_estcount <- function(outfile) {
  combo <- outfile %>%
    enframe() %>%
    transmute(name,
              est_count = map(value,
                              ~ .x$class_counts$modelEstimated %>%
                                mutate(class = as.character(class))
              ),
              label_time = map(value,
                               ~ .x$parameters$unstandardized %>%
                                 filter(paramHeader == "Means") %>%
                                 filter(!str_detect(param, "C#")) %>%
                                 group_by(LatentClass) %>%
                                 mutate(est_se = as.numeric(est_se),
                                        est = as.numeric(est),
                                        maxest = est == max(est)) %>%
                                 filter(maxest)
              ),

    ) %>%
    unnest(cols = c(est_count, label_time)) %>%
    rename(Class = LatentClass,
           Variable = param,
           Value = est)

  if(any(combo$class != combo$LatentClass)){
    warning("Something went wrong")
  }
  return(combo)# message("Count Added successfully. NAs introduced by coercion are to be expected.")
}


## Exec IPP creation -------------------------------

### Travel time -------------------------------------------------------------

allOut_time <- readModels(
  here("analysis/03_Mplus/trav-beh/time/"),
  recursive = FALSE)

ipps_time <- create_ipps(allOut_time)

ipps_time

# ipps_time +
#   geom_label_repel(data = est_dat_time, aes(label = count))#, x = Variable, y = Value))

ggsave(plot = ipps_time,"analysis/figures/ipps_time.png", width = 6.5, height = 4.5)

ipp_time3 <- allOut_time$X3.class_lpa_time.out %>% create_ipps()
ipp_time3

classCounts_time3 <- classCounts_time %>% filter(str_detect(name, "3"))


ipp_time4 <- allOut_time$X4.class_lpa_time.out %>% create_ipps()
ipp_time4

ggsave(plot = ipp_time4, "analysis/figures/ipp_time4.png", width = 6.5, height = 4)



est_dat_time <- add_estcount(outfile = allOut_time)
classCounts_time <- est_dat_time %>%
  group_by(name, Class) %>%
  summarise(count)

classCounts_time4 <- classCounts_time %>% filter(str_detect(name, "4"))



### Attitudes ---------------------------------------------------------------
allOut_att <- readModels(
  here("analysis/03_Mplus/attitudes/"),
  recursive = FALSE)

ipps_att <- create_ipps(allOut_att)


att_desc <- c(q_crl = "I won’t rely on another person to get to work on time",
  q_csc = "My schedule is too erratic to be in a carpool",
  q_dfr = "I like the freedom of driving my own car",
  q_dnj = "I enjoy driving my car even in heavy traffic",
  q_drx = "Driving a car is a relaxing way to commute",
  q_tlf = "Taking public transit does not fit my lifestyle")

unname(att_desc)

ipps_att

ggsave(plot = ipps_att,"analysis/figures/ipps_att.png", width = 6.5, height = 4.5)

est_dat_att <- add_estcount(outfile = allOut_att)
classCounts_att <- est_dat_att %>%
  group_by(name, Class) %>%
  summarise(count)

classCounts_att4 <- classCounts_att %>% filter(str_detect(name, "4"))


ipp_att5 <- allOut_att$X5.class_lpa_att.out %>% create_ipps() +
  scale_x_discrete(labels = str_wrap(unname(att_desc), width = 20))

ipp_att5


ggsave(plot = ipp_att5, "analysis/figures/ipps_att5.png", width = 6.5, height = 4)

# class 1: freedom lovers: they hate congestion, but love the freedom of driving in their car. They may enjoy driving, but not commuting

# class 2: car users: these people are okay with using their cars, but they do not feel passionately either way

# class 3: car lovers: These like driving their cars in all situations, they enjoy driving.

# class 4: indifferent: these people really had very little opinion either way for most of the questions.

# class 5: cars haters: these people don't think of cars as providing freedom, and they do not like using cars to get around.

### Travel time, distance --------------------------------------------------
allOut_ttds <- readModels(
  here("analysis/03_Mplus/trav-beh/time_dist/"),
  recursive = FALSE)

ipps_ttdsx <- create_ipps(allOut_ttds)

ggsave(plot = ipps_ttds,"analysis/figures/ipps_ttds.png", width = 6.5, height = 4.5)


