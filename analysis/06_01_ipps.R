# Create item probability plots for all LPA models
# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
library(ggrepel)
devtools::load_all()


# Item Probability Plots (IPPs)==================
## Fn: Plot mixtures for LCA probability scale --------------------------

# This fn taken and edited from source code for MplusAutomation::plotMixtures().
# I had to do this to get it to plot the line graphs for LCA probability scale

plotMixtures.probscale <- function(modelList,
                                   coefficients,
                                   paramCat,
                                   paramOrder = c("bik", "wlk", "trn", "dal", "dot", "pas", "oth", "wsfh")) {
  # select the category to plot (cat 2),
  plotdat <-
    lapply(modelList, function(x) {
      subset(x$parameters[[coefficients]], x$parameters[[coefficients]]$category == paramCat)
    })

  # Bind into one df with identifying variable

  plotdat <- do.call(rbind, lapply(names(modelList), function(x) {
    data.frame(Title = modelList[[x]]$input$title, plotdat[[x]])
  }))

  # Drop useless stuff
  plotdat <- plotdat %>% select(-est_se, -pval, -category)

  # Get some classy names
  names(plotdat)[which(names(plotdat) %in% c("param", "est", "LatentClass"))] <-
    c("Variable", "Value", "Class")

  plotdat$Variable <- factor(plotdat$Variable, levels = toupper(paramOrder))

  levels(plotdat$Variable) <- paste0(toupper(substring(levels(plotdat$Variable), 1, 1)), tolower(substring(levels(plotdat$Variable), 2)))


  classplotdat <-
    ggplot(
      NULL,
      aes_string(
        x = "Variable",
        y = "Value",
        group = "Class",
        linetype = "Class",
        shape = "Class",
        colour = "Class"
      )
    )

  classplot <- classplotdat + geom_point(data = plotdat) +
    geom_line(data = plotdat) +
    theme_bw()

  if (length(modelList) > 1) {
    classplot <- classplot + facet_wrap(~ Title)
  }
  return(classplot)
}


## Fn: Create IPPs ------------------------------------------
create_ipps <- function(outfile, parameter = "Means") {
  outfile %>%
    plotMixtures(parameter = parameter, ci = NULL) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title, scales = "free_y", ncol = 2) +
    theme(strip.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          text = element_text(size = 8, family = "serif"))
}

create_ipps.probscale <- function(outfile,
                                  coefficients,
                                  paramCat,
                                  paramOrder = c("bik", "wlk", "trn", "dal", "dot", "pas", "oth", "wsfh")) {
  plotMixtures.probscale(modelList = outfile, coefficients, paramCat, paramOrder) + #parameter = parameter, ci = NULL) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title, scales = "free_y", ncol = 2) +
    theme(strip.text = element_text(size = 11),
          legend.title = element_text(size = 10),
          text = element_text(size = 10, family = "serif"))
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

### Travel Mode -------------------------------------------------------------

allOut_mode <- readModels(
  here("analysis/03_Mplus/trav-beh/modeUsed/"),
  recursive = FALSE)

modeOrder <- c('dot' = "Driving Others", 'pas' = "Riding as Passenger",
               'dal' = "Driving Alone", 'bik' = "Biking",
               'wlk' = "Walking",
               'trn' = "Taking Transit", 'oth' = "Using Other Modes",
               'wsfh' = "Work and/or School From Home")

mode_className <- c("Active Mode and Transit Users",
                      "Carpool Users",
                      'Non-Drivers',
                      'Home Schoolers / Workers',
                      'Solitary Drivers')

classCounts_mode5 <- allOut_mode[["X5.class_lca_modeused_elim.out"]]$class_counts$mostLikely

classCounts_mode5$count

classname_count_labels <- paste0(mode_className, " (n=", classCounts_mode5$count, ")")


ipps_mode <- create_ipps.probscale(outfile = allOut_mode,
                                   coefficients = "probability.scale",
                                   paramCat = 2,
                                   paramOrder = names(modeOrder))


ggsave(plot = ipps_mode,"analysis/figures/ipps_modeWS.png", width = 6.5, height = 4.5)

ipp_mode5 <- allOut_mode["X5.class_lca_modeused_elim.out"] %>%
  create_ipps.probscale(coefficients = "probability.scale",
                        paramCat = 2,
                        paramOrder = names(modeOrder)) +
  scale_x_discrete(labels = str_wrap(unname(modeOrder), width = 11)) +
  ylab("Estimated Probabilities") +
  scale_colour_discrete(labels = str_wrap(classname_count_labels, width = 16))



ipp_mode5

ggsave(plot = ipp_mode5, "analysis/figures/ipp_mode5.png", width = 6.5, height = 3)








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


