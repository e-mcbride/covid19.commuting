#' Loglikelihood replication table from `mplusModel`
#'
#' This function takes the output of a single model as an `mplusModel` object and returns the table of final stage loglikelihood values.
#'
#' @param mplusModel An `mplusModel` object, only of a single model.
#' @export
LLrep_to_table <- function(mplusModel) {
  modoutput <- mplusModel$output

  start <- which(modoutput %in% "Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:")
  end <- which(modoutput %in% "MODEL FIT INFORMATION")

  llrp <- modoutput[start:end] %>%
    stringr::str_extract_all("(-?[:digit:]+.?[:digit:]+)", simplify = T) %>%
    as.data.frame()
  if(ncol(llrp) != 3) {
    warning("There was a problem extracting the loglikelihood replication table")
  }
  llrp <- llrp %>%
    dplyr::rename("loglikelihood" = V1, "seed" = V2, "initStageStart" = V3) %>%
    filter(loglikelihood != "")

  return(llrp)
}

#' How many times the maximum loglikelihood was replicated
#'
#' This function returns a count of the number of times the maximum loglikelihood was replicated
#'
#' @param mplusModel An `mplusModel` object, only of a single model.
#' @export
LLreplication <- function(mplusModel) {
  # TO DO: add functionality for table created using `LLrep_to_table`
  nrep <- LLrep_to_table(mplusModel) %>%
    dplyr::mutate(replicated = loglikelihood == max(loglikelihood)) %>%
    dplyr::summarise(reps = sum(replicated)) %>%
    dplyr::pull(reps)
  message(paste0("Max Loglikelihood replicated ", nrep, " times."))
  return(nrep)
}
