# 
#' Gather multilevel data into long format
#' Specifically designed for multilevel data of repeateud measures. Will create NAs for missing time points.
#' @param df 
#' @param varlist 
#' @param maxlevel1 
#' @param varnames 
#'
#' @return
#' @export
#'
#' @examples
gather_multilevel <- function(df, varlist, maxlevel1, varnames = NULL) {
  require(tidyverse)
  
  # first, gather all the scale scores separately.
  gatherlist <- map(varlist, function(x) df %>%
                      dplyr::select(contains(x)) %>%
                      mutate(id = row_number()) %>%
                      pivot_longer(cols = -id, names_to = "time", values_to = "value") %>%
                      arrange(id))
  
  # then, set up the variables for a rearranged dataframe (id,time, value)
  id <- gatherlist[[1]]$id
  time <- rep(0:(maxlevel1-1), length(unique(id)))
  scalevalue <- map2_dfc(gatherlist, varlist, function(x, name) {
    out <- x |> select(value)
    names(out) <- name
    out
  })
  
  if(length(id) != length(time)) {
    error("Length of ID and length of time columns do not agree. Check your selection of variables or your argument to maxlevel1")
  }
  
  if(missing(varnames)) {
    names(scalevalue) <- varlist
  } else {
    names(scalevalue) <- varnames
  }
  out <- bind_cols(tibble(id = id, time = time), scalevalue)
}