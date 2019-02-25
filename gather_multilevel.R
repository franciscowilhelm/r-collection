### function for gathering multiple scales at once.
# Specifically designed for multilevel data of repeateud measures. Will create NAs for missing time points.
gather_multilevel <- function(df, varlist, maxlevel1) {
  # first, gather all the scale scores separately.
  gatherlist <- map(varlist, function(x) df %>%
                      dplyr::select(contains(x)) %>%
                      mutate(id = row_number()) %>%
                      gather(key = time, value = value , -id) %>%
                      arrange(id))
  
  # then, set up the variables for a rearranged dataframe (id,time, value)
  id <- gatherlist[[1]]$id
  time <- rep(0:(maxlevel1-1), length(unique(id)))
  scalevalue <- map_dfc(gatherlist, "value")
  names(scalevalue) <- varlist
  out <- cbind(id, time, scalevalue)
}