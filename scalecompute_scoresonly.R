# this function takes the name of a scale and teh dataframe and returns the name of the scale and the scale scores of scoreItems()
scalecompute_scoresonly <- function(scalename, dataframe, exclude = FALSE) {
  scale <- select(dataframe, contains(scalename))

  # if exclude = TRUE, exclude all persons who have more than 1/3 NA
  max_na <- ncol(scale)/3
  real_na <- apply(scale, 1, function(x) sum(is.na(x)))
  index_na <- (real_na > max_na)
  scale[index_na,] <- NA

  keys <- rep(1,length(scale))
  scored <- scoreItems(keys, scale, impute = "none")
  scalescore <- scored$scores
  return(scalescore)
}
