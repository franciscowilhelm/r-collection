# this function takes the name of a scale and teh dataframe and returns the name of the scale and the output of scoreItems()
scalecompute_multi <- function(scalenames, dataframe, exclude = TRUE) {

exclude_helper <- function(scale) {
  # if exclude = TRUE, exclude all persons who have more than 1/3 NA
  max_na <- ncol(scale)/3
  real_na <- apply(scale, 1, function(x) sum(is.na(x)))
  index_na <- (real_na > max_na)
  scale[index_na,] <- NA
  return(scale)
  }

dataframe_exclude <- map_dfc(scalenames,
                            function(x) {exclude_helper(scale = dplyr::select(dataframe, contains(x)))})

keys.list <- map(scalenames, function(x) {names(select(dataframe, contains(x))) })
names(keys.list) <- scalenames

negativeitems <- function(scale) {
  psych::pca(scale)$loadings < 0
}

negative_index <- map(scalenames, function(x) {negativeitems(select(dataframe, contains(x))) })

if(any(map_lgl(negative_index, any))) {
  message("Some items were negatively correlated with total scale and were automatically reversed. \n Please Check.") }

keys_negative <- map2(keys.list, negative_index, function(x,y) {
  x[y] <- str_c("-", x[y], sep = "")
  return(x)})


if(exclude == TRUE) {
  scaleout <- scoreItems(keys_negative, dataframe_exclude, impute = "none")
}

if(exclude == FALSE) {
  scaleout <- scoreItems(keys.list, keys_negative, impute = "none")
}

scaleout$keys.list <- negative_index
return(scaleout)
}

