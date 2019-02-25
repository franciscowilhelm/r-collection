# extract the lagged/leaded variable from DataCombine::slide

slideExtract <- function(data, Var, TimeVar, GroupVar, NewVar, slideBy, keepInvalid = FALSE, reminder = TRUE) {
  x <- DataCombine::slide(data = data, Var = Var, TimeVar = TimeVar, GroupVar = GroupVar, 
                          NewVar = NewVar, slideBy = slideBy, keepInvalid = keepInvalid, reminder)
  out <- dplyr::select(x, NewVar)
}