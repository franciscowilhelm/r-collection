sep_label <- function(coefout) {
  # takes coef() as input
  require(tidyverse)
  newlabels <- map_dfr(array_branch(coefout |> select(Label), 1), function(x) {
    xout <- bind_cols(as_tibble(t(x)), tibble(DV = NA, IV = NA))
    if(str_detect(xout$Label, "<-")) {
      xout <- xout |> mutate(DV = str_extract(xout$Label, ".*(?=<-)"),
                             IV = str_extract(xout$Label, "(?<=<-).*"))
    }
    return(xout)
  })
  coefout <- bind_cols(coefout, newlabels |> select(DV, IV))
  return(coefout)
}

coef_wrapper <- function(model) {
  testcoefs <-
    coef(model, params = c('regression'))
  testcoefs <- sep_label(testcoefs)
  
  testcoefs <-
    testcoefs |> mutate(DV = str_replace_all(DV, label_replace),
                        IV = str_replace_all(IV, label_replace))
  
  # testcoefs <- bind_rows(testcoefs, coef(model, params = 'new') |> select(Label, est, se, pval) |> mutate(IV = str_replace_all(Label, label_replace)))
  testcoefs
}

confint_wrapper <- function(model) {
  coefs <- coef(model, params = 'new') |> select(Label, est)
  confints <- confint(model, params = c('new')) |> mutate(IV = str_replace_all(Label, label_replace))
  left_join(coefs, confints, by = "Label")
}


# library(MplusAutomation)
# m1_sem <- readModels('../aging_miami/mplus/02_m1_sem/')
# testcoefs <- coef(m1_sem$mediationonly.out, params = c('regression', 'new'))
# 
# x <- sep_label(testcoefs)
