sep_label <- function(coefout) {
  # takes coef() as input
  require(tidyverse)
  map_dfr(array_branch(coefout, 1), function(x) {
    xout <- bind_cols(as_tibble(t(x)), tibble(DV = NA, IV = NA))
    if(str_detect(xout$Label, "<-")) {
      xout <- xout |> mutate(DV = str_extract(xout$Label, ".*(?=<-)"),
                             IV = str_extract(xout$Label, "(?<=<-).*"))
    }
    return(xout)
  })
}

# library(MplusAutomation)
# m1_sem <- readModels('../aging_miami/mplus/02_m1_sem/')
# testcoefs <- coef(m1_sem$mediationonly.out, params = c('regression', 'new'))
# 
# x <- sep_label(testcoefs)
