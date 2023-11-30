source("mplus_r3step_parsing_utils.R")
library(tidyverse)
library(MplusAutomation)
source("star_assign.R")

# todo: esnure backwards compatibility with older, automatic r3step.
x <- convert_mplus("tests/private/profiledata/r3stepauto_cat_5.out",
                   varnames = c("PROACTIV", "PROMOTIO", "PREVENTI", "SUPH_GM", "SUPO_GM"),
                   maxclasses = 5)

r3step_pubtable(x)


# other test with manual predictors
xqu_cor_converter <- convert_mplus("../covidshock/mplus/10_nagin/21d_correlates/01b_c6_pred copy_editedforparsing.out",
                                   varnames = c("proa", "prom", "prev", "supo"),
                                   maxclasses = 6)

r3step_pubtable(xqu_cor_converter)
# r3step_out[[1]]$ref_class <-
#   recode(
#     r3step_out[[1]]$ref_class,
#     `1` = 5,
#     `2` = 1,
#     `3` = 3,
#     `4` = 2,
#     `5` = 4
#   )
# r3step_out[[1]]$y_class <-   recode(
#   r3step_out[[1]]$y_class,
#   `1` = 5,
#   `2` = 1,
#   `3` = 3,
#   `4` = 2,
#   `5` = 4
# )