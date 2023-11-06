# source("mplus_r3step_parsing_utils.R")
library(tidyverse)
source("star_assign.R")

x <- convert_mplus("tests/private/profiledata/r3stepauto_cat_5.out",
                   varnames = c("PROACTIV", "PROMOTIO", "PREVENTI", "SUPH_GM", "SUPO_GM"),
                   maxclasses = 5)

r3step_pubtable(x)

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