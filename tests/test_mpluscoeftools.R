library(MplusAutomation)
source("mplus_coef_tools.R")

mod <- readModels("exampleData")

x <- coef_wrapper(mod)
x
coef_matrix(x)
