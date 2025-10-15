library(MplusAutomation)
source("mplus_coef_tools.R")

mod <- readModels("exampleData")

mod$ex9.5_bayes.out$input$analysis$estimator == "BAYES"

x <- coef_wrapper(mod$ex9.5_bayes.out)
x
coef_matrix(x)

