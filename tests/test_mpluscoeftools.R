library(MplusAutomation)
source("mplus_coef_tools.R")

mod <- readModels("exampleData")

mod$ex9.5_bayes.out$input$analysis$estimator == "BAYES"

x <- coef_wrapper(mod$ex9.5_bayes.out)
x
cm <- coef_matrix(x)
cm

stopifnot(any(cm$W$IV == "X2" & cm$W$Y1 != ""))
stopifnot(any(cm$W$IV == "Y1" & cm$W$Y2 != ""))

stopifnot(stringr::str_detect(cm$W$Y1[cm$W$IV == "X2"], "0.466"))
stopifnot(stringr::str_detect(cm$W$Y2[cm$W$IV == "Y1"], "0.825"))

# example 2
x2 <- coef_wrapper(mod$example1c_moderation.out)
x2

coef(mod$example1c_moderation.out, params = "variability")

cm2 <- coef_matrix(x2, value = "est_se")
cm2

stopifnot(nrow(cm2$W) > 0)
stopifnot(any(cm2$W$IV == "X" & stringr::str_detect(cm2$W$M, "0\\.242")))
stopifnot(any(cm2$W$IV == "M" & stringr::str_detect(cm2$W$Y, "0\\.466")))
stopifnot(any(cm2$W$IV == "X" & stringr::str_detect(cm2$W$Y, "0\\.370")))

