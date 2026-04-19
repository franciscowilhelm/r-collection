# using Holzinger Swineford from lavaan
library(tidyverse)
library(lavaan)
library(semTools)
source("forn_larcker_test.R")

HS.model <- ' visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = warnings)
}

fit <- cfa(HS.model, HolzingerSwineford1939, ncpus = 1L)

res_1 <- collect_warnings(forn_larcker_test(
  fit,
  x = c("visual", "speed"),
  y = c("visual", "textual", "speed")
))
res_2 <- collect_warnings(forn_larcker_test(
  fit,
  x = c("visual"),
  y = c("textual", "speed")
))
res_3 <- collect_warnings(forn_larcker_test(
  fit,
  x = c("visual", "speed"),
  y = c("textual")
))
res_4 <- collect_warnings(forn_larcker_test(
  fit,
  x = "visual",
  y = "textual",
  summary = TRUE
))

stopifnot(
  length(res_1$warnings) == 0L,
  length(res_2$warnings) == 0L,
  length(res_3$warnings) == 0L,
  length(res_4$warnings) == 0L
)

stopifnot(
  identical(names(res_1$value), c("x", "y", "criterion", "latcor_sq", "ave_x", "ave_y")),
  identical(names(res_2$value), c("x", "y", "criterion", "latcor_sq", "ave_x", "ave_y")),
  identical(names(res_3$value), c("x", "y", "criterion", "latcor_sq", "ave_x", "ave_y")),
  is.character(res_4$value),
  length(res_4$value) == 1L
)
