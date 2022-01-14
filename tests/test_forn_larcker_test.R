# using Holzinger Swineford from lavaan
library(tidyverse)
library(lavaan)
library(semTools)
source("forn_larcker_test.R")

HS.model <- ' visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, HolzingerSwineford1939)

forn_larcker_test(fit, x = c("visual", "speed"), y = c("visual", "textual", "speed"))
forn_larcker_test(fit, x = c("visual"), y = c("visual", "textual", "speed"))
forn_larcker_test(fit, x = c("visual", "speed"), y = c("textual"))

