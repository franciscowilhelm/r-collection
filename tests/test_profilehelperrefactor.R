# test profile display
library(MplusAutomation)
library(tidyverse)
library(tidyLPA)

source("profile_helpers.R")
source("plot_profiles_refactor.R")

load("tests/private/profiles_var_eq_wlsmv_winsor.RData")
load("exampleData/temp.RData")
plot_profiles(profiles_var_eq_wlsmv_winsor$model_1_class_4, bifscores_t2_wlsmv, varnames = c("gen", "hum", "mob", "net"))
plot_profiles(m23_morin$config_class_6.out, bifscores, scale_values = FALSE, varnames = c("gen", "hum", "mob", "net"), class = "C1",
              arrange = "var", arrange_var = "gen")

# tidyLPA test
load("tests/private/m23_var_eq.RData")
p4 <- plot_profiles(profiles_m23_t1_var_eq$model_1_class_4, bifscores, varnames = c("gen", "hum", "mob", "net"),
                    arrange = "var", arrange_var = "gen", class = "Class")
