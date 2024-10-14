# test profile display
library(MplusAutomation)
library(tidyverse)
library(tidyLPA)

load("tests/private/profiles_var_eq_wlsmv_winsor.RData")
plot_profiles(profiles_var_eq_wlsmv_winsor$model_1_class_4, bifscores_t2_wlsmv, varnames = c("gen", "hum", "mob", "net"))
