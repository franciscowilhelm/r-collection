# test profile display
library(MplusAutomation)
library(tidyverse)
load("tests/private/profiledata/df_scores_selm.RData")
df_tmp <- df_scores_selm |> select(ceng_T1, net_T1, lea_T1, env_T1, shoi_T1) |> na.omit()
x <- readModels("tests/private/profiledata/")
plot_profiles(x, df_tmp, varnames = c("ceng_T1", "net_T1", "lea_T1", "env_T1", "shoi_T1"), arrange = "original")
plot_profiles(x, df_tmp, varnames = c("ceng_T1", "net_T1", "lea_T1", "env_T1", "shoi_T1"),
              varlabels = c("Engagement", "Network", "Learning", "Environment", "Shock Intensity"),
              arrange = "original")
