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
 
# Create test data based on the example you provided
test_data <- tribble(
  ~var, ~classA, ~classB, ~chisq, ~df, ~p, ~mean_classA, ~se_classA, ~mean_classB, ~se_classB,
  "CSAT_T3", 1, 2, 5.79, NA, 0.016, 2.58, 0.236, 3.34, 0.21,
  "CSAT_T3", 1, 3, 20.9, NA, 0.000, 2.58, 0.236, 3.73, 0.086,
  "CSAT_T3", 1, 4, 5.95, NA, 0.015, 2.58, 0.236, 3.21, 0.07,
  "CSAT_T3", 3, 2, 2.45, NA, 0.117, 3.73, 0.086, 3.34, 0.21,
  "CSAT_T3", 3, 4, 17.5, NA, 0.000, 3.73, 0.086, 3.21, 0.07,
  "CSAT_T3", 4, 2, 0.371, NA, 0.543, 3.21, 0.07, 3.34, 0.21,
  "EXHA_T3", 1, 2, 0.074, NA, 0.786, 2.86, 0.338, 2.99, 0.356,
  "EXHA_T3", 1, 3, 7.71, NA, 0.005, 2.86, 0.338, 1.82, 0.155,
  "EXHA_T3", 1, 4, 0.51, NA, 0.475, 2.86, 0.338, 3.13, 0.145,
  "EXHA_T3", 3, 2, 7.58, NA, 0.006, 1.82, 0.155, 2.99, 0.356,
  "EXHA_T3", 3, 4, 30.4, NA, 0.000, 1.82, 0.155, 3.13, 0.145,
  "EXHA_T3", 4, 2, 0.141, NA, 0.707, 3.13, 0.145, 2.99, 0.356,
  "PJF_T3", 1, 2, 6.50, NA, 0.011, 3.18, 0.203, 3.84, 0.161,
  "PJF_T3", 1, 3, 17.2, NA, 0.000, 3.18, 0.203, 4.09, 0.078,
  "PJF_T3", 1, 4, 1.13, NA, 0.288, 3.18, 0.203, 3.42, 0.074,
  "PJF_T3", 3, 2, 1.52, NA, 0.217, 4.09, 0.078, 3.84, 0.161,
  "PJF_T3", 3, 4, 30.1, NA, 0.000, 4.09, 0.078, 3.42, 0.074,
  "PJF_T3", 4, 2, 5.74, NA, 0.017, 3.42, 0.074, 3.84, 0.161
)

result <- test_data %>%
  group_by(var) %>%
  summarize(comparison = summarize_comparisons_concise(cur_data()),
            .groups = "drop")

# Print the result
print(result, n = Inf)

#Prioritizing relationships: It prioritizes showing "less than" relationships, then equality relationships, 
# and avoids showing redundant "greater than" relationships
# we could also turn this around for a more positive message!
