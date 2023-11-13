load("exampleData/data_scores_df.RData")
source("gather_multilevel.R")
data_scores_df <- data_scores_df[1:151,] # exampledata is faulty in ID!
x <- data_scores_df |> select(-ends_with("_t0"), -ends_with("_gm")) |> 
  gather_multilevel(c("weng", "ceng"), 7, c("weng", "ceng"))
