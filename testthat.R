# scoreItemsMulti

load("exampleData/df_example.RData")
source("scoreItemsMulti.R")
library(tidyverse)
temp <- scoreItemsMulti(scalenames = c("over_h", "ceng"), dataframe = df_example, exclude = TRUE)

