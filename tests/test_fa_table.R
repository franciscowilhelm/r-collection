library(psych)
source("fa_table.R")
res <- fa(psychTools::bfi[1:25],5)
fa_table(res)

restable <- fa_table(res)
