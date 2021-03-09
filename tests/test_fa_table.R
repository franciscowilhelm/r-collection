library(psych)
source("fa_table.R")
res <- fa(psychTools::bfi[1:25],5)
fa_table(res)

restable <- fa_table(res)

# test with 1 factor
res <- fa(psychTools::bfi[1:25],1)
fa_table(res)

# test with unequal labels
res <- fa(psychTools::bfi[1:5],1)
fa_table(res, c("a", "b", "c", "d", "e", "f"))
