library(psych)
library(psychTools)
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


# test with polychoric correlation
res <- fa(psychTools::bfi[1:25],5, cor = "poly")
fa_table(res)

# test with tetrachoric correlation
data(ability)

res <- fa(ability[1:200,1:5],5, cor = "tet")
fa_table(res)

# test with varimax rotation
res <- fa(bfi[1:25],5, rotate = 'varimax')
fa_table(res)
restable <- fa_table(res)