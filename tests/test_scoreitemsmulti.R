load("exampleData/df_example.RData")
source("scoreItemsMulti.R")
temp_normal <- scoreItemsMulti(scalenames = c("over_h", "ji_quant"), dataframe = df_example, exclude = TRUE)
temp_manual <- scoreItemsMulti(scalenames = c("over_h", "ji_quant"), dataframe = df_example, exclude = TRUE,
                               manual_keys = list(ji_quant = c("ji_quant_1_t1", "-ji_quant_2_t1", "ji_quant_3_t1", "ji_quant_4_t1")))


temp_manual2 <- scoreItemsMulti(scalenames = c("over_h", "ceng", "hapa", "sup_su", "sup_fam"), dataframe = df_example, exclude = TRUE,
                               manual_keys = list(over_h = c("over_h_1_t1", "over_h_2_t1", "-over_h_3_t1"),
                                                  hapa = c("hapa_1_t1", "-hapa_2_t1", "hapa_3_t1", "hapa_4_t1")))
