library(tidyverse)

add_numbers_antwortlabel <- function(x) {
  map2_chr(x, seq_along(x), function(x, i) {
    str_c(x, " (", i, ")")
  })
}

# add_numbers_antwortlabel(clipboard()) %>% writeClipboard()


# x <- clipboard() %>% str_split(pattern = "\n", simplify = TRUE)
# add_numbers_antwortlabel(x) %>% writeClipboard()


# out <- map(x[[1]], function(x) {str_split(x, pattern = " ", simplify = TRUE)})
# out2 <- data.frame(vor = map_chr(out, function(x) x[1]),
#                    nach = map_chr(out, function(x) x[2]))
# 
# out2 %>%  write.table(., "clipboard", sep="\t", row.names=FALSE)
