qualtrics_matrixq_generator <- function(inpfile, sheet, varcol, itemcol, outfile) {
  require(readxl)
  require(tidyverse)
  x <- read_excel(inpfile, sheet)
  varnames <- purrr::as_vector(x[,varcol]) %>% str_split("_")
  itemidx <- lapply(varnames, length) == 2 # where successfully split, indicate that it is an item. requires varcol to be well-formatted.
  items_raw <- x[itemidx,]
  items <- map_dfr(purrr::as_vector(items_raw[,varcol]), function(x) {
    tmp <- str_split(x, "_", simplify = TRUE)
    data.frame(scale = tmp[1], itemno = tmp[2])
  })
  items <- items %>% mutate(itemtext = items_raw[,itemcol, drop = TRUE])
  scalenames <- unique(items$scale)
  
  itemtext_list <- map(scalenames, function(x) {
    items %>% filter(scale == x) %>% select(itemtext) %>% simplify()
  })
  names(itemtext_list) <- scalenames
  
  text <- map2_chr(scalenames, seq_along(scalenames), function(x, i) {
      str_c(str_c(i, ".", x), # number and instruktion
            "",
            str_c(itemtext_list[[x]], collapse = "\n"),
            "",
            "[Antwortoptionen einfuegen]",
            "", sep = "\n", collapse = "\n")
    })
    cat(text, sep = "\n", file = outfile)
}





# correlates <- read_xlsx("data/Item Ideas Merged 20210614.xlsx", sheet = 4)
# 
# varnames <- correlates$varname %>% str_split("_")
# itemidx <- lapply(varnames, length) == 2
# items_raw <- correlates[itemidx,]
# items <- map_dfr(items_raw$varname, function(x) {
#   tmp <- str_split(x, "_", simplify = TRUE)
#   data.frame(scale = tmp[1], itemno = tmp[2])
#   })
# 
# items <- items %>% mutate(itemtext = items_raw$Items)
# scalenames <- unique(items$scale)
# 
# itemtext_list <- map(scalenames, function(x) {
#   items %>% filter(scale == x) %>% select(itemtext) %>% simplify()
# })
# names(itemtext_list) <- scalenames  
# 
# # for antwortlabel: determine sections of scales then extract all from antwortlabel list.
# # for this we use itemidx (where varnames appear) as its the most reliable indicator of when scales end / new scales begin.
# sectionidx <- vector(length = length(itemidx))
# for(i in seq_along(itemidx)) {
#    sectionidx[i] <- ifelse(itemidx[i] == FALSE & itemidx[i+1] == TRUE,
#          TRUE, FALSE)
# }
# # check if number of sections = number of scales
# if(sum(sectionidx) != length(scalenames)) {
#  warning("Number of sections identified does not match number of scales identified.")}
# 
# sectionstart <- which(sectionidx)
# sectionend <- sectionstart[2:length(sectionstart)]-1
# sectionend[length(sectionend)+1] <- length(sectionidx)
# 
# antwortlabel_list <- map2(sectionstart, sectionend, function(s,e) {
#   correlates[s:e,] %>% select(Antwortlabel) %>% filter(!is.na(Antwortlabel)) %>% 
#     simplify()
# })
# names(antwortlabel_list) <- scalenames  
# 
# # needs to print a txt with number. instruktion, empty line, items, empty line, antwortlabel 
# text <- map2_chr(scalenames, seq_along(scalenames), function(x, i) {
#   str_c(str_c(i, ".", x), # number and instruktion
#         "",
#         str_c(itemtext_list[[x]], collapse = "\n"),
#         "",
#         str_c(antwortlabel_list[[x]], collapse = "\n"),
#         "",
#         sep = "\n", collapse = "\n")
# })
# 
# cat(text, sep = "\n", file = "csm_s2_correlates.txt")
