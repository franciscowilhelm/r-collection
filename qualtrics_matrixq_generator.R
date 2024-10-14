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
  return(list(scalenames, itemtext_list, items))
}

# # # todo: integrate the output returned from qualtrics_matrixq_generator function as x
qualtrics_matrixq_responsescales <- function(inpfile, sheet, varcol, leadin, resp, resplabel) {
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
  items <- items %>% mutate(itemno = items_raw[,resp, drop = TRUE], itemtext = items_raw[,resplabel, drop = TRUE]) %>% filter(!is.na(itemtext))
  
  scalenames <- unique(items$scale)
  # leadin always written on first line of scale.
  leadin_out <- map(str_c(scalenames, "_1"), function(name) {
    as.character(x[x[,varcol] == name, leadin] |> drop_na())
  })
  
  return(list(responses = items, leadin = leadin_out))
}
