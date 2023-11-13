codebookpath <- "..//..//..//OneDrive - Universitaet Bern///Diss//02_Lehre//HS23_Methodenseminar//05_Study Design//Codebook Seminar.xlsx"
debugSource("../r-collection/qualtrics_matrixq_generator.r")
items <- readxl::read_xlsx(codebookpath,
                           sheet = 3)
scaleisntr <- readxl::read_xlsx(codebookpath,
                                sheet = 7)

codebook <- qualtrics_matrixq_generator(codebookpath,
                                              sheet = 3, varcol = "Variable name", itemcol = "Item", outfile = "tmp.txt")

responsescales <- qualtrics_matrixq_responsescales(codebookpath,
                                                                  sheet = 7, varcol = "Var", leadin = "Instruction",
                                                                  resp = "Antwort", resplabel =  "Antwortlabel")

# add numbers to antwort labels

add_numbers_antwortlabel <- function(x) {
  map2_chr(x, seq_along(x), function(x, i) {
    str_c(x, " (", i, ")")
  })
}

# tmp <- add_numbers_antwortlabel(aging_codebook_responsescales[[2]]$phea)

# labels_new <- map(unique(aging_codebook_responsescales$scale), function(x) {
#   inp <- aging_codebook_responsescales %>% filter(scale == x)
#   add_numbers_antwortlabel(inp$itemtext)
# })

# merge formats

text <- map2_chr(codebook[[1]], seq_along(codebook[[1]]), function(x, i) {
  out <- str_c(str_c(x, ".", ), # number and instruktion
               "",
               str_c(codebook[[2]][[x]], collapse = "\n"),
               "",
               str_c(responsescales |> filter(scale == x) |> select(itemtext), collapse = "\n"),
               "", sep = "\n", collapse = "\n")
  return(out)
})
cat(text, sep = "\n", file = "aging_t3.txt")

