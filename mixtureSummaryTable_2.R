mixtureSummaryTable2 <- function (modelList,
                                    keepCols = c(
                                        "Title",
                                        "NLatentClasses",
                                        "Parameters",
                                        "AIC",
                                        "BIC",
                                        "aBIC",
                                        "Entropy",
                                        "BLRT_PValue"
                                    )) {
    map_dfr(modelList, function(model) {
      
      # check if model converged
      if(sjmisc::is_empty(model$errors)) {
        out <- model$summaries %>% select(all_of(keepCols))
        out <- bind_cols(out, tibble(min_prob = min(model$class_counts$mostLikely$proportion),
                                     max_prob = max(model$class_counts$mostLikely$proportion),
                                     min_N = min(model$class_counts$mostLikely$count),
                                     max_N = max(model$class_counts$mostLikely$count)))
        out
      } else { # if errors, return string with empty
        tibbleinput <- rep(NA, length(keepCols)+4)
        names(tibbleinput) <- c(keepCols, "min_prob", "max_prob", "min_N", "max_N")
        out <- tibble(!!!tibbleinput)
        out[1,1] <- str_c(model$summaries$Title, "- ERROR")
        out
        
      }
    })
}
