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
        out <- model$summaries %>% select(all_of(keepCols))
        out <- bind_cols(out, tibble(min_prob = min(model$class_counts$mostLikely$proportion),
                                     max_prob = max(model$class_counts$mostLikely$proportion),
                                     min_N = min(model$class_counts$mostLikely$count),
                                     max_N = max(model$class_counts$mostLikely$count)))
        out

    })

}