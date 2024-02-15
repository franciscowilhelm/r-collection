tidy_lav <- function(model, operator, decimals = 2, sep_label = FALSE, label_replace = NULL) {
  tidyout <- broom::tidy(model) |>
    filter(op == operator) |>
    mutate(across(where(is.numeric), ~round(.x, decimals))) |> 
    mutate(sign  = vstar_assign(p.value)) |> 
    select(term, op, estimate, std.all, std.error, p.value, sign)
  
  if(sep_label == TRUE) {
    tidyout <- sep_label_lav(tidyout)
    if(!is.null(label_replace)) {
      tidyout <-
        tidyout |> mutate(DV = str_replace_all(DV, label_replace),
                            IV = str_replace_all(IV, label_replace))
    }
    tidyout <- tidyout |> select(DV, IV, estimate, std.all, std.error, p.value, sign)
    
  }
  return(tidyout)
}

sep_label_lav <- function(model) {
  # model here is broom::tidy output
  newlabels <- map_dfr(array_branch(model |> select("term"), 1), function(x) {
    xout <- bind_cols(as_tibble(t(x)), tibble(DV = NA, IV = NA))
    if(str_detect(xout$term, "~")) {
      xout <- xout |> mutate(DV = str_extract(xout$term, ".*(?=~)"),
                             IV = str_extract(xout$term, "(?<=~).*"))
    }
    return(xout)
  })
  coefout <- bind_cols(model, newlabels |> select(DV, IV))
  return(coefout)
}
