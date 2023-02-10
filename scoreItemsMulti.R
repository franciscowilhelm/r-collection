scoreItemsMulti <- function(scalenames, dataframe, exclude = TRUE, manual_keys = NULL) {
  #' Automatically score multiple scales and return several psychometrical scale parameters
  #'
  #' This is a convenience wrapper for psych::scoreItems that allows automatically
  #' scoring multiple scales at once.
  #' @param scalenames character vector of scale names
  #' @param dataframe dataframe holding the items to be scored
  #' @param exclude Boolean indicating whether to exclude participant responses where more than 1/3 of a scale are NA.
  #' @param manual_keys named list with manual keys, formatted like in scoreItems.
  #' @return a list object holding scale scores and other information
  # rewrite 2021/11/18: when scales have different number of response options reverse coding may bug out. this is fixed here by specifying each scale separately.

  require(dplyr)
  
  purrr::walk(scalenames,
       function(x) {
         tmp <- dplyr::select(dataframe, starts_with(x))
         if (ncol(tmp) == 0) {
           stop(
             "For one or more scalenames no items could be found. Please check your scalenames/dataframe."
           )
         }
       } )
  
  # select items from dataframe
  dataframe_items <-  dplyr::select(dataframe, starts_with(scalenames))
  
  
  exclude_helper <- function(scale) {
           # if exclude = TRUE, exclude all persons who have more than 1/3 NA
           max_na <- ncol(scale) / 3
           real_na <- apply(scale, 1, function(x)
             sum(is.na(x)))
           index_na <- (real_na > max_na)
           scale[index_na, ] <- NA
           return(scale)
  }
  
  # modify dataframe such that participants who have to many NAs dont get scores
  dataframe_exclude <- purrr::map_dfc(scalenames,
                                             function(x) {
                                               exclude_helper(scale = dplyr::select(dataframe_items, dplyr::starts_with(x)))
                                             })
  scalenames_autoonly <- setdiff(scalenames, names(manual_keys)) # only auto generate keys for those where no manual keys are provided.
         
  keys.list <-
    purrr::map(scalenames_autoonly, function(x) {
      names(dplyr::select(dataframe_items, dplyr::starts_with(x)))
    })
  names(keys.list) <- scalenames_autoonly
  
  negativeitems <- function(scale) {
    psych::pca(scale)$loadings < 0
  }
  
  negative_index <-
    purrr::map(scalenames_autoonly, function(x) {
      negativeitems(dplyr::select(dataframe_items, dplyr::starts_with(x)))
    })
  names(negative_index) <- scalenames_autoonly
  
  
  if (any(purrr::map_lgl(negative_index, any))) {
    message(
      "Some items were negatively correlated with total scale and were (automatically) reversed. \n Please Check $negative_index"
    )
  }
  
  keys_negative <-
    purrr::map2(keys.list, negative_index, function(x, y) {
      x[y] <- stringr::str_c("-", x[y], sep = "")
      return(x)
    })
  
  # manual keys
  if (!is.null(manual_keys)) {
    for (i in seq_along(manual_keys)) {
      keys_negative[[names(manual_keys[i])]] <- manual_keys[[i]]
      # create negative index based on manual inputs
      negative_index[[names(manual_keys[i])]] <- ifelse(grepl("-", manual_keys[[i]], fixed = TRUE), TRUE, FALSE)
    }
  }
  


  
  if (exclude == TRUE) {
    list_scored <-
      purrr::map(scalenames, function(name) {
        keys <- keys_negative[[name]]
        subdf <- dataframe_exclude %>% select(starts_with(name))
        out <- psych::scoreItems(keys = keys, subdf, impute = "none")
        out$scores[] <-
          apply(out$scores, 2, function(a) {
            ifelse(is.nan(a), NA_real_, a)
          })
        return(out)
      })
  }


  names(list_scored) <- scalenames
  scores <- purrr::map2_dfc(list_scored, scalenames, function(scale, name) {
    x <- data.frame(scale$scores)
    names(x) <- name
    return(x)
  })
  alpha <-   purrr::map2_dfc(list_scored, scalenames, function(scale, name) {
    x <- data.frame(scale$alpha)
    names(x) <- name
    return(x)
  })
  
  scaleout <- list(scores = scores, alpha = alpha, negative_index = negative_index)
  

  return(scaleout)
}
       