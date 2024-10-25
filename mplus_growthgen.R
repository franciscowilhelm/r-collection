# Generate Mplus syntax for (latent) growth models
# pass list of named variables, e.g. `0` = . If not named, timepoints are assumed to be spaced equally.

mplus_growthuni <- function(vars = list(), preds = NULL, quadratic = FALSE, correlate = TRUE) {
  require(rlang)
  require(stringr)
  require(purrr)
  
  # input checks
  if(is_empty(vars)) {
    stop("List of variables is empty.")
  }
  if(is_empty(names(vars))) {
    warning("List of variables is unnamed. Assuming timepoints are spaced equally.")
  }
  
  if(is_empty(names(vars))) {
    timepoints <- seq(0, length(vars)-1)
    vars_syntax_vector <- map2_chr(vars, timepoints, ~str_c(.x, "@", .y))
    vars_syntax_merged <- str_c(vars_syntax_vector, collapse = " ")
    # to avoid over-long lines
    vars_syntax_merged <- str_wrap(vars_syntax_merged, width = 70) 
  }
  if(!is_empty(names(vars))) {
    timepoints <- as.numeric(names(vars))
    vars_syntax_vector <- map2_chr(vars, timepoints, ~str_c(.x, "@", .y))
    vars_syntax_merged <- str_c(vars_syntax_vector, collapse = " ")
    vars_syntax_merged <- str_wrap(vars_syntax_merged, width = 70)
  }
  
  # generate syntax for growth curve, and possible predictors
  if(quadratic == FALSE) {
    syntax_growth <- str_c("i s | ", vars_syntax_merged, ";")
  }
  if(quadratic == TRUE) {
    syntax_growth <- str_c("i s q | ", vars_syntax_merged, ";")
    
  }

  
  if(is.null(preds))
    { preds_chr <- "" } else {
    preds_chr <- str_c(preds, collapse = " ")
    }
  if(quadratic == FALSE) {
    syntax_predictors <- str_c("i s ON ", preds_chr, " ;")
  }
  if(quadratic == TRUE) {
    syntax_predictors <- str_c("i s q ON ", preds_chr, " ;")
  }
  
  # bind syntax and return it to user
  if(!is.null(preds)) {
    syntax_out <- str_c(syntax_growth, "\n", syntax_predictors)
  }
  if(is.null(preds)) {
    syntax_out <- str_c(syntax_growth)
  }
  
  if(correlate == TRUE) {
    syntax_out <- str_c(syntax_out, "! correlated intercepts and slopes",
                        "i WITH s;",
                        sep = "\n")
  }
  
  return(syntax_out)
  
}

mplus_growthbivariate <- function(vars1 = list(),
                                 vars2 = list(),
                                 preds = NULL, quadratic = FALSE,
                                 correlate = TRUE) {
  require(rlang)
  require(stringr)
  require(purrr)
  
  # input checks
  if(is_empty(vars1) | is_empty(vars2)) {
    stop("List of variables is empty.")
  }
  if(is_empty(names(vars1)) | is_empty(names(vars2))) {
    warning("List of variables is unnamed. Assuming timepoints are spaced equally.")
  }
  
  syntax_growth <- list()
  syntax_predictors <- list()
  for(i in 1:2) {
    if(i == 1) { vars <- vars1 } else { vars <- vars2}
    if(is_empty(names(vars))) {
      timepoints <- seq(0, length(vars)-1)
      vars_syntax_vector <- map2_chr(vars, timepoints, ~str_c(.x, "@", .y))
      vars_syntax_merged <- str_c(vars_syntax_vector, collapse = " ")
      vars_syntax_merged <- str_wrap(vars_syntax_merged, width = 70) 
    }
    if(!is_empty(names(vars))) {
      timepoints <- as.numeric(names(vars))
      vars_syntax_vector <- map2_chr(vars, timepoints, ~str_c(.x, "@", .y))
      vars_syntax_merged <- str_c(vars_syntax_vector, collapse = " ")
      vars_syntax_merged <- str_wrap(vars_syntax_merged, width = 70) 
    }
    
    # generate syntax for growth curve, and possible predictors
    if(quadratic == FALSE) {
      syntax_growth[i] <- str_c("i", i, " s", i, " | ", vars_syntax_merged, ";")
    }
    if(quadratic == TRUE) {
      syntax_growth[i] <- str_c("i", i, " s", i, " q", i, " | ", vars_syntax_merged, ";")
    }
    
    if(is.null(preds))
    { preds_chr <- "" } else {
      preds_chr <- str_c(preds, collapse = " ")
    }
    if(quadratic == FALSE) {
      syntax_predictors[i] <- str_c("i", i, " s", i, " ON ", preds_chr, " ;")
    }
    if(quadratic == TRUE) {
      syntax_predictors[i] <- str_c("i", i, " s", i, " q", i, " ON ", preds_chr, " ;")
    }
  }
  
  
  # bind syntax and return it to user
  syntax_growth <- str_c(syntax_growth[[1]], "\n", syntax_growth[[2]])
  if(!is.null(preds)) {
    syntax_predictors <- str_c(syntax_predictors[[1]], "\n", syntax_predictors[[2]])
    syntax_out <- str_c(syntax_growth, "\n", syntax_predictors)
  }
  if(is.null(preds)) {
    syntax_out <- syntax_growth
  }
  if(correlate == TRUE) {
    syntax_out <- str_c(syntax_out, "! correlated intercepts and slopes",
                        "i1 WITH i2 s1 s2;" , "i2 WITH s1 s2;",
                        "s1 WITH s2;",
                        sep = "\n")
  }
  
  return(syntax_out)
  
}