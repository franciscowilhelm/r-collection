# fa(df_scores_alt, nfactors = 3) %>% fa_table()


fa_table <- function(x, varlabels = NULL, title = "Factor analysis results") {
  #get sorted loadings
  require(dplyr)
  require(purrr)
  require(tibble)
  require(gt)
  x <- psych::fa.sort(x)
  if(!is.null(varlabels)) {
    varlabels <- varlabels[x$order]
  }
  if(is.null(varlabels)) {varlabels <- rownames(x$loadings)}

  loadings <- data.frame(unclass(x$loadings))
  
  #make nice names
  factornamer <- function(nfactors) {
    paste0("Factor_", 1:nfactors)}
  
  fnames <- factornamer(ncol(loadings))
  names(loadings) <- fnames
  
  # prepare locations
  factorindex <- apply(loadings, 1, function(x) which.max(abs(x)))
  
  # adapted from from sjplot: getremovableitems
  getRemovableItems <- function(dataframe, fctr.load.tlrn = 0.1) {
    # clear vector
    removers <- vector(length = nrow(dataframe))
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in seq_along(removers)) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(dataframe[i, ]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload - max2load) < fctr.load.tlrn) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers[i] <- TRUE
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return(removers)
  }
 
 removable <- getRemovableItems(loadings)
 
  small_loadings <- purrr::map(fnames, function(f) {
    abs(loadings[,f]) < .30
  })
  cross_loadings <- purrr::map2(fnames, seq_along(fnames), function(f, i) {
    (abs(loadings[,f] > .20)) & (factorindex != i) 
  })
  
  ind_table <- dplyr::tibble(varlabels, loadings) %>%
    dplyr::rename(Indicator = varlabels) %>% 
    dplyr::mutate(Communality = x$communality, Uniqueness = x$uniquenesses, Complexity = x$complexity) %>% 
    dplyr::mutate(across(starts_with("Factor"), round, 3))  %>%
    dplyr::mutate(across(c(Communality, Uniqueness, Complexity), round, 2))
                    
  
  ind_table <- ind_table %>% gt(rowname_col = "Indicator") %>% tab_header(title = title)
  # mark small loadiongs
  for(f in seq_along(fnames)) {
    ind_table <- ind_table %>%  tab_style(style = cell_text(color = "#D3D3D3", style = "italic"),
                             locations = cells_body(columns = fnames[f], rows = small_loadings[[f]]))
  }
  # mark cross loadings
  for(f in seq_along(fnames)) {
    ind_table <- ind_table %>%  tab_style(style = cell_text(style = "italic"),
                                    locations = cells_body(columns = fnames[f], rows = cross_loadings[[f]]))
  }
  # mark non-assignable indicators

  ind_table <- ind_table %>%  tab_style(style = cell_fill(color = "#D93B3B"),
                                  locations = cells_body(rows = removable))

  # adapted from https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/
  f_table <- rbind(x[["Vaccounted"]], x[["Phi"]]) %>%
    as.data.frame() %>%
    rownames_to_column("Property") %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    gt() %>% tab_header(title = "Eigenvalues, Variance Explained, and Factor Correlations for Rotated Factor Solution")
  
  # print(ind_table)
  # print(f_table)
  return(list("ind_table" = ind_table, "f_table" = f_table))
  
  
}
