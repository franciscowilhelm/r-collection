# utilities for parsing mplus output

# Levels of nesting:
# level 1: sections
# level 2: classes
# level 3: line types

# section parser only needs to understand alternative parameterizations
# define section headers

# updated 2023/11 for using with manual R3step method. 
# notice that you have to edit sections manually for correct reading. Insert a line called "REGRESSION MARKER" for start.

section_headers <- tolower(c("REGRESSION MARKER",
  "tests of categorical latent variable multinomial logistic regressions using",
                             "RESULTS IN PROBABILITY SCALE",
                             "LATENT CLASS ODDS RATIO RESULTS",
                     "odds ratios for tests of categorical latent variable multinomial logistic regressions",
                     "ODDS RATIO FOR THE ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
                     "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
                     "LOGISTIC REGRESSION ODDS RATIO RESULTS",
                     "BRANT WALD TEST FOR PROPORTIONAL ODDS",
                     "technical 1 output"))


mplus_section_parser <- function(mplustxt, chunknames) {
  #chunkpositions <- map(chunknames, ~str_detect(mplustxt, .x)) #too fuzzy, look for exact matches
  chunkpositions <- map(chunknames, function(x) mplustxt == x) # exact matches
  sectioncol <- vector(mode = "character", length = length(mplustxt))
  sectioncol[sectioncol == ""] <- NA
  for(chunk in seq_along(chunknames)) {
    sectioncol[chunkpositions[[chunk]] == TRUE] <- chunknames[chunk]
  }
  
  return(sectioncol)
}

# takes file, converts, creates sections
convert_mplus <- function(file, varnames, clustervar = "c", maxclasses) {
  # make varnames lower case
  varnames <- tolower(varnames)
  out <- read.delim(file, stringsAsFactors = FALSE)
  names(out) <- "output"
  out <- tibble(output = tolower(out$output)) %>% mutate(linenumber = row_number())
  # discard everything before the last mention of residual variances
  startmarker <- str_which(out$output, tolower("REGRESSION MARKER"))
  # starthere <- max(which(startmarker))
  
  out <- out[startmarker:nrow(out),]
  # generate section header column
  out$section <- mplus_section_parser(out$output, section_headers)
  #fill all section rows with corresponding section 
  out <- out %>% tidyr::fill(section,  .direction = "down")
  
  # discard sections which are not yet coded, create dataframe holding each of the sections (7/22/2020: odds ratios, normal coefficients)
  out <- out %>% filter(section != tolower('TECHNICAL 1 OUTPUT') &
                          section != tolower('BRANT WALD TEST FOR PROPORTIONAL ODDS') &
                          section != tolower("results in probability scale")
  )
  out_coef <- out %>% filter(section == section_headers[1] |
                               section ==  section_headers[2] |
                               section == section_headers[7])
  out_odds <- out %>% filter(section == section_headers[8] |
                               section ==  section_headers[5] |
                             section ==  section_headers[6])
  
  # because tidytext is unsutaible for mplus output, define another chain of string splits and trimmings
  out_odds$output <- map(out_odds$output, ~mplus_line_parser(.x))
  out_coef$output <- map(out_coef$output, ~mplus_line_parser(.x))
  
  line_types_list <-  line_classifier_options(varnames)
  
  out_coef <-
    out_coef %>%
    mutate(line_type = map_chr(out_coef$output, ~ mplus_line_classifier(.x, line_types_list))) %>%
    filter(line_type != "unclassified") %>% 
    mplus_parameters_parser(odd = FALSE, maxclasses = maxclasses, clustervar = clustervar) %>% 
    mutate(ref_class = as.numeric(ref_class), y_class = as.numeric(str_extract(y_class, ".$")))
  out_odds <-
    out_odds %>%
    mutate(line_type = map_chr(out_odds$output, ~ mplus_line_classifier(.x, line_types_list))) %>%
    filter(line_type != "unclassified") %>% 
    mplus_parameters_parser(odd = TRUE, maxclasses = maxclasses, clustervar = clustervar) %>% 
    mutate(ref_class = as.numeric(ref_class), y_class = as.numeric(str_extract(y_class, ".$")))

  
  
  return(list(lr_coefs = out_coef, oddsratios = out_odds))
}


# parses lines, splitting them into words/elements

mplus_line_parser <- function(line) {
  stringi::stri_split_boundaries(line) %>% flatten() %>% str_trim(side = "both")
}

# line classifier

## define line types
line_classifier_options <- function(varnames) {
  varnames_grouping <- str_c("(", varnames, ")")
  tibble(
    type = c("class_regressed",
             "parameters",
             "refclass"),
    regexpr = c(
      "\\bon\\b",
      str_c(varnames_grouping, collapse =  "|"),
      "parameterization using reference class.\\d"
    )
  )
}



## classifies lines function
mplus_line_classifier <- function(line, line_types_list) {

  line_c <- str_c(line, collapse = " ")
  classified <- map2_chr(line_types_list$type, line_types_list$regexpr,
           function(x, y) {
             # om <- "om"
             return(ifelse(any(str_detect(line_c, y)), x, NA))
             })
  
  classified <- classified[!is.na(classified)] #insert the one which is not NA
  if(is_empty(classified)) {
    classified <- "unclassified"
  }  #character(0) to unclassified
  return(classified)
}  
  
# parses input lines line_type-specific

mplus_parameters_parser <- function(lines_df, filter = FALSE, odd = TRUE, clustervar, maxclasses) {
  # precreate df
  lines <- lines_df$output
  line_type <- lines_df$line_type

  p <- 1 #holds the current row for passing of parameter to df, which is unequal the current text line
  ref_class <- maxclasses #default ref_class is highest possible class 
  
  if(!odd) {
    df <- tibble(ref_class = character(1), y_class = character(1),
                 param = character(1), estimate = character(1), se = character(1), est_se = character(1), pval = character(1),
                 .rows = sum(line_type == "parameters"))
  } else if(odd) {
    df <- tibble(ref_class = character(1), y_class = character(1),
                 param = character(1), estimate = character(1), se = character(1), ci_low = character(1), ci_up = character(1),
                 .rows = sum(line_type == "parameters"))
  }

  # go thourhg line by line
  for(l in 1:length(line_type)) {
    if(line_type[l] == "refclass") {
      line_c <- lines[[l]] %>% str_c(collapse = " ")
      ref_class = str_extract(line_c, "\\d")    
    }
    if(line_type[l] == "class_regressed") {
      line_c <- lines[[l]] %>% str_c(collapse = " ")
      y_class = str_extract(line_c, str_c(clustervar, "#\\d"))
    }
    if(line_type[l] == "parameters" & odd == FALSE) {
      line <- stringi::stri_remove_empty(lines[[l]])
      df[p,] <- tibble(ref_class = as.character(ref_class), y_class = y_class,
                   param = line[1], estimate = line[2], se = line[3], est_se = line[4], pval = line[5])
      p <- p+1
    } else if (line_type[l] == "parameters" & odd == TRUE) {
      line <- stringi::stri_remove_empty(lines[[l]])
      df[p,] <- tibble(ref_class = as.character(ref_class), y_class = y_class,
                       param = line[1], estimate = line[2], se = line[3], ci_low = line[4], ci_up = line[5])
      p <- p+1
    }
  }
  
  if(!odd) {
    df <- df %>% mutate_at(vars(estimate, se, est_se, pval), list(~as.numeric(.))) #convert some columns
  } else {
    df <- df %>% mutate_at(vars(estimate, se, ci_low, ci_up), list(~as.numeric(.)))
  }
  # reduce columns so that they do not appear twice
  
  if (filter == TRUE) {
    list_filtered <- vector(mode = "list", length = ref_class)
    list_filtered[[1]] <- filter(df, ref_class == 1)
    for (ref in seq_len(max(df$ref_class))) {
      if (ref > 1) {
        list_filtered[[ref]] <- filter(df, ref_class == ref) %>% 
          filter(!str_detect(.$y_class, str_c("c#", 1:ref - 1, collapse = "|")))
      }
    }
    df <- list_filtered %>% bind_rows()
  }
  return(df)
}

r3step_pubtable <- function(obj, digits = 2) {
  # obj = object created by convert_mplus().
  # table: predictors in rows, profile comparisons in columns. sub columns: a) for coef (se) sign. signs; b) OR column
  lr_coefs <-  obj[["lr_coefs"]] |> arrange(ref_class, y_class)
  oddsratios <-  obj[["oddsratios"]] |>  arrange(ref_class, y_class)
  
  # subset rows such that we have no duplicates
  lr_coefs_subset <- lr_coefs |> filter(y_class > ref_class)
  oddsratios_subset <- oddsratios |> filter(y_class > ref_class) |> filter(!is.na(estimate)) |> 
    rename(or = estimate, or_se = se) # workaround of bugs, to be fixed
  
  # join oddsratios
  tbl_combined <- left_join(lr_coefs_subset, oddsratios_subset, by = c('ref_class', 'y_class', "param")) |>
    mutate(coef_se = str_c(round(estimate, digits), " (", round(se, digits), ")", vstar_assign(pval)),
           or = round(or, digits))
  
  # pivot wider to create form
  
  tbl_combined_wide_lr <- pivot_wider(tbl_combined, id_cols = "param", names_from = c("ref_class", "y_class"), values_from = "coef_se")
  tbl_combined_wide_or <- pivot_wider(tbl_combined, id_cols = "param", names_from = c("ref_class", "y_class"), values_from = "or")
  
  # formatting with subcolumns: tbd
  return(list(lr = tbl_combined_wide_lr, or = tbl_combined_wide_or))
}
