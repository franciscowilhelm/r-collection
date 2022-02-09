# utilities for parsing mplus output

# Levels of nesting:
# level 1: sections
# level 2: classes
# level 3: line types

# section parser only needs to understand alternative parameterizations
# define section headers

section_headers <- c("alternative parameterizations for the categorical latent variable regression",
                     "odds ratio for the alternative parameterizations for the categorical latent variable regression",
                     "quality of numerical results")


mplus_section_parser <- function(mplustxt, chunknames) {
  # chunkpositions <- map(chunknames, ~str_detect(mplustxt, .x)) #too fuzzy, look for exact matches
  chunkpositions <- map(chunknames, function(x) mplustxt == x) # exact matches
  sectioncol <- vector(mode = "character", length = length(mplustxt))
  sectioncol[sectioncol == ""] <- NA
  for(chunk in seq_along(chunknames)) {
    sectioncol[chunkpositions[[chunk]] == TRUE] <- chunknames[chunk]
  }
  
  return(sectioncol)
}

# takes file, converts, creates sections
convert_mplus <- function(file, varnames) {
  out <- read.delim(file, stringsAsFactors = FALSE)
  names(out) <- "output"
  out <- tibble(output = tolower(out$output)) %>% mutate(linenumber = row_number())
  
  # generate section header column
  out$section <- mplus_section_parser(out$output, section_headers)
  #fill all section rows with corresponding section 
  out <- out %>% tidyr::fill(section,  .direction = "down")
  
  # discard sections which are not yet coded, create dataframe holding each of the sections (7/22/2020: odds ratios, normal coefficients)
  out <- out %>% filter(section != 'quality of numerical results')
  
  out_odds <- out %>% filter(section == 'odds ratio for the alternative parameterizations for the categorical latent variable regression')
  out_coef <- out %>% filter(section == 'alternative parameterizations for the categorical latent variable regression')
  
  # because tidytext is unsutaible for mplus output, define another chain of string splits and trimmings
  out_odds$output <- map(out_odds$output, ~mplus_line_parser(.x))
  out_coef$output <- map(out_coef$output, ~mplus_line_parser(.x))
  
  line_types_list <-  line_classifier_options(varnames)
  
  out_odds <-
    out_odds %>% mutate(line_type = map_chr(out_odds$output, ~ mplus_line_classifier(.x, line_types_list))) %>% filter(line_type != "unclassified")
  out_coef <-
    out_coef %>% mutate(line_type = map_chr(out_coef$output, ~ mplus_line_classifier(.x, line_types_list))) %>% filter(line_type != "unclassified")
  # leads to a weird structure of output column but ok ...
  out <- list(out_coef, out_odds)
  out <- mplus_parameters_parser(out[[1]], odds = out[[2]]) %>%
    mutate(ref_class = as.numeric(ref_class), y_class = as.numeric(str_extract(y_class, "\\d")))
  
  
  return(out)
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

mplus_parameters_parser <- function(lines_df, filter = TRUE, odds = NULL) {
  # precreate df
  lines <- lines_df$output
  line_type <- lines_df$line_type
  
  # if Odds Ratios are wanted, include.
  if (!is.null(odds)) { 
      odd_logical = TRUE
     odds_lines <- odds$output
  } else { odd_logical = FALSE }
  
  df <- tibble(ref_class = character(1), y_class = character(1),
         param = character(1), estimate = character(1), or = character(1), se = character(1), est_se = character(1), pval = character(1),
         .rows = sum(line_type == "parameters"))
  p <- 1 #holds the current row for passing of parameter to df, which is unequal the current text line
  
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
    if(line_type[l] == "parameters") {
      line <- stringi::stri_remove_empty(lines[[l]])
      if (odd_logical) { odds_line <- stringi::stri_remove_empty(odds_lines[[l]])  }
      df[p,] <- tibble(ref_class = ref_class, y_class = y_class,
                   param = line[1], estimate = line[2], or = ifelse(odd_logical, odds_line[2], NA),
                   se = line[3], est_se = line[4], pval = line[5])
      p <- p+1
    }
  }
  df <- df %>% mutate_at(vars(estimate, se, est_se, pval), list(~as.numeric(.))) #convert some columns
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
