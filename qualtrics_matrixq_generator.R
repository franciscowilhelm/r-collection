qualtrics_matrixq_generator <- function(inpfile, sheet, varcol, itemcol, itemformat = "_number",
                                        responses = FALSE,
                                        leadin = NULL, resp = NULL, resplabel = NULL) {
  require(readxl)
  require(tidyverse)
  x <- read_excel(inpfile, sheet)
  
  if(itemformat == "_number") {
    varnames <- purrr::as_vector(x[,varcol]) %>% str_split("_")
    itemidx <- lapply(varnames, length) == 2 # where successfully split, indicate that it is an item. requires varcol to be well-formatted.
    items_raw <- x[itemidx,]
    items <- map_dfr(purrr::as_vector(items_raw[,varcol]), function(x) {
      tmp <- str_split(x, "_", simplify = TRUE)
      data.frame(scale = tmp[1], itemno = tmp[2])
    })
    items <- items %>% mutate(itemtext = items_raw[,itemcol, drop = TRUE])
    scalenames <- unique(items$scale)
  }
  
  
  # stemonly - when only variable ame is given without numbering of items
  
  # first a helper function that checks non-consecutive uses of the scalename
  checkNonConsecutive <- function(vec, target) {
    # If vec is a data frame with one column, convert it to a vector
    if (is.data.frame(vec)) {
      if (ncol(vec) == 1) {
        vec <- vec[[1]]
      } else {
        stop("Dataframe with more than one column provided. Please supply a single column or a vector.")
      }
    }
    
    # Ensure vec is a vector
    vec <- as.vector(vec)
    
    # Find the indices where the target appears
    idx <- which(vec == target)
    
    # If there are 0 or 1 occurrences, there's no possibility of non-consecutiveness
    if (length(idx) <= 1) {
      return(FALSE)
    }
    
    # Check if all consecutive differences are exactly 1
    return(any(diff(idx) != 1))
  }
  
  # begin creation/extraction of items
  if(itemformat == "stemonly") {
    itemidx_lgl <- !is.na(x[,varcol]) & !is.na(x[,itemcol]) 
    items_raw <- x[itemidx_lgl,] # items_raw contains all rows which are detected as items
    scalenames <- unique(items_raw[,varcol]) |> simplify()
    items <- map_dfr(scalenames, function(scale) {
      if(checkNonConsecutive(items_raw[,varcol], scale)) {
        stop("The variable/scale label is used non-consecutively. Please check for errors in codebook.")
      }
      items_of_scale <- items_raw |> filter(!!sym(varcol) == scale)
      items_of_scale <- items_of_scale |> mutate(itemno = seq_len(nrow(items_of_scale))) |> 
        rename(scale = varcol, itemtext = itemcol)
      return(items_of_scale)
    })
    # optionally, response and instructions if on same sheet
    if(responses == TRUE) {
      responses_leadin_list <- map(scalenames, function(scale) {
        resp_of_scale <- items_raw |> filter(!!sym(varcol) == scale)
        leadin = resp_of_scale |> select(all_of(leadin)) |> drop_na() |> pull()
        resp_of_scale <- resp_of_scale |> rename(resplabel = resplabel) |> pull(resplabel)
        return(list(leadin = leadin, responses = resp_of_scale))
    })
    }
  
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
    #cat(text, sep = "\n", file = outfile)
  return(list(scalenames = scalenames, itemtext_list = itemtext_list, text = text, items = items, responses_leadin_list = responses_leadin_list))
}

# # # todo: integrate the output returned from qualtrics_matrixq_generator function as x
qualtrics_matrixq_responsescales <- function(inpfile, sheet, varcol, leadin, resp, resplabel, itemformat = "_number")
  require(readxl)
  require(tidyverse)
  x <- read_excel(inpfile, sheet)
  pattern <- "(?=(?:.*_.*))_"
  varnames <- purrr::as_vector(x[,varcol]) %>% str_split (pattern)
  itemidx <- (lapply(varnames, length) ==1 | lapply(varnames, length) ==2) # where successfully split, indicate that it is an item. requires varcol to be well-formatted.
  items_raw <- x[itemidx,]
  
  items <- str_split(items_raw |> select(all_of(varcol)) |> as_vector(), pattern, simplify = TRUE) |> 
    as_tibble(.name_repair = c("universal")) |> rename(scale = ...1, no = ...2)
  
  items <- items %>% mutate(no = items_raw[,resp, drop = TRUE], itemtext = items_raw[,resplabel, drop = TRUE]) %>% filter(!is.na(itemtext))
  
  scalenames <- unique(items$scale)
  leadin_out <- map(scalenames, function(name) {
    # Build a regular expression:
    #   ^           : start of string
    #   name        : the scalename
    #   (           : start optional group
    #     _\\d+     : underscore followed by one or more digits
    #   )?          : end optional group (zero or one occurrence)
    #   $           : end of string
    pattern <- paste0("^", name, "(_\\d+)?$")
    
    # Filter rows in x based on the regex pattern
    rows <- x[str_detect(x[, varcol, drop = TRUE], pattern), leadin]
    
    # Drop any NA values and convert to character vector
    as.character(drop_na(rows))
  })
  
  return(list(responses = items, leadin = leadin_out))
}

#  # requires functions that Qualtrics does in fact not provide - recoding variable names rahter than choices/answers.
# qualtrics_matrixq_generator_refactor <- function(inpfile, sheet, varcol, itemcol, common_scales = NULL) {
#   # Load required packages
#   require(readxl)
#   require(tidyverse)
#   
#   # Read in the Excel sheet.
#   x <- read_excel(inpfile, sheet)
#   
#   # Extract the variable names from the specified column and split on underscore.
#   varnames <- purrr::as_vector(x[, varcol]) %>% str_split("_")
#   
#   # Identify rows where the split yields exactly two parts.
#   itemidx <- lapply(varnames, length) == 2
#   items_raw <- x[itemidx, ]
#   
#   # Build a data frame with:
#   #   - 'scale': the first part of the varname (the original scale)
#   #   - 'itemno': the second part (item number)
#   #   - 'varname': the full variable name (needed for recode lines)
#   items <- map_dfr(purrr::as_vector(items_raw[, varcol]), function(x) {
#     tmp <- str_split(x, "_", simplify = TRUE)
#     data.frame(scale   = tmp[1],
#                itemno  = tmp[2],
#                varname = x,
#                stringsAsFactors = FALSE)
#   })
#   
#   # Add the item text from the specified column.
#   items <- items %>% mutate(itemtext = items_raw[, itemcol, drop = TRUE])
#   
#   ## --- Map to common scales if requested ---
#   # If a common_scales mapping is provided (e.g. list(crq1 = c("oexp", "jmk", "ssk"))),
#   # then for each item, check if its original scale appears in any mapping and update.
#   if (!is.null(common_scales)) {
#     items <- items %>% mutate(new_scale = map_chr(scale, function(s) {
#       new_scale <- s
#       for (common in names(common_scales)) {
#         if (s %in% common_scales[[common]]) {
#           new_scale <- common
#           break
#         }
#       }
#       new_scale
#     }))
#   } else {
#     items <- items %>% mutate(new_scale = scale)
#   }
#   
#   # Determine the unique new (reformatted) scales in order of appearance.
#   old_scalenames <- unique(items$scale)
#   new_scalenames <- unique(items$new_scale)
#   
#   ## --- Build the item text list with recode lines before each item ---
#   # For each new scale group, process its items in order.
#   # For every item, insert a recode line of the form: [[Choice:original_varname]]
#   itemtext_list <- map(new_scalenames, function(ns) {
#     items_group <- items %>% filter(new_scale == ns)
#     map_chr(1:nrow(items_group), function(i) {
#       recode_line <- paste0("[[Choice:", items_group$varname[i], "]]")
#       # Each item is output as: recode line (on its own line) followed by the item text.
#       paste(recode_line, items_group$itemtext[i], sep = "\n")
#     })
#   })
#   names(itemtext_list) <- new_scalenames
#   
#   ## --- Create a mapping of new scale to original scale(s) ---
#   # This mapping is returned so that you can see, for each new scale,
#   # which original scale(s) contributed to it.
#   mapping_df <- items %>%
#     group_by(new_scale) %>%
#     summarise(original_scales = paste(unique(scale), collapse = ", ")) %>%
#     ungroup()
#   
#   ## --- Assemble the final output text ---
#   # For each new scale group, create a block that includes:
#   #  - A header line with both the new scale and (in parentheses) its original scale(s)
#   #  - The series of items (each now preceded by its recode line)
#   #  - A placeholder for answer options.
#   text <- map2_chr(new_scalenames, seq_along(new_scalenames), function(ns, i) {
#     orig <- mapping_df$original_scales[mapping_df$new_scale == ns]
#     preheader <- c("[[Matrix]]")
#     header <- paste0("[[ID:", ns, "]]\n", "Instruktion", "\n\n[[AdvancedChoices]]")
#     group_items <- paste(itemtext_list[[ns]], collapse = "\n")
#     str_c(preheader, header, group_items, "--Antwortoptionen einfuegen--", sep = "\n\n")
#   })
# 
#   # Return a list containing:
#   #   - new_scalenames: the reformatted scale names,
#   #   - itemtext_list: a list of item texts (each with its recode line) by group,
#   #   - items: the full items data frame (which includes both original 'scale' and new 'new_scale'),
#   #   - scale_mapping: a data frame mapping each new scale to its original scale(s).
#   return(list(
#     old_scalenames = old_scalenames,
#     new_scalenames = new_scalenames,
#     itemtext_list  = itemtext_list,
#     items          = items,
#     scale_mapping  = mapping_df,
#     text = text
#   ))
# }

write_qualtricsgen <- function(text, outfile) {
  # Write the assembled text to the output file.
  cat(text, sep = "\n", file = outfile)
}
