# unfinished as I found no vectorized solution for recoding values
lpa_sort_by_g <- function(profile, varname) {
  if("tidyProfile" %in% class(profile)) {
    mean_g <- profile %>% get_estimates() %>% filter(Parameter == varname, Category == "Means") %>% 
      group_by(Class) %>% summarise(mean_g = mean(Estimate))
  }
  else if("mplus.model" %in% class(profile)) {
    mean_g <- profile[["parameters"]][["unstandardized"]] %>% filter(param == toupper(varname), paramHeader == "Means") %>% 
      rename(Class = LatentClass) %>% mutate(Class = as.numeric(Class)) %>% 
      group_by(Class) %>% summarise(mean_g = mean(est))
  }
  mean_g <- mean_g %>% mutate(rank = rank(mean_g)) %>% arrange(rank)
  return(mean_g)
}

#rec string syntax creator
rec_creator <- function(new, old) {
  out <- vector(mode = "numeric", length = length(new))
  for(i in seq_along(new)) {
    out[i] <- str_c(old[i], "=", new[i], ";")
  }
  return(out)
}


plot_profiles <- function(lpafit, df, scale_values = TRUE, 
                          varnames, varlabels = NULL, classlabels = NULL,
                          arrange = "original", arrange_values = NULL,
                          arrange_var = NULL,
                             classlabels_wrap = 10) {
  # varlabels in same order as varnames
  
  # add proportions to classlabel
  if("tidyProfile" %in% class(lpafit)) {
    lpa_df <- lpafit[["dff"]] %>% select(Class)
    classlabels_prop <- str_c((lpafit[["model"]][["class_counts"]][["mostLikely"]][["proportion"]]*100) %>% round(1), "%")
  } 
  else if("mplus.model" %in% class(lpafit)) {
    lpa_df <- lpafit[["savedata"]]["C1"] %>% rename(Class = C1)
    classlabels_prop <- str_c( (lpafit[["class_counts"]][["mostLikely"]][["proportion"]]*100) %>% round(1), "%")
  } 
  
  if(!is.null(classlabels)){
    classlabels <- str_c(classlabels, " (", classlabels_prop, ")") %>% str_wrap(width = classlabels_wrap)
  }
  if(is.null(classlabels)){
    classlabels <- str_c(1:max(lpa_df$Class), " (", classlabels_prop, ")")
  }
  # rec string syntax creator
  rec_creator <- function(new, old) {
    out <- vector(mode = "numeric", length = length(new))
    for(i in seq_along(new)) {
      out[i] <- str_c(old[i], "=", new[i], ";")
    }
    return(out)
  }
  
  # arrange profiles by G or other metric.
  if(arrange == "var") {
    tmp <- lpa_sort_by_g(lpafit, arrange_var)
    lpa_df$Class_r <- factor(lpa_df$Class, levels = tmp$Class, labels = classlabels[tmp$Class])
    
  }
  else if(arrange == "manual") {
    # new way with factor and specifying manually the levels
    lpa_df$Class_r <- factor(lpa_df$Class, levels = arrange_values, labels = classlabels[arrange_values])
    # old way with recode
    #lpa_df$Class <- recode(lpa_df$Class, !!!new)
  }
  else if(arrange == "original") {
    # old <- sort(unique(lpa_df$Class))
    lpa_df$Class_r <- factor(lpa_df$Class, levels = sort(unique(lpa_df$Class)), labels = classlabels)
  }

  # merge dataframes, scale values if requested
  x <- bind_cols(df, lpa_df)
  if(scale_values == TRUE) {
    x <- x %>% mutate(across(all_of(varnames), scale))
  }
  # plotdat w or wo rename
  if(is.null(varlabels)) {
    plotdat <- x %>% group_by(Class_r) %>% summarise(across(all_of(varnames), ~mean(.x, na.rm = TRUE)))
  } else {
    names(varnames) <- varlabels
    plotdat <- x |> group_by(Class_r) |> summarise(across(all_of(varnames), ~mean(.x, na.rm = TRUE)))
    # plotdat <- rename(plotdat, all_of(varnames))
  }
    plotdat_long <- plotdat %>%  pivot_longer(-Class_r) %>% 
      mutate(name = as_factor(name)) %>% 
      mutate(Class = as.factor(Class_r))
    plot <- plotdat_long %>% 
      ggplot(aes(x = Class_r, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Profile", y = "", fill = "") + theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   axis.ticks.y = element_line(), axis.line.y = element_line())
    
    if(!is.null(classlabels)) {
      plot + scale_x_discrete(labels = labels(x$Class_r))
    }
    else if(is.null(classlabels)) {
      plot + scale_x_discrete(labels = labels(x$Class_r))
    }
}

# current
plot_csm_profile <- function(lpafit, df = df_s3, scale_values = TRUE, classlabels = NULL, which_csm = "all", arrange = "g", arrange_values = NULL,
                             classlabels_wrap = 10) {
  
  # add proportions to classlabel
  if("tidyProfile" %in% class(lpafit)) {
    lpa_df <- lpafit[["dff"]] %>% select(Class)
    classlabels_prop <- str_c((lpafit[["model"]][["class_counts"]][["mostLikely"]][["proportion"]]*100) %>% round(1), "%")
  } 
  else if("mplus.model" %in% class(lpafit)) {
    lpa_df <- lpafit[["savedata"]]["C1"] %>% rename(Class = C1)
    classlabels_prop <- str_c( (lpafit[["class_counts"]][["mostLikely"]][["proportion"]]*100) %>% round(1), "%")
  } 
  
  if(!is.null(classlabels)){
    classlabels <- str_c(classlabels, " (", classlabels_prop, ")") %>% str_wrap(width = classlabels_wrap)
  }
  if(is.null(classlabels)){
    classlabels <- str_c(1:max(lpa_df$Class), " (", classlabels_prop, ")")
  }
  # rec string syntax creator
  rec_creator <- function(new, old) {
    out <- vector(mode = "numeric", length = length(new))
    for(i in seq_along(new)) {
      out[i] <- str_c(old[i], "=", new[i], ";")
    }
    return(out)
  }
  
  # arrange profiles by G or other metric.
  if(arrange == "g") {
    tmp <- lpa_sort_by_g(lpafit, "gen")
    lpa_df$Class_r <- factor(lpa_df$Class, levels = tmp$Class, labels = classlabels[tmp$Class])

  }
  else if(arrange == "manual") {
    # new way with factor and specifying manually the levels
    lpa_df$Class_r <- factor(lpa_df$Class, levels = arrange_values, labels = classlabels[arrange_values])
           # old way with recode
           #lpa_df$Class <- recode(lpa_df$Class, !!!new)
  }
  else if(arrange == "original") {
    # old <- sort(unique(lpa_df$Class))
    lpa_df$Class_r <- factor(lpa_df$Class, levels = sort(unique(lpa_df$Class)), labels = classlabels)
  }
  
  
  # merge dataframes, scale values if requested
  x <- bind_cols(df, lpa_df)
  if(scale_values == TRUE) {
    x <- x %>% mutate(across(all_of(scalenames_csm[1:7]), scale))
  }
  # plot depending on which_csm
  if(which_csm == "all") {
    plotdat <- x %>% group_by(Class_r) %>% summarise(across(all_of(scalenames_csm[1:7]), ~mean(.x, na.rm = TRUE))) %>% 
      rename(`Impression Management` = cimp, `Building contacts` = cbui, `Using contacts` = cuse,
             `Human capital development` = chum, `Goal setting and planning` = cgoa, `Self-exploration` = cslf, `Mobility-oriented behavior` = cmob)
    plotdat_long <- plotdat %>%  pivot_longer(-Class_r) %>% 
      mutate(name = as_factor(name)) %>% 
      mutate(name = fct_relevel(name, "Impression Management", "Building contacts", "Using contacts", "Human capital development", "Goal setting and planning",
                                "Self-exploration", "Mobility-oriented behavior")) %>% 
      mutate(Class = as.factor(Class))
    plot <- plotdat_long %>% 
      ggplot(aes(x = Class_r, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Profile", y = "", fill = "") + theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   axis.ticks.y = element_line(), axis.line.y = element_line())
    
    if(!is.null(classlabels)) {
      plot + scale_x_discrete(labels = labels(x$Class_r))
    }
    else if(is.null(classlabels)) {
      plot + scale_x_discrete(labels = labels(x$Class_r))
    }
  }
  else if(which_csm == "redux") {
    plotdat <- x %>% group_by(Class_r) %>% summarise(across(all_of(scalenames_csm[c(2,4,5,7)]), ~mean(.x, na.rm = TRUE))) %>% 
      rename(`Building contacts` = cbui, `Human capital development` = chum,
             `Goal setting and planning` = cgoa, `Mobility-oriented behavior` = cmob)
    plotdat_long <- plotdat %>%  pivot_longer(-Class_r) %>% 
      mutate(name = as_factor(name)) %>% 
      mutate(name = fct_relevel(name, "Building contacts", "Human capital development", "Goal setting and planning",
                                "Mobility-oriented behavior")) %>% 
      mutate(Class = as.factor(Class_r))
    plot <- plotdat_long %>% 
      ggplot(aes(x = Class, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Profile", y = "", fill = "") + theme_minimal() + 
      theme(panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white', color = 'white'),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   axis.ticks.y = element_line(), axis.line.y = element_line())
    
    plot
    # if(!is.null(classlabels)) {
    #   plot + scale_x_discrete(labels = waiver())
    # }
    # else if(is.null(classlabels)) {
    #   plot + scale_x_discrete(labels = waiver())
    # }
  }

}


# deprecated function?
# plot_profiles_individual <- function(lpafit, scale_values = TRUE, classlabels = NULL) {
#   tmp <- lpa_sort_by_g(lpafit, "gen")
#   
#   if("tidyProfile" %in% class(lpafit)) {
#     lpa_df <- lpafit[["dff"]] %>% select(Class)
#     classlabels_prop <- str_c( (lpafit[["model"]][["class_counts"]][["mostLikely"]][["proportion"]]*100) %>% round(1), "%")
#   } 
#   else if("mplus.model" %in% class(lpafit)) {
#     lpa_df <- lpafit[["savedata"]]["C1"] %>% rename(Class = C1)
#     classlabels_prop <- str_c( (lpafit[["class_counts"]][["mostLikely"]][["proportion"]]*100) %>% round(1), "%")
#   } 
#   
#   new <- tmp$rank
#   old <- c(1:max(tmp$rank))
#   names(new) <- old
#   lpa_df$Class <- recode(lpa_df$Class, !!!new)
#   
#   # class sizes for axis tick labels
#   classlabels_prop <- classlabels_prop[new] #reorder 
#   
#   
#   x <- bind_cols(df_s3, lpa_df)
#   if(scale_values == TRUE) {
#     x <- x %>% mutate(across(all_of(scalenames_csm[1:7]), scale))
#   }
#   plotdat <- x %>% group_by(Class) %>% summarise(across(all_of(scalenames_csm[1:7]), ~mean(.x, na.rm = TRUE))) %>% 
#     rename(`Impression Management` = cimp, `Soziale Kontakte knüpfen` = cbui, `Soziale Kontakte nutzen` = cuse,
#            `Wissen und Kompetenzen entwickeln` = chum, `Karriereziele und -pläne entwickeln` = cgoa, `Selbsterkundung` = cslf, `Mobilitätsorientierte Aktivitäten` = cmob)
#   plotdat <- plotdat %>% 
#     pivot_longer(-Class) %>% 
#     mutate(name = as_factor(name)) %>% 
#     mutate(name = fct_relevel(name, "Impression Management", "Soziale Kontakte knüpfen", "Soziale Kontakte nutzen", "Wissen und Kompetenzen entwickeln",
#                               "Karriereziele und -pläne entwickeln",
#                               "Selbsterkundung", "Mobilitätsorientierte Aktivitäten")) %>% 
#     mutate(Class = as.factor(Class))
#   map(unique(plotdat$Class), function(c) {
#     plotdat |> filter(Class == c) |> 
#       ggplot(aes(x = name, y = value, fill = name)) +
#       geom_bar(stat = "identity", position = "dodge") +
#       # facet_grid(Class ~ .) +
#       labs(x = "", y = "z-Standardisierte Werte", fill = "") + theme_minimal() + 
#       theme(panel.background = element_rect(fill = 'white', color = 'white'),
#             plot.background = element_rect(fill = 'white', color = 'white'),
#             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   axis.ticks.y = element_line(), axis.line.y = element_line(),
#             axis.text.x=element_blank()) +
#       coord_cartesian(ylim = c(-1.2,1.2))
#   })
#   
#     
# }
# 
bch_calculator <- function(profile, arrange = "g", arrange_values = NULL) {
  bch_results <- get_lcCondMeans(profile)
  
  # arrange and rename profiles by G or other metric.
  if(arrange == "g") {
    profileranks <- lpa_sort_by_g(profile, "gen") %>% arrange(Class)
    arrange_values <- profileranks$Class
    arrange_splicer <- profileranks$rank
    names(arrange_splicer) <- arrange_values
  }
  else if(arrange == "manual") {
    profileranks <- data.frame(old = arrange_values, new = 1:profile[["summaries"]][["NLatentClasses"]]) %>% arrange(old)
    arrange_splicer <- profileranks$new
    names(arrange_splicer) <- profileranks$old
    
  }
  
  # rename columns in bch_results overall
  # overall_oldnames <- names(bch_results$overall)
  overall_newnames <-
    c("var", rbind(str_c("m.", arrange_splicer), str_c("s.", arrange_splicer)),
      "chisqu", "df", "p")
  names(bch_results$overall) <- overall_newnames
  
  # rename columns in bch_results pairwise
  pairwise_new <- bch_results$pairwise
  pairwise_new$classA <-
    factor(pairwise_new$classA, levels = arrange_values)
  pairwise_new$classA <-
    recode_factor(pairwise_new$classA,!!!arrange_splicer)
  pairwise_new$classB <-
    factor(pairwise_new$classB, levels = arrange_values)
  pairwise_new$classB <-
    recode_factor(pairwise_new$classB,!!!arrange_splicer)
  bch_results$pairwise <- pairwise_new 
  # factor --> as character --> as numeric; bonkers but necessary
  bch_results$pairwise <- bch_results$pairwise %>% mutate(across(c(classA, classB), function(x) as.numeric(as.character(x))))
  nprofiles <- profile[["summaries"]][["NLatentClasses"]]
  tmp <- bch_results$pairwise # tmp as short hand for easier calculations below

  # populate pairwise with all possible combinations, recode, then trim again...

  # populate
  filterfun <- function(x,y) x == y
  tmp2 <- cross2(1:nprofiles, 1:nprofiles, .filter = filterfun) %>% map_dfr(., function(x) {
    map_dfr(unique(tmp$var), function(var) {
      if (nrow(tmp[tmp$var == var & tmp$classA == x[[1]] & tmp$classB == x[[2]], ]) == 1) {
        tmp[tmp$var == var & tmp$classA == x[[1]] & tmp$classB == x[[2]], ]
      }
      else {
        out <- tmp[tmp$var == var & tmp$classA == x[[2]] & tmp$classB == x[[1]], ]
        out$classA <- x[[1]]
        out$classB <- x[[2]]
        out
      }
    })
  })

  # deprecated old recode
  # # arrange profiles by G or other metric.
  # if(arrange == "g") {
  #   profileranks <- lpa_sort_by_g(profile, "gen")
  #   arrange_values <- profileranks$LatentClass
  #   arrange_splicer <- profileranks$rank
  #   names(arrange_splicer) <- arrange_values
  #   tmp2$classA <- factor(tmp2$classA, levels = arrange_values)
  #   tmp2$classA <- recode_factor(tmp2$classA, !!!arrange_splicer)
  #   tmp2$classB<- factor(tmp2$classB, levels = arrange_values)
  #   tmp2$classB <- recode_factor(tmp2$classB, !!!arrange_splicer)
  # }
  # else if(arrange == "manual") {
  #   tmp2$classA <- factor(tmp2$classA, levels = arrange_values)
  #   tmp2$classA <- recode_factor(tmp2$classA, !!!arrange_splicer)
  #   tmp2$classB<- factor(tmp2$classB, levels = arrange_values)
  #   tmp2$classB <- recode_factor(tmp2$classB, !!!arrange_splicer)
  # }
  # else if(arrange == "original") {
  #   arrange_values <- unique(tmp2$classA)
  #   tmp2$classA <- factor(tmp2$classA)
  #   tmp2$classB<- factor(tmp2$classB)
  # }

  # function that calculates mean differences
  tempfun <- function(var, classA, classB) {
    ma <- bch_results_long[(bch_results_long$var == var & bch_results_long$name == classA), "value", drop = TRUE]
    mb <- bch_results_long[(bch_results_long$var == var & bch_results_long$name == classB), "value", drop = TRUE]
    ma-mb
  }

  tmp2 <- cross2(1:nprofiles, 1:nprofiles) %>% map_dfr(., function(x) {
    tmp2[tmp2$classA == x[[1]] & tmp2$classB == x[[2]], ]
  })

  bch_results$overall_alt <- bch_results$overall %>%
    pivot_longer(cols = c(str_c("m.", c(1:nprofiles))), names_to = "profile", values_to = "mean") %>% #pivot longer with means over rows
    mutate(profile = str_extract(profile, "[0-9]")) # extract profile number
  bch_results$overall_alt <- bch_results$overall_alt %>% arrange(var,profile) %>% select(var, var,profile, mean) %>%
    pivot_wider(names_from = profile, values_from = mean)
  # create "superscripts" listing significant pairwise comparisons
  tmp3 <- map(bch_results$overall$var, function(v) {
    sign_comparisons <- map(1:nprofiles, function(class) {
      tmp2 %>% filter(var == v & classA == class & p < 0.05) %>% select(classB) %>% deframe()
    })
    return(sign_comparisons)
  })
  names(tmp3) <- bch_results$overall$var
  tmp3 <- map(tmp3, ~map_chr(., function(x) {
    if(length(x) > 0) {
      c(paste0("[",paste0(x, collapse = ","), paste0("]")))
    } else c("") }))

  bch_results$combined <-
    map2_dfr(array_branch(bch_results[["overall_alt"]] %>% select(-var), 1), tmp3, function(x,y) {
      paste(round(x,2),y) %>% set_names(str_c("P", seq_len(length(.)))) %>% as_tibble_row(.name_repair = "unique")
    }) %>% bind_cols(bch_results[["overall_alt"]]["var"], .)


  # trim again and recalculate tmp2 for viewing
  filterfun2 <- function(x,y) (x == y | x >= y)
  tmp2 <- cross2(1:nprofiles, 1:nprofiles, .filter = filterfun2) %>% map_dfr(., function(x) {
    tmp2[tmp2$classA == x[[1]] & tmp2$classB == x[[2]], ]
  })


  # show: var, classA / classB, chisq, p, mean difference, m classA, m classB
  bch_results_long <- bch_results$overall %>% pivot_longer(cols = c(str_c("m.", c(1:nprofiles)))) %>%
    mutate(name = str_extract(name, "[0-9]"))

  tmp2 <- tmp2 %>% mutate(m_diff = mapply(tempfun, var, classA, classB)) %>% arrange(classA, classB, var)

  # add wald test for overall differences to dataframes
  bch_results$overall_alt <- bind_cols( bch_results$overall_alt, bch_results$overall %>% select(chisqu, p)) %>% mutate(chisqu = as.numeric(chisqu),
                                                                                                                      p = as.numeric(p))
  bch_results$combined <- bind_cols( bch_results$combined, bch_results$overall %>% select(chisqu, p)) %>% mutate(chisq = as.numeric(chisqu),
                                                                                                        p = as.numeric(p))


  return(list(pairwise = tmp2, overall = bch_results$overall_alt, superscripts = bch_results$combined))
}