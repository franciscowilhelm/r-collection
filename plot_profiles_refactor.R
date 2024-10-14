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
  plot
  
  # if(!is.null(classlabels)) {
  #   plot + scale_x_discrete(labels = labels(classlabels))
  # }
  # else if(is.null(classlabels)) {
  #   plot + scale_x_discrete(labels = labels(classlabels))
  # }
}

# plotdat <-profiles_var_eq_67[["model_1_class_7"]][["model"]][["savedata"]]|> group_by(C1) %>% summarise(across(c("GEN", "HUM", "MOB", "NET"), ~mean(.x, na.rm = TRUE)))
# 
# plotdat_long <- plotdat %>%  pivot_longer(-C1) %>%
#   mutate(name = sjlabelled::as_factor(name), Class = as.factor(C1))
# 
# plot <- plotdat_long |> 
#   ggplot(aes(x = Class, y = value, fill = name)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Profile", y = "", fill = "") + theme_minimal() +
#   theme(panel.background = element_rect(fill = 'white', color = 'white'),
#         plot.background = element_rect(fill = 'white', color = 'white'),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   axis.ticks.y = element_line(), axis.line.y = element_line())