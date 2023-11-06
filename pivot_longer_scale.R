pivot_longer_scale <- function(df_wide, scalename, id = "id") {
  df_long <- df_wide %>% select(id, starts_with(scalename)) %>%
    pivot_longer(
      starts_with(scalename),
      values_to = scalename,
      names_to = "time",
      names_prefix = str_c(scalename, "_T")) %>%
    mutate(time = as.numeric(time)-1)
  # if(scoreonly = TRUE) {
  #     df_long <- df_long %>% select(scalename)
  # }
  return(df_long)
}
