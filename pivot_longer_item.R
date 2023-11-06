# long version of items
pivot_longer_item <- function(df_wide, itemstem, itemnumber) {
  df_long <- df_wide %>%
    select(psID, starts_with(itemstem)) %>%
    select(psID, ends_with(itemnumber)) |>
    pivot_longer(
      starts_with(itemstem),
      values_to = str_c(itemstem,"_", itemnumber),
      names_to = "time",
      names_pattern = str_c(itemstem, "_T(\\d+)_", itemnumber)) %>%
    mutate(time = as.numeric(time)-1)
  # if(scoreonly = TRUE) {
  #     df_long <- df_long %>% select(scalename)
  # }
  return(df_long)
}

# list_item_long <-  map2(c("lea", "net", "env", "ceng", "winv"), list(1:3,1:3,1:3,1:3,1:4), function(scale, scalelength) {
#   tmp <- map(scalelength, function(itemnumber) {
#     pivot_longer_item(df_selm, scale, as.character(itemnumber))
#   })
#   reduce(tmp, \(l, r) left_join(l, r, by = c("psID", "time"))) # weird notation with \ for anon function
# })
# 
# df_items_long <- reduce(list_item_long, \(l, r) left_join(l, r, by = c("psID", "time")))