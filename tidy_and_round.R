tidy_and_round <- function(model) {
  broom::tidy(model) %>% mutate(across(where(is.numeric), ~round(.x, 2)))
}