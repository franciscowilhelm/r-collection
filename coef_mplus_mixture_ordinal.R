coef_mplus_mixture_ordinal <- function(fit, parameter) {
  require(dplyr)
  x <- fit$parameters$probability.scale %>% dplyr::filter(param == parameter)
  require(ggplot2)
  x %>% ggplot(aes(x = category, y = est)) +
    geom_bar(stat = "identity") +
    facet_wrap(vars(LatentClass)) +
    labs(title = str_c("Distribution of ", parameter, " across profiles"))
}

coef_mplus_mixture_continuous <- function(fit, parameter) {
  # extract mean and variance values across and show table
  require(dplyr)
  require(stringr)
  x <- fit$parameters$unstandardized
  x <- x %>% filter(param == parameter) %>%
    select(paramHeader, param, est, LatentClass) %>% 
    arrange(paramHeader, LatentClass)
  return(x)
}