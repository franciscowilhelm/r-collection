

#' Calculate ICC
#'
#'Adapted from psychometric package.
#'
#'
#' @param dv character variable to calculate the ICC for.
#' @param grp character variable that is the level 2 identifier
#' @param data dataset to be passed (as dataframe)
#'
#' @return ICC value as double
#' @export
#'
#' @examples
ICC1.lme <- function (dv, grp, data)  {
  require(lme4)
  data <- data[, c(dv, grp)]
  names(data) <- c("dv", "grp")
  mod <- lmer(dv ~ 1 + (1 | grp), data = data, na.action=na.omit)
  mod_varcorr <- VarCorr(mod)
  t0 <-  attr(mod_varcorr$grp, "stddev")
  sig2 <- attr(mod_varcorr, "sc")
  icc1 <- t0/(t0+sig2)
  names(icc1) <- dv
  return(icc1)
}
