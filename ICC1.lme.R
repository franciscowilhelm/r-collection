

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
  mod <- lmer(dv ~ 1 + (1 | grp), na.action=na.omit)
  t0 <- as.numeric(VarCorr(mod)[1,1])
  sig2 <- as.numeric(VarCorr(mod)[2,1])
  icc1 <- t0/(t0+sig2)
  names(icc1) <- dv
  return(icc1)
  }
