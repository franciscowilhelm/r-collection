icc_beta_automate <- function(df, y, x, id) {
  # big problem is that you cannot access mer objects with "$" operator, and cannot return convergence codes....
  x_gm = str_c(x, "_gm")
  formula = as.formula(
    str_c(y, "~", "I(", x, "-", x_gm, ") + (I(", x, "-", x_gm, ") |", id, ")", sep = " ")
  )
  grp_means = aggregate(df[x], df[id], mean, na.rm = T)
  colnames(grp_means)[2] = x_gm
  df = merge(df, grp_means, by= id)
  lmm  <- lmer(formula,
               data = df, REML = FALSE)
  convergence <- length(attr(lmm,"optinfo")$conv$lme4)
  output <- list(lmm, convergence, icc_beta(lmm))
  names(output) <- c("merModObject", "convergence", "icc_beta")
  return(output)
}