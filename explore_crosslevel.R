# explore cross level interactions
# aim: function that automatically tests for cross level interactions in lme4 and Mplus

# TODO: require "optimx" for optimizer
explore_crosslevel_lme <- function(dataset, id, x, y, m, t = NULL) {
  # generate model
  if (!is.null(t)) {
    lme_formula <- str_c(y, " ~ ", "1 + ", t, "+", x, " + ", m, " + ", x, ":", m, " + ", "(1 +", x, "|", id, ")")
  }
  else {
    lme_formula <- str_c(y, " ~ ", "1 + ", x, " + ", m, " + ", x, ":", m, " + ", "(1 +", x, "|", id, ")")
  }
  require(lmerTest)
  lme_model <- lmer(lme_formula, data = dataset, control = lmerControl(optimizer = "optimx", optCtrl=list(method="L-BFGS-B")))
  convergence <- attr(lme_model,"optinfo")$conv$lme4
  out <- list(lme_model, convergence)
  names(out) <- c("lme_model", "convergence")
  return(out)
}

explore_crosslevel_lm <- function(dataset, x, y, m) {
  # generate model
    formula <- str_c(y, " ~ ", x, " + ", m, " + ", x, "*", m)
    require(stats)
    stats::lm(formula, data = dataset)
  }

# test <- explore_crosslevel_lme(data_scores_weekly_long_pcentered, "id", "hapa_pcenter", "csm_eng", "age", "time")

explore_crosslevel_mplus <- function(dataset, id, x, y, m, t = NULL) {
  model_clm <- mplusObject(
    TITLE = str_c("CROSS LEVEL INTERACTION", y, "ON", x, "*", m, sep = " "),
    VARIABLE = str_c("cluster =", id, "; \n WITHIN = ", t, "; \n BETWEEN =", m, ";"),
    ANALYSIS = "TYPE = TWOLEVEL RANDOM; \n estimator = BAYES;\n proc = 2;",
    MODEL = str_c("%WITHIN%\n s|", y, " ON ", x, ";\n", y, " ON ", t, ";",
                  "\n%BETWEEN%\n", y, ";\n  ",x, " WITH s;\n", y, " ON ", m, ";\n", "s ON ", m, ";"),
    autov = FALSE,
    usevariables = c(id, x, y, m, t),
    rdata = dataset)
  
  current_dir <- getwd()
  sub_dir <- "mplus_temp"
  ifelse(!dir.exists(file.path(current_dir, sub_dir)), dir.create(file.path(current_dir, sub_dir)), FALSE)
  mplusModeler(model_clm, file.path(current_dir, sub_dir, "explore_crosslevel.dat"), run = 1, hashfilename = FALSE)
  #runModels(target = "/Users/franciscowilhelm/Documents/GitHub/csm-weekly/mplus/clmod/") 
  fit_clm <- readModels(file.path(current_dir, sub_dir))
  unlink(file.path(current_dir, sub_dir), recursive = TRUE)
  return(fit_clm)
}

# test2 <- explore_crosslevel_mplus(data_scores_weekly_long_pcentered, "id", "hapa", "csm_eng", "age", "time")
