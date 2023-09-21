mplus_growthuni(vars = list(`0` = "y_t1", `1` = "y_t2", `3` = "y_t4"))
mplus_growthuni(vars = list(`0` = "y_t1", `1` = "y_t2", `3` = "y_t4"), preds = c("x1", "x2"))

# unnamed vars
mplus_growthuni(vars = list( "y_t1", "y_t2", "y_t4"))

# quadratic
mplus_growthuni(vars = list(`0` = "y_t1", `1` = "y_t2", `3` = "y_t4"), preds = c("x1", "x2"), quadratic = TRUE)

# paralels / bivariate
mplus_growthbivariate(vars1 = list(`0` = "y_t1", `1` = "y_t2", `3` = "y_t4"),
                      vars2 = list(`0` = "y2_t1", `1` = "y2_t2", `3` = "y2_t4"))
mplus_growthbivariate(vars1 = list(`0` = "y_t1", `1` = "y_t2", `3` = "y_t4"),
                      vars2 = list(`0` = "y2_t1", `1` = "y2_t2", `3` = "y2_t4"),
                      preds = c("x1", "x2"))
