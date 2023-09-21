# generates long indicator (and factor) names for semTools::measEq function
longIndGenerator <- function(varlist, suffix) {
  out <- purrr::map(varlist, function(x) {
    c(x, stringr::str_c(x, suffix))
  })
  names(out) <- varlist
  return(out)
}

# test
# tmp <- longIndGenerator(c("bla", "bli", "blu"), "_t2")
