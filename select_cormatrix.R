# for Mplus Matrices

# works with covariance and correlation matrices
select_cormatrix <- function(matrix, vars, tri = T, cov = F) {
  if(tri == T & cov == F) {
    matrix <- as.matrix(as.dist(matrix))
    diag(matrix) <- 1
  }

  if(cov == T) {
    matdiag <- diag(matrix)
    matrix <- as.matrix(as.dist(matrix))
    diag(matrix) <- matdiag
  }

  as.data.frame(matrix) %>%
    dplyr::select(toupper(vars)) %>%
    .[toupper(vars),]
}
