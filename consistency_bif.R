# experimental function: estimate consistency from a fitted bifactor lavaan model
consistency_bif <- function(fit, g.scale = "g") {
  lambda_mat <- lavInspect(fit, what = "est")$lambda
  psi_mat <- lavInspect(fit, what = "est")$psi
  
  s_indicators <- rownames(lambda_mat[apply(lambda_mat, 1, function(x) sum(x > 0)) == 2,])
  
  out <- map_dbl(s_indicators, function(i) {
    # consistency of an indicator is: (squared loading of indicator on g * variance of g)/true variance of indicator (g-variance of indicator  + s-variance of indicator)
    
    #squared loading of indicator on g (true variance g)
    sq_gl <- lambda_mat[i, g.scale]^2
    # variance of g
    varg <- psi_mat[g.scale,g.scale]
    
    #position of specific factor
    pos_k <- which(lambda_mat[i,] > 0 & colnames(lambda_mat) != g.scale)
    
    # squared loading of indicator on s (true variance specific)
    sq_sl <- lambda_mat[i,pos_k]^2
    # variance of s
    vars <- psi_mat[pos_k,pos_k]
    
    # put together in equation
    return(
      (sq_gl*varg)/((sq_gl*varg)+(sq_sl*vars))
    )  })
  return(out)
}