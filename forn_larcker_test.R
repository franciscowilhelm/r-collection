# fornell larcker test function
forn_larcker_test <- function(lavmodel, x, y, summary = FALSE) {
  avevar <- semTools::reliability(lavmodel)["avevar",x] # average variance extracted from indicators
  latcor_sq <- lavInspect(lavmodel, what = "cor.lv")[x,y]^2 #squared latent correlation
  criterion_met <-  avevar > latcor_sq
  if(length(x) == 1) {
    criterion_met <- t(as.matrix(criterion_met))
    rownames(criterion_met) <- x
    latcor_sq <- t(as.matrix(latcor_sq))
    rownames(latcor_sq) <- x
    names(avevar) <- x
  }
  if(length(y) == 1) {
    criterion_met <- as.matrix(criterion_met)
    colnames(criterion_met) <- y
    latcor_sq <- as.matrix(latcor_sq)
    colnames(latcor_sq) <- y
  }
  if(summary == TRUE) {
    if(!criterion_met) {
      return(str_c("Average variance extracted for first factor is:", round(avevar,2),
                   ". Latent squared correlation is: ", round(latcor_sq,2),
                   ". Fornell-Larcker criterion not met."))
    }
    if(criterion_met) {
      return(str_c("Average variance extracted for first factor is:", round(avevar,2),
                   ". Latent squared correlation is: ", round(latcor,2),
                   ". Fornell-Larcker criterion successfully met."))
    }
    
  }
  if(summary == FALSE) {
    out_criterion <- criterion_met %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(-rowname) %>%
      rename(criterion = value, x = rowname, y = name)
    out_latcor <- latcor_sq %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(-rowname) %>%
      rename(latcor_sq = value) %>% select(latcor_sq)
    out_avevar <- avevar[out_criterion$x] %>% enframe() %>% rename(ave = value)
    out <- bind_cols(out_criterion, out_latcor, out_avevar %>% select(ave))
    out <- out %>% filter(!(x == y))
    return(out)
  }
  
}