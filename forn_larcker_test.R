# fornell larcker test function
forn_larcker_test <- function(lavmodel, x, y, summary = FALSE) {
  avevar <- semTools::reliability(lavmodel)["avevar",x] # average variance extracted from indicators
  latcor_sq <- lavInspect(lavmodel, what = "cor.lv")[x,y]^2 #squared latent correlation
  criterion_met <-  avevar > latcor_sq
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
    out_criterion <- criterion_met %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(-rowname) %>% rename(criterion = value)
    out_latcor <- latcor_sq %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(-rowname) %>% rename(latcor_sq = value) %>% select(latcor_sq)
    out_avevar <- avevar[out_criterion$rowname] %>% enframe() %>% rename(ave = value)
    out <- bind_cols(out_criterion, out_latcor, out_avevar %>% select(ave))
    return(out)
  }
  
}