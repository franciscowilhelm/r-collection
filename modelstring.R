#' modelstring_alpha_core
#'
#' @param var_names the names of the variables that form the scale
#' @param time the name of the time variable when using a longitudinal multilevel design. Defaults to NULL.
#'
#' @return a syntax to be passed to MplusAutomation
#' @export
#'
#' @examples
modelstring_alpha_core <- function(var_names, time = NULL) {
  # compute variables
  var_no <- length(var_names)
  param_labels <- purrr::map(list("wc", "bc"), function(x) stringr::str_c(x, seq_len((var_no*(var_no-1))/2)))
  param_labels$wv <- str_c("wv", seq_len(var_no))
  param_labels$bv <- str_c("bv", seq_len(var_no))
  names(param_labels) <- c("wc", "bc", "wv", "bv")
  
  # create string placeholders
  string_varstart <- vector("list", var_no-1)
  string_with <- vector("list", var_no-1)
  
  # within section
  # wc = within correlations
  for (i in seq_len(var_no-1)) {
  string_varstart[i] <- str_c(var_names[i], "WITH\n", sep = " ")
  if(i == 1) { 
    string_with[i] <- str_c(var_names[-i], " (", param_labels$wc[i:var_no-i] , ")", sep = "", collapse = "\n")
    label_counter <-  var_no-i
    } else {
      string_with[i] <- str_c(var_names[-1:-i], " (", param_labels$wc[(label_counter+1):((label_counter+1)+var_no-i-1)] , ")", sep = "", collapse = "\n")
      label_counter <- ((label_counter+1)+var_no-i-1)
    }
  }
  string_wc <- str_c(string_varstart, string_with,";", collapse = "\n")
  
  # wv = within variances
  string_wv <- str_c(var_names, " (", param_labels$wv, ")", sep = "", collapse = "\n") %>% str_c(., ";\n")
    
  
  # between section
  # bc = between correlations
  for (i in seq_len(var_no-1)) {
    string_varstart[i] <- str_c(var_names[i], "WITH\n", sep = " ")
    if(i == 1) { 
      string_with[i] <- str_c(var_names[-i], " (", param_labels$bc[i:var_no-i] , ")", sep = "", collapse = "\n")
      label_counter <-  var_no-i
    } else {
      string_with[i] <- str_c(var_names[-1:-i], " (", param_labels$bc[(label_counter+1):((label_counter+1)+var_no-i-1)] , ")", sep = "", collapse = "\n")
      label_counter <- ((label_counter+1)+var_no-i-1)
    }
  }
  string_bc <- str_c(string_varstart, string_with,";", collapse = "\n")
  
  # bv = between variances
  string_bv <- str_c(var_names, " (", param_labels$bv, ")", sep = "", collapse = "\n") %>% str_c(., ";\n")
  
  # putting parts together
  string_final <- str_c("%WITHIN%",string_wc, string_wv, "%BETWEEN%", string_bc, string_bv, sep = "\n")
  
  # optional: items ON time
  if(!is.null(time)) {
    time_regression <- str_c(str_c(var_names, " ", collapse = ""), "ON", time, ";", sep = " ")
    string_final <- str_c("%WITHIN%",string_wc, string_wv, time_regression, "%BETWEEN%", string_bc, string_bv, sep = "\n")
  }
  
  

  
  return(string_final) }


#########################################################################################

modelstring_alpha_constraint <- function(var_names) {
  # compute parameter labels
  var_no <- length(var_names)
  param_labels <- purrr::map(list("wc", "bc"), function(x) stringr::str_c(x, seq_len((var_no*(var_no-1))/2)))
  param_labels$wv <- str_c("wv", seq_len(var_no))
  param_labels$bv <- str_c("bv", seq_len(var_no))
  names(param_labels) <- c("wc", "bc", "wv", "bv")
  
  # compute model constraint formula
  newconstraint <- "NEW (comp_v_w alpha_w comp_v_b alpha_b);"
  comp_v_w <- str_c("comp_v_w = ", str_sub( str_c(param_labels$wv, "+", collapse =""), end = -2),
                    "+2*(", str_sub( str_c(param_labels$wc, "+", collapse =""), end = -2), ");")
  alpha_w <- str_c("alpha_w = (((", str_sub( str_c(param_labels$wc, "+", collapse =""), end = -2), ")/", length(param_labels$wc), ")*", 
                   var_no*var_no, ")/comp_v_w;")
  comp_v_b <- str_c("comp_v_b = ", str_sub( str_c(param_labels$bv, "+", collapse =""), end = -2),
                    "+2*(", str_sub( str_c(param_labels$bc, "+", collapse =""), end = -2), ");")
  alpha_b <- str_c("alpha_b = (((", str_sub( str_c(param_labels$bc, "+", collapse =""), end = -2), ")/", length(param_labels$bc), ")*", 
                   var_no*var_no, ")/comp_v_b;")
  
  model_constraint <- str_c(newconstraint, comp_v_w, alpha_w, comp_v_b, alpha_b, sep = "\n")
  return(model_constraint)
}

#########################################################################################
modelstring_omega_core <- function(var_names, time = NULL) {
  # compute parameter labels
  var_no <- length(var_names)
  param_labels <- purrr::map(list("wl", "wr", "bl", "br"), function(x) stringr::str_c(x, seq_len(var_no)))
  names(param_labels) <- c("wl", "wr", "bl", "br")
  
  # within section
  # factor model section
  factorname <- str_c(str_trunc(var_names[1], width = 3, ellipsis =""), "_w")
  by <- str_c(factorname, " BY ")
  firstitemloading <- str_c(var_names[1], "* (", param_labels$wl[1], ")", "\n")
  otheritemloading <- str_c(var_names[2:var_no], " (", param_labels$wl[2:var_no], ")", collapse = "\n")
  factor_var_constraint <- str_c(factorname, "@1;\n\n")
  
  # residual variance section
  residual <- str_c(var_names,  " (", param_labels$wr, ");", collapse = "\n")
  
  # putting within section together
  within_section <- str_c("%WITHIN%\n", by, firstitemloading, otheritemloading, ";", factor_var_constraint,residual)
  
  # optional: items ON time
  if(!is.null(time)) {
    time_regression <- str_c(str_c(var_names, " ", collapse = ""), "ON", time, ";", sep = " ")
    within_section <- str_c("%WITHIN%\n", by, firstitemloading, otheritemloading, ";",
                            factor_var_constraint,residual,"\n",time_regression)
  }
  
  
  # between section
  # factor model section
  factorname <- str_c(str_trunc(var_names[1], width = 3, ellipsis =""), "_b")
  by <- str_c(factorname, " BY ")
  firstitemloading <- str_c(var_names[1], "* (", param_labels$bl[1], ")", "\n")
  otheritemloading <- str_c(var_names[2:var_no], " (", param_labels$bl[2:var_no], ")", collapse = "\n")
  factor_var_constraint <- str_c(factorname, "@1;\n\n")
  
  # residual variance section
  residual <- str_c(var_names,  " (", param_labels$br, ");", collapse = "\n")
  
  between_section <- str_c("%BETWEEN%\n", by, firstitemloading, otheritemloading, ";", factor_var_constraint,residual)
  
  string_final <- str_c(within_section, between_section, sep = "\n")
  return(string_final)
}

#########################################################################################
modelstring_omega_constraint <- function(var_names) {
  # compute parameter labels
  var_no <- length(var_names)
  param_labels <- purrr::map(list("wl", "wr", "bl", "br"), function(x) stringr::str_c(x, seq_len(var_no)))
  names(param_labels) <- c("wl", "wr", "bl", "br")
  
  newconstraint <- "NEW (num_w denom_w omega_w h_w num_b denom_b omega_b h_b);"
  num_w <- str_c("num_w = (", str_sub( str_c(param_labels$wl, "+", collapse =""), end = -2), ")**2;")
  denom_w <- str_c("denom_w = ((", str_sub( str_c(param_labels$wl, "+", collapse =""), end = -2), ")**2)+(", 
                   str_sub( str_c(param_labels$wr, "+", collapse =""), end = -2), ");")
  omega_w <- str_c("omega_w = num_w/denom_w;")
  h_w <- str_c("h_w = 1/(1+(1/(", str_c("(",param_labels$wl, "**2/", param_labels$wr, ")", sep = "", collapse = "+"), ")));")
  
  num_b <- str_c("num_b = (", str_sub( str_c(param_labels$bl, "+", collapse =""), end = -2), ")**2;")
  denom_b <- str_c("denom_b = ((", str_sub( str_c(param_labels$bl, "+", collapse =""), end = -2), ")**2)+(", 
                   str_sub( str_c(param_labels$br, "+", collapse =""), end = -2), ");")
  omega_b <- str_c("omega_b = num_b/denom_b;")
  h_b <- str_c("h_b = 1/(1+(1/(", str_c("(",param_labels$bl, "**2/", param_labels$br, ")", sep = "", collapse = "+"), ")));\n")
  
  constraints_index <- str_c(newconstraint, num_w, denom_w, omega_w, h_w, num_b, denom_b, omega_b, h_b, sep = "\n")
  positive_residuals <- str_c(c(param_labels$wr, param_labels$br), " > 0;\n", collapse = "")
  string_final <- str_c(constraints_index, positive_residuals)
  return(string_final)
}


