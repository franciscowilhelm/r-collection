sep_label <- function(coefout) {
  # takes coef() as input
  require(tidyverse)
  newlabels <- map_dfr(array_branch(coefout |> select(Label), 1), function(x) {
    xout <- bind_cols(as_tibble(t(x)), tibble(DV = NA, IV = NA))
    if(str_detect(xout$Label, "<-")) {
      xout <- xout |> mutate(DV = str_extract(xout$Label, ".*(?=<-)"),
                             IV = str_extract(xout$Label, "(?<=<-).*"))
    }
    return(xout)
  })
  coefout <- bind_cols(coefout, newlabels |> select(DV, IV))
  return(coefout)
}

coef_wrapper <- function(model, label_replace = NULL, params = c('regression'), bayes = FALSE, addci = FALSE) {
  # get coefs
  testcoefs <-
    coef(model, params = params)
  # make p values two tailed (rough) if Bayes
  if(bayes == TRUE) {
    testcoefs <-  testcoefs |> mutate(pval = pval*2)
  }
  # add Confidence intervals
  if(addci == TRUE) {
    testcoefs <- left_join(testcoefs, confint(model,params), by = "Label")
    
  }
  # add DV and IV and replace labels
  testcoefs <- sep_label(testcoefs)
  if(!is.null(label_replace)) {
    testcoefs <-
      testcoefs |> mutate(DV = str_replace_all(DV, label_replace),
                          IV = str_replace_all(IV, label_replace))
  }

  
  # testcoefs <- bind_rows(testcoefs, coef(model, params = 'new') |> select(Label, est, se, pval) |> mutate(IV = str_replace_all(Label, label_replace)))
  testcoefs
}

confint_wrapper <- function(model) {
  coefs <- coef(model, params = 'new') |> select(Label, est)
  confints <- confint(model, params = c('new')) |> mutate(IV = str_replace_all(Label, label_replace))
  left_join(coefs, confints, by = "Label")
}


# library(MplusAutomation)
# m1_sem <- readModels('../aging_miami/mplus/02_m1_sem/')
# testcoefs <- coef(m1_sem$mediationonly.out, params = c('regression', 'new'))
# 
# x <- sep_label(testcoefs)

#' Build predictor x outcome coefficient matrices from MplusAutomation coefs
#'
#' @param coefs  Data frame from coef_wrapper(): columns include Label, est, se, pval,
#'               optionally LowerCI, UpperCI, and the parsed DV, IV (e.g., "W CRATSK").
#' @param value  What to show in cells: "est", "est_se", "est_ci", "est_p", "est_se_p".
#' @param level  "split" returns list(W=..., B=...), "W", "B", or "both" (keeps level
#'               in row/col names).
#' @param digits Number of decimals for numeric formatting.
#' @param stars  Add significance markers based on p (<.001 ***, <.01 **, <.05 *, <.10 †).
#' @param varmap Optional named character vector c(old="New", ...) applied to *unleveled*
#'               IV and DV labels (after removing the "W "/ "B " prefix).
#' @param na_str String for empty cells.
#' @param as_matrix If TRUE, return base matrix; otherwise a tibble with IV as first col.
#'
#' @return A tibble or matrix. If level="split", returns a named list.
coef_matrix <- function(coefs,
                        value = c("est", "est_se", "est_ci", "est_p", "est_se_p"),
                        level = c("split", "both", "W", "B"),
                        digits = 3,
                        stars = TRUE,
                        varmap = NULL,
                        na_str = "",
                        as_matrix = FALSE) {

  value <- match.arg(value)
  level <- match.arg(level)

  stopifnot(all(c("DV","IV","est") %in% names(coefs)))

  suppressPackageStartupMessages({
    require(dplyr, quietly = TRUE)
    require(stringr, quietly = TRUE)
    require(tidyr, quietly = TRUE)
    require(rlang, quietly = TRUE)
  })

  # keep only bona fide regression rows (need both DV & IV)
  x <- coefs %>%
    filter(!is.na(DV), !is.na(IV)) %>%
    mutate(
      lvl = str_extract(DV, "^[WB]"),
      dv_raw = str_trim(str_remove(DV, "^[WB]\\s+")),
      iv_raw = str_trim(str_remove(IV, "^[WB]\\s+"))
    )

  # optional pretty names
  re_labs <- function(v) {
    if (is.null(varmap)) return(v)
    # dplyr::recode with !!! for named vector
    tryCatch(dplyr::recode(v, !!!as.list(varmap)), error = function(e) v)
  }
  x <- x %>%
    mutate(
      dv = re_labs(dv_raw),
      iv = re_labs(iv_raw)
    )

  # ensure CI columns; if absent, approximate normal CI from SE
  if (!all(c("LowerCI","UpperCI") %in% names(x))) {
    x <- x %>%
      mutate(
        LowerCI = ifelse(!is.na(se), est - 1.96 * se, NA_real_),
        UpperCI = ifelse(!is.na(se), est + 1.96 * se, NA_real_)
      )
  }

  # star formatter
  starify <- function(p) {
    if (!stars || is.na(p)) return("")
    if (p < .001) return("***")
    if (p < .01)  return("**")
    if (p < .05)  return("*")
    if (p < .10)  return("\u2020") # dagger
    ""
  }

  fnum <- function(z, d = digits) {
    ifelse(is.na(z), NA_character_, formatC(z, format = "f", digits = d))
  }

  fp <- function(p) {
    if (is.na(p)) return(NA_character_)
    if (p < .001) "<.001" else paste0("=", formatC(p, format="f", digits = 3))
  }

  # build cell strings
  x <- x %>%
    mutate(
      cell_est  = paste0(fnum(est), starify(pval)),
      cell_se   = ifelse(!is.na(se), paste0("(", fnum(se), ")"), NA_character_),
      cell_ci   = ifelse(!is.na(LowerCI) & !is.na(UpperCI),
                         paste0("[", fnum(LowerCI), ", ", fnum(UpperCI), "]"),
                         NA_character_),
      cell_p    = ifelse(!is.na(pval), paste0("p", fp(pval)), NA_character_)
    ) %>%
    mutate(
      cell = dplyr::case_when(
        value == "est"      ~ cell_est,
        value == "est_se"   ~ ifelse(is.na(cell_se), cell_est, paste0(fnum(est), starify(pval), " ", cell_se)),
        value == "est_ci"   ~ ifelse(is.na(cell_ci), cell_est, paste0(fnum(est), starify(pval), " ", cell_ci)),
        value == "est_p"    ~ ifelse(is.na(cell_p),  cell_est, paste0(fnum(est), starify(pval), ", ", cell_p)),
        value == "est_se_p" ~ {
          se_part <- ifelse(is.na(cell_se), "", paste0(" ", cell_se))
          p_part  <- ifelse(is.na(cell_p),  "", paste0(", ", cell_p))
          paste0(fnum(est), starify(pval), se_part, p_part)
        },
        TRUE ~ fnum(est)
      )
    )

  # helper to pivot one level
  make_one <- function(df, keep_level_in_names = FALSE) {
    df2 <- df %>%
      select(lvl, iv, dv, cell) %>%
      distinct()

    if (keep_level_in_names) {
      df2 <- df2 %>%
        mutate(iv = paste(lvl, iv), dv = paste(lvl, dv))
    }

    wide <- df2 %>%
      select(iv, dv, cell) %>%
      tidyr::pivot_wider(names_from = dv, values_from = cell) %>%
      arrange(iv)

    # replace NAs for display
    wide[is.na(wide)] <- na_str

    if (as_matrix) {
      rn <- wide$iv
      wide <- as.matrix(wide[, -1, drop = FALSE])
      rownames(wide) <- rn
    } else {
      colnames(wide)[1] <- "IV"
    }
    wide
  }

  if (level == "W") {
    return(make_one(filter(x, lvl == "W"), keep_level_in_names = FALSE))
  }
  if (level == "B") {
    return(make_one(filter(x, lvl == "B"), keep_level_in_names = FALSE))
  }
  if (level == "both") {
    return(make_one(x, keep_level_in_names = TRUE))
  }
  # level == "split"
  list(
    W = make_one(filter(x, lvl == "W"), keep_level_in_names = FALSE),
    B = make_one(filter(x, lvl == "B"), keep_level_in_names = FALSE)
  )
}