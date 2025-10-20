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

build_bayes_coefs <- function(model, params) {
  raw <- model$parameters$unstandardized
  if (is.null(raw)) {
    stop("Model does not contain unstandardized parameters.")
  }

  param_tables <- lapply(params, function(param) {
    res <- MplusAutomation::paramExtract(raw, params = param)
    if (is.null(res)) return(NULL)
    res_df <- as.data.frame(res, stringsAsFactors = FALSE)
    res_df$Section <- attr(res, "type")
    res_df$Label <- switch(attr(res, "type"),
                           regression = paste0(gsub("\\.ON", "<-", res_df$paramHeader), res_df$param),
                           loading    = paste0(res_df$param, "<-", gsub("\\.BY|\\.\\|", "", res_df$paramHeader)),
                           undirected = paste0(gsub("\\.WITH", "<->", res_df$paramHeader), res_df$param),
                           expectation = paste0(res_df$param, "<-", gsub("\\.Means|\\.Intercepts|\\.Thresholds", "", res_df$paramHeader)),
                           variability = paste0(res_df$param, "<->", res_df$param),
                           new = res_df$param,
                           res_df$param)
    res_df
  })

  param_tables <- param_tables[!vapply(param_tables, is.null, logical(1))]
  if (!length(param_tables)) {
    return(tibble::tibble(Label = character(),
                          est = numeric(),
                          se = numeric(),
                          pval = numeric(),
                          sig = logical(),
                          LowerCI = numeric(),
                          UpperCI = numeric()))
  }

  out <- dplyr::bind_rows(param_tables)

  extralabels <- rep("", nrow(out))
  if ("LatentClass" %in% names(out)) {
    extralabels <- paste0(extralabels, " C_", out[, "LatentClass"])
  }
  if ("BetweenWithin" %in% names(out)) {
    extralabels <- paste0(extralabels, " ", substr(out[, "BetweenWithin"], 1, 1))
  }
  extralabels <- gsub("(\\s)(.*)$", "\\2", extralabels)
  out$Label <- trimws(paste(extralabels, out$Label))

  if ("posterior_sd" %in% names(out) && !"se" %in% names(out)) {
    out$se <- out$posterior_sd
  }
  if ("posterior_sd" %in% names(out)) {
    out$posterior_sd <- NULL
  }
  if (!"se" %in% names(out)) {
    out$se <- NA_real_
  }

  if ("lower_2.5ci" %in% names(out)) {
    out$LowerCI <- as.numeric(out$lower_2.5ci)
    out$lower_2.5ci <- NULL
  }
  if ("upper_2.5ci" %in% names(out)) {
    out$UpperCI <- as.numeric(out$upper_2.5ci)
    out$upper_2.5ci <- NULL
  }

  numeric_cols <- intersect(c("est", "se", "LowerCI", "UpperCI"), names(out))
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) as.numeric(as.character(x)))

  if ("pval" %in% names(out)) {
    out$pval <- suppressWarnings(as.numeric(out$pval))
  } else {
    out$pval <- NA_real_
  }
  if ("sig" %in% names(out)) {
    out$sig <- as.logical(out$sig)
  } else {
    out$sig <- rep(NA, nrow(out))
  }
  out$pval_note <- ifelse(!is.na(out$pval), "one-tailed", NA_character_)

  missing_ci <- setdiff(c("LowerCI", "UpperCI"), names(out))
  for (col in missing_ci) {
    out[[col]] <- NA_real_
  }

  out <- dplyr::select(out, Label, est, se, pval, sig, LowerCI, UpperCI, pval_note)
  tibble::as_tibble(out)
}

extract_random_slope_map <- function(model) {
  lines <- tryCatch(model$input$model, error = function(e) NULL)
  if (is.null(lines)) {
    return(tibble::tibble(slope = character(), dv = character(), iv = character()))
  }
  lines <- stringr::str_trim(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!stringr::str_starts(lines, "!")]
  lines <- lines[stringr::str_detect(lines, "\\|")]
  if (!length(lines)) {
    return(tibble::tibble(slope = character(), dv = character(), iv = character()))
  }
  pattern <- stringr::regex("^([A-Za-z][A-Za-z0-9_]*)\\s*\\|\\s*([A-Za-z][A-Za-z0-9_]*)\\s+ON\\s+([^;!]+)", ignore_case = TRUE)
  matches <- stringr::str_match(lines, pattern)
  valid <- which(!is.na(matches[, 1]))
  if (!length(valid)) {
    return(tibble::tibble(slope = character(), dv = character(), iv = character()))
  }
  matches <- matches[valid, , drop = FALSE]
  slopes <- toupper(matches[, 2])
  dvs <- toupper(matches[, 3])
  iv_part <- matches[, 4]
  rows <- vector("list", length(slopes))
  for (i in seq_along(slopes)) {
    ivs <- stringr::str_split(stringr::str_trim(iv_part[i]), "\\s+|\\+")[[1]]
    ivs <- ivs[nzchar(ivs)]
    ivs <- stringr::str_replace_all(ivs, "[^A-Za-z0-9_]", "")
    if (!length(ivs)) next
    rows[[i]] <- tibble::tibble(
      slope = slopes[i],
      dv = dvs[i],
      iv = toupper(ivs)
    )
  }
  out <- dplyr::bind_rows(rows)
  if (is.null(out)) {
    return(tibble::tibble(slope = character(), dv = character(), iv = character()))
  }
  dplyr::distinct(out)
}

inject_random_slopes <- function(df, map) {
  if (is.null(map) || !nrow(map)) return(df)
  join_map <- dplyr::rename(map, dv_target = dv, iv_target = iv)
  slope_rows <- df %>%
    dplyr::filter(lvl == "B", dv_raw %in% join_map$slope) %>%
    dplyr::left_join(join_map, by = c("dv_raw" = "slope"))

  if (!nrow(slope_rows)) return(df)

  slope_rows <- slope_rows %>%
    dplyr::filter(
      !is.na(dv_target),
      !is.na(iv_target),
      stringr::str_to_upper(iv_raw) %in% c("INTERCEPTS", "MEANS")
    )

  if (!nrow(slope_rows)) return(df)

  slope_rows <- slope_rows %>%
    dplyr::mutate(
      lvl = "W",
      dv_raw = dv_target,
      iv_raw = iv_target,
      DV = paste("W", dv_target),
      IV = iv_target,
      Label = paste0("W ", dv_target, "<-", iv_target),
      random_slope = TRUE
    ) %>%
    dplyr::select(-dv_target, -iv_target)

  dplyr::bind_rows(df, slope_rows)
}

coef_wrapper <- function(model, label_replace = NULL, params = c('regression'), bayes = NULL, addci = FALSE) {
  if (inherits(model, "mplus.model.list")) {
    if (length(model) == 1) {
      model <- model[[1]]
    } else {
      stop("Multiple models supplied; please pass a single Mplus model object.")
    }
  }

  detect_bayes <- function(m) {
    estimator <- tryCatch(m$input$analysis$estimator, error = function(e) NULL)
    if (is.null(estimator)) return(FALSE)
    toupper(estimator) == "BAYES"
  }

  if (is.null(bayes)) {
    bayes <- detect_bayes(model)
  }

  analysis_type <- tryCatch(model$input$analysis$type, error = function(e) NULL)
  has_random <- !is.null(analysis_type) && any(grepl("RANDOM", toupper(analysis_type), fixed = TRUE))
  random_map <- if (has_random) extract_random_slope_map(model) else tibble::tibble(slope = character(), dv = character(), iv = character())
  params_full <- unique(c(params, if (nrow(random_map)) "expectation" else character()))

  # get coefs
  if (isTRUE(bayes)) {
    testcoefs <- build_bayes_coefs(model, params_full)
    if (!addci) {
      testcoefs <- dplyr::select(testcoefs, -dplyr::any_of(c("LowerCI", "UpperCI")))
    }
  } else {
    testcoefs <- coef(model, params = params_full)
    if (nrow(testcoefs) == 0) {
      return(testcoefs)
    }
    if (isTRUE(addci)) {
      testcoefs <- dplyr::left_join(testcoefs, confint(model, params), by = "Label")
    }
    testcoefs$sig <- rep(NA, nrow(testcoefs))
    testcoefs$pval_note <- NA_character_
  }
  # add DV and IV and replace labels
  testcoefs <- sep_label(testcoefs)
  if(!is.null(label_replace)) {
    testcoefs <-
      testcoefs |> mutate(DV = str_replace_all(DV, label_replace),
                          IV = str_replace_all(IV, label_replace))
  }
  if (nrow(random_map)) {
    if (!is.null(label_replace)) {
      random_map <- random_map |>
        mutate(
          dv = str_replace_all(dv, label_replace),
          iv = str_replace_all(iv, label_replace),
          slope = str_replace_all(slope, label_replace)
        )
    }
    attr(testcoefs, "random_slopes") <- random_map
  } else {
    attr(testcoefs, "random_slopes") <- NULL
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

  random_map <- attr(coefs, "random_slopes")
  x <- inject_random_slopes(x, random_map)

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

  sig_vec <- if ("sig" %in% names(x)) x$sig else rep(NA, nrow(x))

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
    if (!stars) return(rep("", length(p)))
    if (any(!is.na(sig_vec))) {
      out <- rep("", length(p))
      out[which(sig_vec %in% TRUE)] <- "*"
      return(out)
    }
    out <- rep("", length(p))
    na_idx <- is.na(p)
    out[!na_idx & p < .001] <- "***"
    out[!na_idx & p >= .001 & p < .01] <- "**"
    out[!na_idx & p >= .01 & p < .05] <- "*"
    out[!na_idx & p >= .05 & p < .10] <- "\u2020" # dagger
    out
  }

  fnum <- function(z, d = digits) {
    ifelse(is.na(z), NA_character_, formatC(z, format = "f", digits = d))
  }

  fp <- function(p) {
    out <- rep(NA_character_, length(p))
    na_idx <- is.na(p)
    out[!na_idx & p < .001] <- "<.001"
    ge_idx <- !na_idx & p >= .001
    out[ge_idx] <- paste0("=", formatC(p[ge_idx], format = "f", digits = 3))
    out
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
