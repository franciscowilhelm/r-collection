slideExtract <- function(data, Var, TimeVar, GroupVar, NewVar, slideBy, keepInvalid = FALSE, reminder = TRUE) {
  x <- DataCombine::slide(data = data, Var = Var, TimeVar = TimeVar, GroupVar = GroupVar, 
                          NewVar = NewVar, slideBy = slideBy, keepInvalid = keepInvalid, reminder)
  out <- dplyr::select(x, NewVar) }
  
  slideRedux <- function(data, Var, TimeVar, GroupVar, NewVar, slideBy = -1, keepInvalid = FALSE, reminder = TRUE, append = TRUE) {
  # extract the lagged/leaded variable from DataCombine::slide, modified the code a bit. See DataCombine::slide() for documentation.
  # append = TRUE / FALSE determines whether to append NewVar to data, or just to return NewVar when append = FALSE.
  # shift variable used internally
  shift <- function(VarVect, shiftBy, reminder = TRUE)  {
    if (!is.numeric(shiftBy)) {
      stop(paste(shiftBy, "must be numeric."), call. = FALSE)
    }
    if (isTRUE(reminder)) {
      message(paste("Remember to put", deparse(substitute(data)), 
                    "in time order before running shift."))
    }
    if (length(shiftBy) > 1) 
      return(sapply(shiftBy, shift, Var = VarVect))
    out <- NULL
    abs_shiftBy = abs(shiftBy)
    if (shiftBy > 0) {
      out <- c(tail(VarVect, -abs_shiftBy), rep(NA, abs_shiftBy))
    }
    else if (shiftBy < 0) {
      out <- c(rep(NA, abs_shiftBy), head(VarVect, -abs_shiftBy))
    }
    else {
      out <- VarVect
    }
    return(out)
  }
  
  # check if all variables exist
  TestExist <- c(Var,TimeVar, GroupVar) %in% names(data)
  if (!all(TestExist)) {
    stop(paste("A variable was not found in the data frame."), 
         call. = FALSE)
  }
  
  # select only necessary variables too speed up function
  x <- data %>% dplyr::select(all_of(c(Var, TimeVar, GroupVar)))
  fake <- total <- FullData <- NULL
  if (!missing(GroupVar) & "data.table" %in% class(x))
    stop(
      paste(
        "slide does not support data.tables with Grouped variables.\n",
        "Convert to data.frame and try again."
      ),
      call. = F
    )
  if (!missing(GroupVar) & "tbl_df" %in% class(x)) {
    message("Converting to plain data frame from tbl_df.")
    x <- as.data.frame(x)
  }
  
  if (!missing(TimeVar))
    x[order(x[, Var], x[, TimeVar]),]
  VarVect <- x[, Var]
  if (missing(NewVar)) {
    NewVar <- paste0(Var, slideBy)
  }
  if (isTRUE(keepInvalid) & missing(GroupVar)) {
    warning("keepInvalid set to FALSE when GroupVar is missing.")
    keepInvalid <- FALSE
  }
  if (isTRUE(reminder)) {
    if (missing(TimeVar)) {
      if (missing(GroupVar)) {
        message(paste(
          "\nRemember to put",
          deparse(substitute(x)),
          "in time order before running."
        ))
      }
      if (!missing(GroupVar)) {
        message(
          paste(
            "\nRemember to order",
            deparse(substitute(x)),
            "by",
            GroupVar,
            "and the time variable before running."
          )
        )
      }
    }
    if (slideBy < 0) {
      message(paste("\nLagging", Var, "by", abs(slideBy),
                    "time units.\n"))
    }
    else if (slideBy > 0) {
      message(paste("\nLeading", Var, "by", abs(slideBy),
                    "time units.\n"))
    }
  }
  if (!missing(GroupVar)) {
    x <- group_by_(x, .dots = GroupVar)
    x$fake <- 1
    Minimum <- abs(slideBy) - 1
    Summed <- dplyr::mutate(x, total = sum(fake))
    SubSummed <- subset(Summed, total <= Minimum) %>% data.frame()
    x <- x %>% select(-fake)
    if (nrow(SubSummed) == 0) {
      FullData <- NULL
    }
    else if (nrow(SubSummed) > 0) {
      FullData <- x
      class(FullData) <- "data.frame"
      Dropping <- unique(SubSummed[, GroupVar])
      class(x) <- "data.frame"
      x <- x[!(x[, GroupVar] %in% Dropping),]
      x <- group_by_(x, .dots = GroupVar)
      if (!isTRUE(keepInvalid)) {
        message(
          paste0(
            "\nWarning: the following groups have ",
            Minimum,
            " or fewer observations.",
            "\nNo valid lag/lead can be created, so they are dropped:\n"
          )
        )
        message(paste(Dropping, collapse = "\n"))
        message("\n")
      }
      else if (isTRUE(keepInvalid)) {
        message(
          paste0(
            "\nWarning: the following groups have ",
            Minimum,
            " or fewer observations.",
            "\n  No valid lag/lead can be created.",
            "\n  NA will be returned for these observations in the new lag/lead variable.",
            "\n  They will be returned at the bottom of the data frame.\n"
          )
        )
        message(paste(Dropping, collapse = "\n"))
        message("\n")
      }
    }
  }
  if (missing(GroupVar)) {
    x[, NewVar] <- shift(VarVect = VarVect,
                            shiftBy = slideBy,
                            reminder = FALSE)
  }
  else if (!missing(GroupVar)) {
    DataSub <-
      eval(parse(text = paste0(
        "group_by(x[, c(GroupVar, Var)], ",
        GroupVar, ")"
      )))
    vars <-
      eval(parse(
        text = paste0(
          "dplyr::mutate(DataSub, NewVarX = shift(",
          Var,
          ",",
          slideBy,
          ", reminder = FALSE))"
        )
      ))
    x[, NewVar] <- vars$NewVarX
  }
  if (isTRUE(keepInvalid) & !is.null(FullData)) {
    invalid <- FullData[(FullData[, GroupVar] %in% Dropping),]
    invalid[, NewVar] <- NA
    x <- bind_rows(x, invalid)
  }
  x <- ungroup(x)
  class(x) <- "data.frame"
  
  if(append == TRUE) {
    return(bind_cols(data, dplyr::select(x, all_of(NewVar))))
  }
  else if(append == FALSE ) {
    return(dplyr::select(x, all_of(NewVar)))
  }
}