#' Turn a variable into a single numeric column suitable for svymean:
#' - binary-like (logical / 0-1 numeric / 2-level factor/character): 0/1 indicator for the "positive" level
#' - otherwise (numeric): pass through as numeric
#' Returns: list(design = <design with new column>, var = <new column name>)
#'
#' @keywords internal
#' @noRd
.materialize_var_for_mean <- function(design, varname) {
  d <- design
  x <- d$variables[[varname]]

  # local helpers
  pick_positive <- function(x) {
    if (is.logical(x)) {
      return(TRUE)
    }
    ux <- unique(x[!is.na(x)])
    if (is.numeric(x) && all(ux %in% c(0, 1))) {
      return(1)
    }
    labs <- if (is.factor(x)) levels(x) else as.character(sort(ux))
    cand <- c("1", "Yes", "yes", "TRUE", "True", "T")
    hit <- cand[cand %in% labs]
    if (length(hit)) {
      return(hit[[1]])
    }
    if (length(labs)) {
      return(labs[[length(labs)]])
    }
    NA
  }
  is_binary_like <- function(x) {
    if (is.logical(x)) {
      return(TRUE)
    }
    ux <- unique(x[!is.na(x)])
    if (is.numeric(x)) {
      return(all(ux %in% c(0, 1)))
    }
    if (is.factor(x) || is.character(x)) {
      return(length(ux) <= 2)
    }
    FALSE
  }

  tmp <- paste0(".av_ind__", varname)

  if (is_binary_like(x)) {
    if (is.logical(x)) {
      ind <- as.numeric(x) # TRUE=1, FALSE=0
    } else if (is.numeric(x)) {
      ind <- as.numeric(x == 1)
    } else {
      pos <- pick_positive(x)
      ind <- as.numeric(as.character(x) == as.character(pos))
    }
    d$variables[[tmp]] <- ind
    return(list(design = d, var = tmp))
  }

  # non-binary: keep numeric as-is; for non-numeric, coerce to indicator on "positive" level
  if (is.numeric(x)) {
    d$variables[[tmp]] <- x
  } else {
    lvl <- pick_positive(x)
    d$variables[[tmp]] <- as.numeric(as.character(x) == as.character(lvl))
  }
  list(design = d, var = tmp)
}
