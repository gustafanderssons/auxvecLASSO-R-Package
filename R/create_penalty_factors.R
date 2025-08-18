#' Create penalty factors for LASSO (with robust interaction handling)
#'
#' @importFrom stats setNames
#'
#' @keywords internal
#' @noRd
create_penalty_factors <- function(colnames_X, must_have_vars = NULL, check_twoway_int = TRUE) {
  penalty_factors <- stats::setNames(rep(1, length(colnames_X)), colnames_X)
  if (is.null(must_have_vars) || length(must_have_vars) == 0L) {
    return(penalty_factors)
  }

  names_lc <- tolower(names(penalty_factors))
  mv_lc <- tolower(must_have_vars)

  int_idx <- grep(":", names_lc, fixed = TRUE)

  canon <- function(s) paste(sort(strsplit(s, ":", fixed = TRUE)[[1]]), collapse = ":")
  canon_names <- if (length(int_idx)) vapply(int_idx, function(i) canon(names_lc[i]), character(1)) else character(0)

  esc <- function(s) gsub("([\\^$.*+?()\\[\\]{}|\\\\])", "\\\\\\1", s, perl = TRUE)

  matched <- rep(FALSE, length(mv_lc)) # track which must-have entries matched
  matched_main <- character(0) # <-- INIT THIS

  for (k in seq_along(mv_lc)) {
    mv <- mv_lc[k]
    if (grepl(":", mv, fixed = TRUE)) {
      if (check_twoway_int && length(int_idx)) {
        target <- canon(mv)
        hit <- int_idx[canon_names == target]
        if (length(hit)) {
          penalty_factors[hit] <- 0
          matched[k] <- TRUE
        }
      } else {
        hit <- which(names_lc == mv)
        if (length(hit)) {
          penalty_factors[hit] <- 0
          matched[k] <- TRUE
        }
      }
    } else {
      # main-effect must-have
      hit_eq <- which(names_lc == mv)
      patt <- paste0("^", esc(mv), "[^:]*$") # starts with var, then no colon
      hit_dm <- grep(patt, names_lc, perl = TRUE)

      if (length(hit_eq) || length(hit_dm)) {
        penalty_factors[unique(c(hit_eq, hit_dm))] <- 0
        matched[k] <- TRUE
        matched_main <- union(matched_main, mv) # <-- RECORD MAIN EFFECT NAME
      }
    }
  }

  # propagate to 2-way interactions between matched main effects
  if (check_twoway_int && length(int_idx) && length(matched_main) >= 2) {
    for (i in int_idx) {
      parts <- strsplit(names_lc[i], ":", fixed = TRUE)[[1]]
      if (length(parts) == 2 && all(parts %in% matched_main)) {
        penalty_factors[i] <- 0
      }
    }
  }

  if (any(!matched)) {
    warning(sprintf(
      "No columns matched for must_have_vars: %s",
      paste(must_have_vars[!matched], collapse = ", ")
    ), call. = FALSE)
  }

  penalty_factors
}
