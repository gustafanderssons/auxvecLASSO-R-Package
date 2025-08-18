#' Generate population totals for a calibration design matrix
#'
#' Build a fixed model matrix on a population frame and return the
#' column totals needed for calibration (optionally weighted). The function
#' freezes dummy/interaction structure on the population by constructing
#' a \code{terms} object, so downstream use on respondent data can reuse
#' the exact same encoding.
#'
#' @param population_df A data frame containing the calibration population.
#' @param calibration_formula A one-sided formula specifying main effects and interactions
#'   (e.g., \code{~ stype + api00_bin:stype}). The intercept is handled by
#'   \code{include_intercept}.
#' @param weights Optional numeric vector of population weights (length \code{nrow(population_df)}).
#'   If \code{NULL} (default), unweighted totals are computed.
#' @param contrasts Optional named list of contrasts to pass to \code{model.matrix()} (e.g.,
#'   \code{list(stype = contr.treatment)}). If \code{NULL}, the current global
#'   \code{options(contrasts=...)} are used.
#' @param include_intercept Logical; if \code{TRUE} (default) keep the \code{(Intercept)} column
#'   in the totals (it will sum to \code{sum(weights)} or \code{nrow(population_df)} if unweighted).
#' @param sparse Logical; if \code{TRUE}, return the population model matrix internally as a sparse
#'   Matrix while computing totals. (Totals are always returned as a base numeric vector.)
#' @param na_action NA handling passed to \code{model.frame()}; defaults to \code{stats::na.pass}.
#'   Consider \code{stats::na.omit} for stricter behavior.
#' @param drop_zero_cols Logical; if \code{TRUE}, drop columns whose population total is exactly zero.
#'   Default \code{FALSE}. A message is emitted if any zero-total columns are found.
#'
#' @return An object of class \code{"calib_totals"}: a list with
#' \itemize{
#'   \item \code{population_totals}: named numeric vector of column totals
#'   \item \code{levels}: list of factor levels observed in the population (for reproducibility)
#'   \item \code{terms}: the \code{terms} object built on \code{population_df}
#'   \item \code{contrasts}: the contrasts actually used (from the model matrix)
#' }
#'
#' @examples
#' \dontrun{
#' # Example using the API data from the survey package
#' library(survey)
#' data(api) # loads apipop, apisrs, apistrat, etc.
#'
#' # Build a population frame and create some binary fields used in a formula
#' pop <- apipop
#' pop$api00_bin <- as.factor(ifelse(pop$api00 >= 700, "700plus", "lt700"))
#' pop$growth_bin <- as.factor(ifelse(pop$growth >= 0, "nonneg", "neg"))
#' pop$ell_bin <- as.factor(ifelse(pop$ell >= 10, "highELL", "lowELL"))
#' pop$comp.imp_bin <- as.factor(ifelse(pop$comp.imp >= 50, "highComp", "lowComp"))
#' pop$hsg_bin <- as.factor(ifelse(pop$hsg >= 60, "highHSG", "lowHSG"))
#'
#' # A calibration formula with main effects + a few interactions
#' cal_formula <- ~ stype + growth_bin + api00_bin + ell_bin + comp.imp_bin + hsg_bin +
#'   api00_bin:stype + hsg_bin:stype + comp.imp_bin:stype + api00_bin:growth_bin
#'
#' # (Optional) frame weights if available; here we use unweighted totals
#' gp <- generate_population_totals(
#'   population_df        = pop,
#'   calibration_formula  = cal_formula,
#'   include_intercept    = TRUE
#' )
#'
#' # Named totals ready for calibration:
#' head(gp$population_totals)
#'
#' # If you later build a respondent model matrix, reuse gp$terms to ensure alignment:
#' # X_resp <- model.matrix(gp$terms, data = apisrs)
#' # stopifnot(identical(colnames(X_resp), names(gp$population_totals)))
#' }
#'
#' @export
generate_population_totals <- function(population_df,
                                       calibration_formula,
                                       weights = NULL,
                                       contrasts = NULL,
                                       include_intercept = TRUE,
                                       sparse = FALSE,
                                       na_action = stats::na.pass,
                                       drop_zero_cols = FALSE) {
  stopifnot(inherits(calibration_formula, "formula"))

  # Build terms on the population to freeze dummy/interaction structure
  tt <- stats::terms(calibration_formula, data = population_df)

  # Keep levels for reproducibility / later validation
  mf <- stats::model.frame(tt, data = population_df, na.action = na_action)
  lvl_list <- lapply(mf, function(x) if (is.factor(x)) levels(x) else NULL)

  # Build population model matrix WITH the same NA policy (use mf here)
  mm_args <- list(object = tt, data = mf)
  if (!is.null(contrasts)) mm_args$contrasts.arg <- contrasts
  X_pop <- do.call(stats::model.matrix, mm_args)

  # Weights (optional) — subset to rows kept by mf
  if (is.null(weights)) {
    w <- rep(1, nrow(population_df))
  } else {
    if (length(weights) != nrow(population_df)) {
      stop("`weights` length (", length(weights), ") must match nrow(population_df) (", nrow(population_df), ").")
    }
    w <- as.numeric(weights)
  }
  w <- w[as.integer(rownames(mf))]


  # Compute column totals (sparse-safe)
  if (inherits(X_pop, "sparseMatrix")) {
    totals <- as.numeric(Matrix::colSums(X_pop * w))
  } else {
    totals <- as.numeric(colSums(X_pop * w))
  }
  names(totals) <- colnames(X_pop)

  # Optionally drop intercept
  if (!isTRUE(include_intercept) && "(Intercept)" %in% names(totals)) {
    totals <- totals[names(totals) != "(Intercept)"]
  }

  # Optionally drop zero-total columns, with a message
  if (isTRUE(drop_zero_cols)) {
    zero_cols <- names(totals)[!is.na(totals) & totals == 0]
    if (length(zero_cols)) {
      message(
        "Dropping ", length(zero_cols), " zero-total column(s): ",
        paste(zero_cols, collapse = ", ")
      )
      totals <- totals[!(names(totals) %in% zero_cols)]
    }
  } else {
    if (any(totals == 0, na.rm = TRUE)) {
      zero_cols <- names(totals)[!is.na(totals) & totals == 0]
      message(
        "Found ", length(zero_cols), " zero-total column(s) in the population model matrix: ",
        paste(zero_cols, collapse = ", "),
        ". Consider `drop_zero_cols = TRUE` if appropriate."
      )
    }
  }

  # Capture contrasts used
  used_contrasts <- attr(X_pop, "contrasts")

  structure(
    list(
      population_totals = stats::setNames(totals, names(totals)),
      levels            = lvl_list,
      terms             = tt,
      contrasts         = used_contrasts
    ),
    class = "calib_totals"
  )
}
