#' Prepare calibration inputs for survey design
#'
#' This internal helper function prepares the necessary inputs for calibration, including
#' extracting the model matrix from the provided calibration formula and verifying the
#' population totals. It ensures that the population totals are appropriately aligned
#' with the model matrix columns.
#'
#' @param design A survey design object (e.g., from **survey**), containing the survey
#'   data and weights.
#' @param calibration_formula A formula specifying the auxiliary variables to be used
#'   for calibration. It should define the relationship between the auxiliary variables
#'   and the survey data.
#' @param calibration_pop_totals A numeric vector or data frame containing the population
#'   totals for the auxiliary variables specified in `calibration_formula`. It must match
#'   the variables in the model matrix generated from the formula.
#'
#' @return A list containing:
#' \describe{
#'   \item{pop}{A numeric vector or data frame of population totals, appropriately
#'     subset to match the variables used in `calibration_formula`.}
#' }
#'
#' @keywords internal
#' @noRd
.prepare_calibration_inputs <- function(design, calibration_formula, calibration_pop_totals) {
  # Build on the SAMPLE design to know what columns will be used
  mf <- stats::model.frame(calibration_formula, data = design$variables)
  tt <- stats::terms(calibration_formula, data = mf)
  X <- stats::model.matrix(tt, data = mf)
  needed <- colnames(X)

  # Accept named numeric vector or data.frame; subset & order to `needed`
  if (is.numeric(calibration_pop_totals) && !is.null(names(calibration_pop_totals))) {
    missing <- setdiff(needed, names(calibration_pop_totals))
    if (length(missing)) {
      stop("Population totals names must include: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    calibration_pop_totals <- calibration_pop_totals[needed]
  } else if (is.data.frame(calibration_pop_totals)) {
    missing <- setdiff(needed, colnames(calibration_pop_totals))
    if (length(missing)) {
      stop("Population totals data.frame columns must include: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    calibration_pop_totals <- calibration_pop_totals[, needed, drop = FALSE]
  } else {
    stop("calibration_pop_totals must be a named numeric vector or data.frame.", call. = FALSE)
  }

  list(pop = calibration_pop_totals, needed = needed)
}
