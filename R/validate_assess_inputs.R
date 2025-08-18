#' Validate inputs for auxiliary variable assessment
#'
#' This internal helper function validates the inputs to ensure that the survey design
#' object is correct, that the diagnostic arguments are valid, and that necessary variables
#' are provided for calibration and domain analysis. It also checks that specified register
#' and survey variables exist in the design data and issues appropriate warnings if necessary.
#'
#' @param design A survey design object (e.g., from **survey**) that contains the survey
#'   data and weights.
#' @param df A data frame containing the survey data.
#' @param calibration_formula A formula specifying the auxiliary variables for calibration.
#'   Must be provided if calibration is to be performed.
#' @param calibration_pop_totals A numeric vector or data frame of population totals for the
#'   auxiliary variables used in the calibration. Must be provided if `calibration_formula` is given.
#' @param register_vars A character vector of auxiliary variables to be used in register diagnostics.
#' @param survey_vars A character vector of survey variables to be used in survey diagnostics.
#' @param domain_vars A character vector of domain variables for stratified analysis.
#'   The variables must exist in the survey design data if provided.
#' @param diagnostics A character vector specifying which diagnostics to compute. Allowed values are
#'   "weight_variation", "register_diagnostics", and "survey_diagnostics".
#' @param already_calibrated A logical indicating whether the weights have already been calibrated.
#'   If `TRUE`, the calibration step will be skipped.
#'
#' @return Invisibly returns `NULL`. If there are any validation errors, an error will be thrown.
#'
#' @keywords internal
#' @noRd
.validate_assess_inputs <- function(design,
                                    df,
                                    calibration_formula,
                                    calibration_pop_totals,
                                    register_vars,
                                    survey_vars,
                                    domain_vars,
                                    diagnostics,
                                    already_calibrated) {
  if (!inherits(design, "survey.design")) {
    stop("design must be a survey.design object", call. = FALSE)
  }

  # diagnostics argument sanity
  allowed <- c("weight_variation", "register_diagnostics", "survey_diagnostics")
  if (!all(diagnostics %in% allowed)) {
    bad <- setdiff(diagnostics, allowed)
    stop("Unknown diagnostics: ", paste(bad, collapse = ", "),
      ". Allowed: ", paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }

  # calibration sanity
  if (!already_calibrated && !is.null(calibration_formula)) {
    if (is.null(calibration_pop_totals)) {
      stop("Provide calibration_pop_totals if calibration_formula is used", call. = FALSE)
    }
  }

  # domain vars must exist in design data if provided
  if (!is.null(domain_vars)) {
    dn <- names(design$variables)
    miss <- setdiff(domain_vars, dn)
    if (length(miss)) {
      stop("All domain_vars must exist in the survey design data. Missing: ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Optional: warn if register/survey vars are not in the design data
  if (!is.null(register_vars)) {
    miss <- setdiff(register_vars, names(design$variables))
    if (length(miss)) {
      warning(
        "Some register_vars not found in design data: ",
        paste(miss, collapse = ", ")
      )
    }
  }
  if (!is.null(survey_vars)) {
    miss <- setdiff(survey_vars, names(design$variables))
    if (length(miss)) {
      warning(
        "Some survey_vars not found in design data: ",
        paste(miss, collapse = ", ")
      )
    }
  }

  invisible(NULL)
}
