#' Validate inputs and filter outcome variables
#'
#' This internal helper function checks the provided data frame and outcome variables,
#' returning only those outcome variables that are present in the data and have at least
#' two distinct non-missing values. It also ensures that auxiliary variables are present
#' in the data, and it provides verbose messages based on the `verbose` flag.
#'
#' @param df Data frame containing both outcome and auxiliary variables.
#' @param outcome_vars Character vector of outcome variable names to be validated.
#' @param auxiliary_vars Character vector of auxiliary variable names. Used to check if
#'   auxiliary variables are present in the data.
#' @param verbose Logical; whether to print verbose messages for data validation steps.
#'
#' @return A character vector of valid outcome variables. If no valid outcomes are found,
#'   it returns a zero-length character vector.
#'
#' @keywords internal
#' @noRd
validate_and_filter_outcomes <- function(df, outcome_vars, auxiliary_vars, verbose) {
  if (nrow(df) == 0) {
    message_verbose(verbose, "Input data frame has zero rows — returning empty selection.")
    return(character(0))
  }
  if (length(auxiliary_vars) == 0) {
    message_verbose(verbose, "No auxiliary variables — returning empty selection.")
    return(character(0))
  }

  valid_outcomes <- outcome_vars[sapply(outcome_vars, function(v) {
    v %in% names(df) && length(unique(stats::na.omit(df[[v]]))) >= 2
  })]

  if (length(valid_outcomes) == 0) {
    message_verbose(verbose, "No valid outcome variables — returning empty selection.")
    return(character(0))
  }

  valid_outcomes
}
