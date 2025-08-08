#' Merge selected variables across outcomes
#'
#' Combines the selected variables from multiple outcomes, ensuring that main effects for
#' interaction terms are included, and adds any must-have variables that should always be
#' included in the model. The resulting vector of selected variables is unique, with duplicates
#' removed.
#'
#' @param all_selected A list of character vectors, where each vector contains the selected
#'   variables for a specific outcome.
#' @param colnames_X A character vector of column names from the model matrix. These represent
#'   the predictor variables, including main effects and interaction terms.
#' @param must_have_idx Integer indices of the must-have variables in `colnames_X`. These
#'   variables will be included in the final list, regardless of whether they are selected.
#' @param check_twoway_int Logical; indicates whether interaction terms were considered. If `TRUE`,
#'   main effects for interaction terms will be included in the selected variables.
#'
#' @return A unique character vector of selected variables, ensuring inclusion of main effects
#'   for interactions and must-have variables.
#'
#' @keywords internal
#' @noRd
merge_selected_variables <- function(all_selected, colnames_X, must_have_idx, check_twoway_int) {
  combined <- unique(unlist(all_selected))

  if (check_twoway_int) {
    interaction_terms <- grep(":", combined, value = TRUE)
    main_effects <- unique(unlist(strsplit(interaction_terms, ":")))
    combined <- unique(c(combined, main_effects))
  }

  must_have_names <- colnames_X[must_have_idx]
  unique(c(combined, must_have_names))
}
