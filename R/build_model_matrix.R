#' Build model matrix for auxiliary variables
#'
#' Constructs a model matrix from the specified auxiliary variables,
#' optionally including two-way interactions between them.
#'
#' This function is primarily used internally for building the necessary
#' matrix for modeling auxiliary variables. If `check_twoway_int` is `TRUE`,
#' the function will include all two-way interactions among the specified
#' auxiliary variables. The output is a model matrix ready for use in statistical models.
#'
#' @param df Data frame containing the auxiliary variables.
#' @param auxiliary_vars Character vector of auxiliary variable names to be used as predictors in the model.
#' @param check_twoway_int Logical; whether to include two-way interactions between the auxiliary variables.
#'   If `TRUE`, interactions of all pairs of auxiliary variables are added to the model matrix.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{X}{A model matrix created from the auxiliary variables and interactions (if `check_twoway_int` is `TRUE`).}
#'   \item{formula_str}{A string representing the formula used to generate the model matrix.}
#' }
#'
#' @keywords internal
#' @noRd
build_model_matrix <- function(df, auxiliary_vars, check_twoway_int) {
  formula_str <- if (check_twoway_int) {
    paste0("~ (", paste(auxiliary_vars, collapse = " + "), ")^2 -1")
  } else {
    paste0("~ ", paste(auxiliary_vars, collapse = " + "), " -1")
  }
  X <- model.matrix(as.formula(formula_str), data = df)
  list(X = X, formula_str = formula_str)
}
