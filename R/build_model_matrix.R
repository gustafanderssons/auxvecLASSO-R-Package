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
  # Identify factors in auxiliary variables
  factor_vars <- auxiliary_vars[sapply(df[auxiliary_vars], is.factor)]

  # Create the formula for the model
  formula_str <- if (check_twoway_int) {
    paste0("~ (", paste(auxiliary_vars, collapse = " + "), ")^2 -1")
  } else {
    paste0("~ ", paste(auxiliary_vars, collapse = " + "), " -1")
  }

  # Build the model matrix using the formula
  X <- model.matrix(as.formula(formula_str), data = df)

  # Capture column names of the model matrix
  colnames_X <- colnames(X)

  # Adjust the list of expanded_vars to include factor levels for factors
  expanded_vars <- auxiliary_vars # Start with original auxiliary_vars list

  # Loop through each factor variable and ensure its levels are included
  for (factor_var in factor_vars) {
    # Identify the columns related to the factor (these are the dummy variables)
    factor_cols <- grep(paste0("^", factor_var), colnames_X)
    # Ensure the factor variable itself (e.g., "stype") is included, if it's not already present
    if (!(factor_var %in% expanded_vars)) {
      expanded_vars <- c(expanded_vars, factor_var)
    }
  }

  # Return the updated model matrix and the formula string used to generate it
  list(X = X, formula_str = formula_str)
}
