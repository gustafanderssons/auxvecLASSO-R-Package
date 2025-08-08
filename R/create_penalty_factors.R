#' Create penalty factors for LASSO
#'
#' Generates a numeric vector of penalty factors for each column in the model matrix.
#' Must-have variables are assigned a penalty factor of 0 to ensure they are always included
#' in the LASSO model, while other variables receive a penalty factor of 1. If interactions
#' are included, the penalty factors for interaction terms involving must-have variables
#' can also be set to 0.
#'
#' @param colnames_X Character vector of column names from the model matrix.
#'   These correspond to the terms in the model, including main effects and interactions.
#' @param must_have_vars Character vector of variable names that must always be included
#'   in the LASSO model (assigned a penalty factor of 0). Interactions involving these variables
#'   may also receive zero penalty if specified.
#' @param include_interactions Logical; if `TRUE`, interaction terms that involve must-have
#'   variables will also receive a penalty factor of 0.
#'
#' @return A numeric vector of penalty factors where:
#' \describe{
#'   \item{0}{Assigned to must-have variables and their interaction terms (if `include_interactions = TRUE`).}
#'   \item{1}{Assigned to other variables that are not must-have or involved in interactions with must-have variables.}
#' }
#'
#' @keywords internal
#' @noRd
create_penalty_factors <- function(colnames_X,
                                   must_have_vars = NULL,
                                   include_interactions = TRUE) {
  penalty_factors <- rep(1, length(colnames_X))
  names(penalty_factors) <- colnames_X  # <-- add names

  if (!is.null(must_have_vars) && length(must_have_vars) > 0) {
    # Case-insensitive match
    colnames_lower <- tolower(colnames_X)
    must_lower <- tolower(must_have_vars)

    # Always include exact matches
    idx <- which(colnames_lower %in% must_lower)

    # Optionally include interactions containing must-have vars
    if (include_interactions) {
      interaction_hits <- which(vapply(
        colnames_lower,
        function(cn) any(must_lower %in% strsplit(cn, ":", fixed = TRUE)[[1]]),
        logical(1)
      ))
      idx <- unique(c(idx, interaction_hits))
    }

    if (length(idx) == 0) {
      warning("No must-have variables found in model matrix columns.")
    }

    penalty_factors[idx] <- 0
  }

  penalty_factors
}
