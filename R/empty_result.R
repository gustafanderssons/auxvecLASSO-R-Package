#' Create an empty result object for auxiliary variable selection
#'
#' This internal helper function generates a standardized empty result structure
#' that is used when there are no valid outcomes or auxiliary variables for modeling.
#' It returns an empty result object with predefined keys for selected variables,
#' lambdas, penalty factors, models, and interaction metadata, ensuring consistency
#' when no valid results are available.
#'
#' @param outcomes Character vector of outcome variable names.
#'   This is used to initialize the result object with named components corresponding
#'   to each outcome, even when no valid outcomes are found.
#'
#' @return A list containing the following components, all initialized to empty or
#'   `NA` values:
#' \describe{
#'   \item{selected_variables}{An empty character vector.}
#'   \item{by_outcome}{A named list of empty lists, one for each outcome in the `outcomes` parameter.}
#'   \item{selected_lambdas}{A named numeric vector of `NA` values, one for each outcome.}
#'   \item{penalty_factors}{An empty numeric vector.}
#'   \item{models}{An empty list.}
#'   \item{goodness_of_fit}{An empty list.}
#'   \item{interaction_metadata}{A list with:
#'     \item{interaction_terms}{An empty character vector.}
#'     \item{main_effects_in_interactions}{An empty character vector.}
#'     \item{full_formula}{`NULL`.}
#'   }
#' }
#'
#' @importFrom stats setNames
#' @keywords internal
#' @noRd
empty_result <- function(outcomes) {
  list(
    selected_variables = character(0),
    by_outcome = stats::setNames(vector("list", length(outcomes)), outcomes),
    selected_lambdas = stats::setNames(rep(NA_real_, length(outcomes)), outcomes),
    penalty_factors = numeric(0),
    models = list(),
    goodness_of_fit = list(),
    interaction_metadata = list(
      interaction_terms = character(0),
      main_effects_in_interactions = character(0),
      full_formula = NULL
    )
  )
}
