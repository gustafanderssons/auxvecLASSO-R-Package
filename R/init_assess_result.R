#' Initialize result container
#'
#' This internal helper function initializes a container (list) to store results
#' for weight variation diagnostics, register diagnostics, and survey diagnostics.
#' It also stores the calibrated weights from the provided survey design object.
#'
#' @param design A survey design object (e.g., from **survey**).
#'   This is used to extract the calibrated weights for initialization.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{calibrated_weights}{The calibrated weights extracted from the survey design.}
#'   \item{weight_variation}{An empty list to store weight variation diagnostics.}
#'   \item{register_diagnostics}{A list containing `total` and `by_domain` elements, both initialized as empty lists,
#'     to store register diagnostics.}
#'   \item{survey_diagnostics}{A list containing `total` and `by_domain` elements, both initialized as empty lists,
#'     to store survey diagnostics.}
#' }
#'
#' @keywords internal
#' @noRd
.init_assess_result <- function(design) {
  list(
    calibrated_weights   = stats::weights(design),
    weight_variation     = list(),
    register_diagnostics = list(total = list(), by_domain = list()),
    survey_diagnostics   = list(total = list(), by_domain = list())
  )
}
