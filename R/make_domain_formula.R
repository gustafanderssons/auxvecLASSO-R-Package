#' Build domain formula from character vector (or NULL)
#'
#' This internal helper function creates a domain formula from a character vector
#' of domain variable names, or returns `NULL` if the input is `NULL` or an empty vector.
#' The domain formula is used for domain-specific calculations in survey diagnostics.
#'
#' @param domain_vars A character vector of domain variable names. If `NULL` or an empty vector,
#'   the function returns `NULL`.
#'
#' @return A formula object if `domain_vars` is not `NULL` or empty, otherwise `NULL`.
#'
#' @keywords internal
#' @noRd
.make_domain_formula <- function(domain_vars) {
  if (is.null(domain_vars) || length(domain_vars) == 0L) {
    return(NULL)
  }
  stats::reformulate(termlabels = domain_vars, response = NULL)
}
