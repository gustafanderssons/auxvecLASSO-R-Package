#' Setup Parallel Backend for Cross-Validation
#'
#' Initializes and registers a parallel backend using the \code{doParallel} package
#' if parallel execution is requested.
#'
#' @param parallel Logical flag indicating whether to enable parallel processing.
#' @param verbose Logical flag indicating whether to print informative messages.
#'
#' @return A cluster object if parallel is \code{TRUE}, otherwise \code{NULL}.
#'
#' @keywords internal
#' @noRd
setup_parallel <- function(parallel, verbose = FALSE) {
  if (!parallel) {
    return(NULL)
  }
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package 'doParallel' is required for parallel = TRUE.")
  }
  n_cores <- max(parallel::detectCores() - 1, 1)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  if (verbose) message("Using ", n_cores, " cores for parallel CV.")
  cl
}
