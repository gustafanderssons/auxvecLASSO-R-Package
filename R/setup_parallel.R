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
setup_parallel <- function(parallel, verbose = FALSE, max_cores = 4L) {
  if (!parallel) {
    return(NULL)
  }

  if (!requireNamespace("parallelly", quietly = TRUE)) {
    stop("Package 'parallelly' is required for parallel = TRUE.")
  }
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package 'doParallel' is required for parallel = TRUE.")
  }

  # parallelly::availableCores() respects _R_CHECK_LIMIT_CORES_, cgroups, etc.
  # omit = 1 leaves one core for the OS (like detectCores() - 1).
  avail <- parallelly::availableCores(omit = 1L)
  # Coerce and guard
  avail <- as.integer(avail)
  if (is.na(avail) || avail < 1L) avail <- 1L

  n_cores <- min(avail, as.integer(max_cores))
  # Make a robust PSOCK cluster (handles libpaths, Rscript, etc.)
  cl <- parallelly::makeClusterPSOCK(n_cores)

  doParallel::registerDoParallel(cl)
  if (verbose) {
    message(
      "Using ", length(cl), " core",
      if (length(cl) == 1L) "" else "s",
      " for parallel CV."
    )
  }
  cl
}
