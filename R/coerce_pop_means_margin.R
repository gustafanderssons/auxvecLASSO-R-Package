#' Coerce domain population means to a two-column data frame
#'
#' @seealso estimate_mean_stats, assess_aux_vector
#'
#' @importFrom stats setNames
#'
#' @keywords internal
#' @noRd
.coerce_pop_means_margin <- function(dom, vec, varname) {
  if (is.null(vec)) {
    return(NULL)
  }
  if (is.numeric(vec) && !is.null(names(vec))) {
    out <- data.frame(
      stats::setNames(list(names(vec)), dom),
      stats::setNames(list(as.numeric(vec)), varname),
      check.names = FALSE
    )
    rownames(out) <- NULL
    return(out)
  }
  # if already a data.frame with dom + var columns, pass through
  if (is.data.frame(vec) && all(c(dom, varname) %in% names(vec))) {
    return(vec[, c(dom, varname), drop = FALSE])
  }
  NULL
}
