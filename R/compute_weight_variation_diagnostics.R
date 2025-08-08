#' Compute Summary Statistics and Variation Metrics for Weights
#'
#' Calculates various descriptive statistics and diagnostic metrics for a vector of
#' survey weights, including measures of central tendency, variability, skewness,
#' kurtosis, and more. The function handles edge cases such as missing values,
#' non-finite numbers, and zero mean/variance. It also computes specialized metrics
#' like the Gini index, entropy, and the coefficient of variation.
#'
#' @param weights A numeric vector of weights. Should be non-negative and finite.
#'   The weights can include zero values, but negative or non-finite values will result
#'   in warnings and cause the function to return `NA` for all metrics.
#'
#' @return A named list containing the following statistics and metrics:
#' \describe{
#'   \item{min}{Minimum value of the weights.}
#'   \item{max}{Maximum value of the weights.}
#'   \item{median}{Median value of the weights.}
#'   \item{mean}{Mean value of the weights.}
#'   \item{sd}{Standard deviation of the weights.}
#'   \item{range}{Range of the weights (max - min).}
#'   \item{coefficient_of_variation}{Coefficient of variation, calculated as the
#'         ratio of standard deviation to mean. Undefined if the mean is 0.}
#'   \item{gini_index}{Gini index, a measure of inequality based on the distribution
#'         of the weights. Returns `NA` if the sum of weights is 0.}
#'   \item{entropy}{Entropy of the normalized weights, representing the degree of
#'         uncertainty or disorder in the distribution of weights. Returns `NA` if
#'         the sum of weights is 0.}
#'   \item{skewness}{Skewness (excess) of the weights, a measure of asymmetry in the
#'         distribution. Returns `NA` if the standard deviation is 0.}
#'   \item{kurtosis}{Excess kurtosis of the weights, a measure of the "tailedness"
#'         of the distribution. Returns `NA` if the standard deviation is 0.}
#'   \item{bottom_1pct}{The 1st percentile (bottom 1%) of the weights.}
#'   \item{top_1pct}{The 99th percentile (top 1%) of the weights. This is the maximum
#'         among the top ceil(1% of n) weights, preserving the original intent.}
#' }
#'
#' @examples
#' compute_weight_variation(c(1, 2, 2, 3, 5, 10, 10, 20))
#' # Returns a list of summary statistics and variation metrics for the weights.
#'
#' @keywords internal
#' @noRd
compute_weight_variation <- function(weights) {
  # Helper: return all-NA result with a consistent shape
  na_result <- function() {
    list(
      min = NA_real_,
      max = NA_real_,
      median = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      range = NA_real_,
      coefficient_of_variation = NA_real_,
      gini_index = NA_real_,
      entropy = NA_real_,
      skewness = NA_real_,
      kurtosis = NA_real_,
      bottom_1pct = NA_real_,
      top_1pct = NA_real_
    )
  }

  # Basic validation
  if (length(weights) == 0L) {
    warning("No weights provided; diagnostics not calculated.", call. = FALSE)
    return(na_result())
  }
  if (anyNA(weights) || any(!is.finite(weights))) {
    warning("Weights contain NA/NaN/Inf; diagnostics not calculated.", call. = FALSE)
    return(na_result())
  }
  if (any(weights < 0)) {
    warning("Weights contain negative values; results may be undefined.", call. = FALSE)
  }

  n <- length(weights)
  w_min <- min(weights)
  w_max <- max(weights)
  w_median <- stats::median(weights)
  w_mean <- base::mean(weights)
  w_sd <- stats::sd(weights)
  w_range <- w_max - w_min

  # Coefficient of variation: undefined if mean == 0
  coeff_var <- if (w_mean == 0) NA_real_ else w_sd / w_mean

  # Gini index (sorted closed-form). Requires sum(weights) > 0.
  sum_w <- sum(weights)
  gini_index <- if (sum_w == 0) {
    NA_real_
  } else {
    w_sorted <- sort(weights)
    (2 * sum(seq_len(n) * w_sorted)) / (n * sum_w) - (n + 1) / n
  }

  # Entropy of normalized weights: 0*log(0) := 0
  if (sum_w == 0) {
    entropy <- NA_real_
  } else {
    p <- weights / sum_w
    p_pos <- p[p > 0]
    entropy <- -sum(p_pos * log(p_pos))
  }

  # Skewness & kurtosis (excess): undefined if sd == 0
  if (w_sd == 0) {
    skewness <- NA_real_
    kurtosis <- NA_real_
  } else {
    z <- weights - w_mean
    skewness <- sum(z^3) / (n * w_sd^3)
    kurtosis <- sum(z^4) / (n * w_sd^4) - 3
  }

  # Tail summaries
  bottom_1pct <- as.numeric(stats::quantile(weights, 0.01, names = FALSE, type = 7))
  # "Top 1%" as max among top ceil(1% n) observations (preserves your original intent)
  k_top <- max(1L, ceiling(0.01 * n))
  top_1pct <- max(utils::tail(sort(weights), k_top))

  list(
    min = w_min,
    max = w_max,
    median = w_median,
    mean = w_mean,
    sd = w_sd,
    range = w_range,
    coefficient_of_variation = coeff_var,
    gini_index = gini_index,
    entropy = entropy,
    skewness = skewness,
    kurtosis = kurtosis,
    bottom_1pct = bottom_1pct,
    top_1pct = top_1pct
  )
}
