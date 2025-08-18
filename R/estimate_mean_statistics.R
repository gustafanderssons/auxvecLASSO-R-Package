#' Estimate Survey-Weighted Means (Overall or By Domain)
#'
#' Computes survey-weighted means and standard errors for one or more variables in a
#' survey design, optionally stratified by domains (e.g., regions, groups). If population
#' means are provided, it also calculates the bias and mean squared error (MSE) of the estimates
#' compared to the population means.
#'
#' @param design A survey design object (e.g., from the **survey** package).
#'   The object must contain survey data and weights.
#' @param vars A character vector of variable names to compute means for. These variables must
#'   exist in `design$variables`.
#' @param by Optional. A one-sided formula (e.g., `~region`) or a character vector of domain
#'   variables for stratified analysis. If `NULL`, computes overall means across all data.
#' @param population_means Optional. A named numeric vector with population means (for `by = NULL`)
#'   or a data frame containing domain variables and population means (for `by` not `NULL`). Used
#'   to calculate bias and MSE for each variable.
#'
#' @return A named list where each component is a data frame with the following columns:
#' \describe{
#'   \item{domain columns}{Columns for the domain variables (if `by` is specified).}
#'   \item{variable}{The name of the variable.}
#'   \item{mean}{The survey-weighted mean of the variable.}
#'   \item{se}{The standard error of the survey-weighted mean.}
#'   \item{bias}{The difference between the survey-weighted mean and the population mean. `NA` if
#'     `population_means` is not provided.}
#'   \item{mse}{The mean squared error (MSE) of the survey-weighted mean. `NA` if `population_means`
#'     is not provided.}
#' }
#'
#' @examples
#' ## ============================================================
#' ## Example 1: Overall means with population means (bias/MSE)
#' ## ============================================================
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   set.seed(123)
#'   options(survey.lonely.psu = "adjust")
#'
#'   n <- 200
#'   region <- factor(sample(c("N", "S"), n, replace = TRUE))
#'   sex <- factor(sample(c("F", "M"), n, replace = TRUE))
#'   y <- 10 + 2 * (sex == "M") + rnorm(n, sd = 1.5)
#'   z <- 100 + 5 * (region == "S") + rnorm(n, sd = 3)
#'   w <- runif(n, 0.8, 1.8) * 50
#'   df <- data.frame(region, sex, y, z, w)
#'
#'   des <- survey::svydesign(ids = ~1, weights = ~w, data = df)
#'
#'   ## Named vector of population means for overall case
#'   pop_means <- c(y = 11.2, z = 103.0)
#'
#'   res_overall <- estimate_mean_stats(
#'     design = des,
#'     vars = c("y", "z"),
#'     by = NULL,
#'     population_means = pop_means
#'   )
#'
#'   ## Each element is a one-row data frame
#'   res_overall$y
#'   res_overall$z
#' }
#'
#' ## ============================================================
#' ## Example 2: Domain means by region with population means table
#' ## ============================================================
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   set.seed(456)
#'   options(survey.lonely.psu = "adjust")
#'
#'   n <- 150
#'   region <- factor(sample(c("N", "S"), n, replace = TRUE), levels = c("N", "S"))
#'   sex <- factor(sample(c("F", "M"), n, replace = TRUE))
#'   y <- 12 + 1.5 * (region == "S") + rnorm(n, sd = 1.2)
#'   z <- 95 + 6 * (region == "S") + rnorm(n, sd = 2.5)
#'   w <- runif(n, 0.7, 2.0) * 40
#'   toy <- data.frame(region, sex, y, z, w)
#'
#'   des2 <- survey::svydesign(ids = ~1, weights = ~w, data = toy)
#'
#'   ## Population means by domain must include the domain column(s) + vars
#'   pop_by_region <- data.frame(
#'     region = c("N", "S"),
#'     y = c(12.2, 13.8),
#'     z = c(95.5, 101.0),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   res_by <- estimate_mean_stats(
#'     design = des2,
#'     vars = c("y", "z"),
#'     by = ~region, # or equivalently: by = "region"
#'     population_means = pop_by_region # merged by the 'region' key
#'   )
#'
#'   ## Each element is a data frame with domain rows
#'   res_by$y
#'   res_by$z
#' }
#'
#' @details
#' - Observations with non-finite **weights** are excluded from the analysis globally.
#' - For each variable, observations with non-finite values are also dropped (in addition to the
#'   global weight filter). An error will occur if no valid data remains.
#' - For domain-specific means, `population_means` must include all domain columns and the variable
#'   being estimated. The rows are merged by domain key.
#'
#' @importFrom survey svymean svyby SE
#' @importFrom stats reformulate as.formula pnorm coef
#' @export
estimate_mean_stats <- function(design, vars, by = NULL, population_means = NULL) {
  # helpers
  norm_by <- function(by) {
    if (is.null(by)) {
      return(NULL)
    }
    if (is.character(by)) {
      return(stats::reformulate(by))
    }
    if (inherits(by, "formula")) {
      if (length(by) != 2L) stop("by must be a one-sided formula (e.g., ~region).", call. = FALSE)
      return(by)
    }
    stop("by must be NULL, a one-sided formula, or a character vector.", call. = FALSE)
  }
  stop_bad <- function(msg) stop(msg, call. = FALSE)

  safe_rse <- function(m, se) {
    eps <- .Machine$double.eps
    ifelse(is.finite(m) & is.finite(se) & abs(m) > eps, se / abs(m), NA_real_)
  }
  safe_p_bias <- function(bias, se) {
    if (is.na(bias)) {
      return(NA_real_)
    }
    if (!is.finite(se) || se <= 0) {
      return(ifelse(abs(bias) <= 1e-12, 1, 0))
    }
    z <- bias / se
    2 * stats::pnorm(-abs(z))
  }

  # validate design/vars
  if (!is.list(design) || is.null(design$variables)) {
    stop_bad("design does not look like a survey.design object (missing $variables).")
  }
  des_vars <- names(design$variables)
  missing_vars <- setdiff(vars, des_vars)
  if (length(missing_vars)) stop_bad(sprintf("Variables not found in design: %s", paste(missing_vars, collapse = ", ")))

  # clean weights once
  wts_all <- stats::weights(design)
  if (any(!is.finite(wts_all))) {
    keep_w <- is.finite(wts_all)
    design <- subset(design, keep_w)
  }

  # normalize by + check domain vars exist
  by_formula <- norm_by(by)
  domain_vars <- if (is.null(by_formula)) character(0) else all.vars(by_formula)
  if (length(domain_vars) && !all(domain_vars %in% names(design$variables))) {
    stop_bad(sprintf(
      "Domain vars not found in design: %s",
      paste(setdiff(domain_vars, names(design$variables)), collapse = ", ")
    ))
  }

  if (length(domain_vars) &&
    !is.null(population_means) &&
    is.data.frame(population_means) &&
    !all(domain_vars %in% names(population_means))) {
    stop_bad("`population_means` must include all domain variables used in `by`.")
  }

  results <- vector("list", length(vars))
  names(results) <- vars

  for (v in vars) {
    x <- design$variables[[v]]
    # type-aware valid-row filter
    w <- stats::weights(design)
    keep_v <- if (is.factor(x) || is.character(x) || is.logical(x)) (!is.na(x) & is.finite(w)) else (is.finite(x) & is.finite(w))
    if (!any(keep_v)) stop_bad(sprintf("Variable '%s' has no valid observations with finite weights.", v))
    d_valid <- subset(design, keep_v)

    # materialize a single numeric column and point the formula to it
    mat <- .materialize_var_for_mean(d_valid, v)
    d2 <- mat$design
    fvar <- stats::as.formula(paste0("~", mat$var))

    if (length(domain_vars) == 0L) {
      # overall mean
      sm <- survey::svymean(fvar, d2, na.rm = TRUE)
      est_mean <- as.numeric(stats::coef(sm))
      est_se <- as.numeric(survey::SE(sm))
      est_rse <- safe_rse(est_mean, est_se)

      if (!is.null(population_means) && is.numeric(population_means) && !is.null(names(population_means)) && v %in% names(population_means)) {
        truth <- population_means[[v]]
        bias <- est_mean - truth
        mse <- bias^2 + est_se^2
        p_b <- safe_p_bias(bias, est_se)
      } else {
        bias <- NA_real_
        mse <- NA_real_
        p_b <- NA_real_
      }

      results[[v]] <- data.frame(
        variable = v, mean = est_mean, se = est_se, rse = est_rse,
        bias = bias, mse = mse, p_bias = p_b,
        check.names = FALSE
      )
    } else {
      # by-domain
      sby <- tryCatch(
        survey::svyby(
          formula = fvar,
          by = by_formula,
          design = d2,
          FUN = survey::svymean,
          vartype = "se",
          keep.names = FALSE,
          drop.empty.groups = FALSE,
          na.rm = TRUE
        ),
        error = function(e) stop_bad(sprintf("Error computing domain means for '%s': %s", v, e$message))
      )
      sby <- as.data.frame(sby)

      # locate estimate/se columns
      est_col <- if ("statistic" %in% names(sby)) {
        "statistic"
      } else {
        setdiff(names(sby), c(domain_vars, grep("^se($|\\.)", names(sby), value = TRUE)))[1]
      }
      se_col <- if ("se" %in% names(sby)) "se" else grep("^se($|\\.)", names(sby), value = TRUE)[1]
      if (is.na(est_col) || is.na(se_col) || !all(domain_vars %in% names(sby))) {
        stop_bad("Unexpected svyby output: missing one of domain vars, estimate, or 'se'.")
      }

      out <- sby[, c(domain_vars, est_col, se_col), drop = FALSE]
      names(out)[names(out) == est_col] <- "mean"
      names(out)[names(out) == se_col] <- "se"
      out$variable <- v
      out$rse <- safe_rse(out$mean, out$se)
      rownames(out) <- NULL

      # optional bias/MSE merge when population_means is a data.frame with matching keys
      if (is.data.frame(population_means) &&
        all(domain_vars %in% names(population_means)) &&
        v %in% names(population_means)) {
        pop_df <- population_means[, c(domain_vars, v), drop = FALSE]
        names(pop_df)[names(pop_df) == v] <- "truth"
        out <- merge(out, pop_df, by = domain_vars, all.x = TRUE, sort = FALSE)
        out$bias <- out$mean - out$truth
        out$mse <- out$bias^2 + out$se^2
        out$p_bias <- mapply(safe_p_bias, out$bias, out$se)
        out <- out[, c(
          domain_vars, "variable", "mean", "se", "rse", "bias", "mse", "p_bias",
          setdiff(names(out), c(domain_vars, "variable", "mean", "se", "rse", "bias", "mse", "p_bias"))
        ),
        drop = FALSE
        ]
      } else {
        out$bias <- NA_real_
        out$mse <- NA_real_
        out$p_bias <- NA_real_
      }

      results[[v]] <- out
    }
  }

  results
}
