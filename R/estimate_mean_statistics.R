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
#' @details
#' - Observations with non-finite **weights** are excluded from the analysis globally.
#' - For each variable, observations with non-finite values are also dropped (in addition to the
#'   global weight filter). An error will occur if no valid data remains.
#' - For domain-specific means, `population_means` must include all domain columns and the variable
#'   being estimated. The rows are merged by domain key.
#'
#' @importFrom survey svymean svyby SE
#' @importFrom stats reformulate as.formula
#' @export
estimate_mean_stats <- function(design, vars, by = NULL, population_means = NULL) {
  # ---- helpers ---------------------------------------------------------------
  norm_by <- function(by) {
    if (is.null(by)) return(NULL)
    if (is.character(by)) return(stats::reformulate(by))
    if (inherits(by, "formula")) {
      # one-sided formulas have length 2; two-sided have length 3
      if (length(by) != 2L) {
        stop("`by` must be a one-sided formula (e.g., ~region) or a character vector.", call. = FALSE)
      }
      return(by)
    }
    stop("`by` must be NULL, a one-sided formula, or a character vector.", call. = FALSE)
  }

  stop_bad <- function(msg) stop(msg, call. = FALSE)

  # ---- validate design/weights/vars -----------------------------------------
  if (!is.list(design) || is.null(design$variables)) {
    stop_bad("`design` does not look like a survey.design object (missing $variables).")
  }

  des_vars <- names(design$variables)
  missing_vars <- setdiff(vars, des_vars)
  if (length(missing_vars)) {
    stop_bad(sprintf("Variables not found in design: %s", paste(missing_vars, collapse = ", ")))
  }

  wts_all <- stats::weights(design)
  if (any(!is.finite(wts_all))) {
    # Drop non-finite weights once
    keep_w <- is.finite(wts_all)
    design <- subset(design, keep_w)
    wts_all <- stats::weights(design)
  }

  # normalize `by` and extract domain columns if any
  by_formula <- norm_by(by)
  domain_vars <- if (is.null(by_formula)) character(0) else all.vars(by_formula)

  # ---- per-variable estimation ----------------------------------------------
  results <- vector("list", length(vars))
  names(results) <- vars

  for (v in vars) {
    # per-variable finite filter
    v_data <- design$variables[[v]]
    keep_v <- is.finite(v_data) & is.finite(stats::weights(design))
    if (!any(keep_v)) {
      stop_bad(sprintf("Variable '%s' has no finite observations with finite weights.", v))
    }
    d_valid <- subset(design, keep_v)

    if (is.null(by_formula)) {
      # overall mean
      sm <- survey::svymean(stats::as.formula(paste0("~", v)), d_valid, na.rm = TRUE)
      est_mean <- as.numeric(coef(sm))
      est_se   <- as.numeric(survey::SE(sm))

      # bias/MSE from vector of population means, if provided
      if (!is.null(population_means) && is.numeric(population_means) && !is.null(names(population_means))) {
        if (!v %in% names(population_means)) {
          bias <- NA_real_
          mse  <- NA_real_
        } else {
          truth <- population_means[[v]]
          bias  <- est_mean - truth
          mse   <- bias^2 + est_se^2
        }
      } else {
        bias <- NA_real_
        mse  <- NA_real_
      }

      out <- data.frame(
        variable = v,
        mean = est_mean,
        se   = est_se,
        bias = bias,
        mse  = mse,
        check.names = FALSE
      )
      results[[v]] <- out

    } else {
      # domain means
      sby <- tryCatch(
        survey::svyby(
          formula = stats::as.formula(paste0("~", v)),
          by      = by_formula,
          design  = d_valid,
          FUN     = survey::svymean,
          vartype = "se",
          drop.empty.groups = FALSE,
          na.rm   = TRUE
        ),
        error = function(e) stop_bad(sprintf("Error computing domain means for '%s': %s", v, e$message))
      )

      # svyby returns columns: domain_vars..., <var>, se
      if (!all(c(domain_vars, v, "se") %in% names(sby))) {
        stop_bad("Unexpected svyby output: missing one of domain vars, estimate, or 'se'.")
      }

      out <- sby[, c(domain_vars, v, "se"), drop = FALSE]
      names(out)[names(out) == v] <- "mean"
      out$variable <- v

      # merge population means by domain keys if given as data.frame
      if (is.data.frame(population_means)) {
        if (!all(domain_vars %in% names(population_means))) {
          stop_bad("`population_means` must include all domain variables used in `by`.")
        }
        if (!v %in% names(population_means)) {
          # no truth column -> leave bias/mse NA
          out$bias <- NA_real_
          out$mse  <- NA_real_
        } else {
          pop_df <- population_means[, c(domain_vars, v), drop = FALSE]
          names(pop_df)[names(pop_df) == v] <- "truth"
          out <- merge(out, pop_df, by = domain_vars, all.x = TRUE, sort = FALSE)
          out$bias <- out$mean - out$truth
          out$mse  <- out$bias^2 + out$se^2
          # keep column order tidy
          out <- out[, c(domain_vars, "variable", "mean", "se", "bias", "mse", setdiff(names(out), c(domain_vars, "variable", "mean", "se", "bias", "mse"))), drop = FALSE]
        }
      } else {
        out$bias <- NA_real_
        out$mse  <- NA_real_
      }

      results[[v]] <- out
    }
  }

  results
}
