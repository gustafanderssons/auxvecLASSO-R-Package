#' Assess Auxiliary vector Calibration and Diagnostics
#'
#' This function assesses the calibration of auxiliary variables in a survey design,
#' performs various diagnostics, and optionally calibrates weights based on a specified
#' calibration formula. It provides diagnostics on weight variation, register data
#' alignment, and survey data alignment. The results are returned as a list of class
#' "assess_aux_vector".
#'
#' The function supports several diagnostic checks, including weight variation
#' diagnostics, register diagnostics (total and by domain), and survey diagnostics
#' (total and by domain).
#'
#' The function may also calibrate survey weights based on a provided calibration formula
#' and population totals. Calibration can be skipped if the weights are already calibrated.
#'
#' @param design A survey design object, typically of class `svydesign`, representing
#'   the survey data design.
#' @param df A data frame containing the survey data to be used in the analysis.
#' @param calibration_formula An optional formula object specifying the auxiliary variables
#'   used for calibration (e.g., \code{~age + gender}). If provided, the weights will
#'   be calibrated.
#' @param calibration_pop_totals An optional list of population totals for the auxiliary
#'   variables in \code{calibration_formula}. This can be used in the calibration process.
#' @param register_vars A character vector specifying the names of the auxiliary variables
#'   from the register data that should be used in the diagnostics. If \code{NULL}, no
#'   register diagnostics will be performed.
#' @param register_pop_means A list containing population means for the register variables.
#'   The list may include a "total" entry for the total population means and/or a "by_domain"
#'   entry for domain-specific population means.
#' @param survey_vars A character vector specifying the names of the survey variables
#'   to be used in the diagnostics. If \code{NULL}, no survey diagnostics will be performed.
#' @param domain_vars A character vector specifying the domain variables used to group
#'   data for domain-specific diagnostics. If \code{NULL}, diagnostics will be computed
#'   for the entire sample rather than for specific domains.
#' @param diagnostics A character vector specifying which diagnostics to compute. Possible
#'   values include:
#'   \itemize{
#'     \item \code{"weight_variation"}: Computes diagnostics related to weight variation.
#'     \item \code{"register_diagnostics"}: Computes diagnostics based on the register data.
#'     \item \code{"survey_diagnostics"}: Computes diagnostics based on the survey data.
#'   }
#'   The default is all three.
#' @param already_calibrated A logical flag indicating whether the weights have already
#'   been calibrated. If \code{TRUE}, the calibration step will be skipped.
#' @param verbose A logical flag indicating whether to print additional messages during
#'   the execution of the function. This can be useful for debugging or monitoring progress.
#'
#' @return A list of class \code{"assess_aux_vector"} containing the results of the
#'   diagnostic assessments. The list includes the following components:
#'   \itemize{
#'     \item \code{weight_variation}: A numeric vector or matrix containing the results
#'       of the weight variation diagnostics.
#'     \item \code{register_diagnostics}: A list containing diagnostics based on the
#'       register data. This may include the total diagnostics and/or domain-specific
#'       diagnostics.
#'     \item \code{survey_diagnostics}: A list containing diagnostics based on the survey
#'       data. This may include the total diagnostics and/or domain-specific diagnostics.
#'   }
#'
#' @examples
#' ## ============================================================
#' ## Example 1: Calibrate weights, then run all diagnostics
#' ##            (register + survey, with a by-domain breakdown)
#' ## ============================================================
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   set.seed(42)
#'   options(survey.lonely.psu = "adjust")
#'
#'   ## --- Simulate a tiny sample
#'   n <- 200
#'   sex <- factor(sample(c("F", "M"), n, replace = TRUE))
#'   sex[1:2] <- c("F", "M")
#'   sex <- factor(sex, levels = c("F", "M"))
#'   region <- factor(sample(c("N", "S"), n, replace = TRUE))
#'   region[1:2] <- c("N", "S")
#'   region <- factor(region, levels = c("N", "S"))
#'   age <- round(rnorm(n, mean = 41, sd = 12))
#'   ## Register variable we have population means for:
#'   reg_income <- 50000 + 2000 * (region == "S") + rnorm(n, sd = 4000)
#'   ## A couple of survey variables to diagnose:
#'   y1 <- 10 + 2 * (sex == "M") + rnorm(n, sd = 2)
#'   y2 <- 100 + 5 * (region == "S") + rnorm(n, sd = 5)
#'   ## Some unequal weights (to make weight-variation meaningful)
#'   w <- runif(n, 0.6, 2.2) * 50
#'
#'   df <- data.frame(sex, region, age, reg_income, y1, y2, w)
#'   design <- survey::svydesign(ids = ~1, weights = ~w, data = df)
#'
#'   ## --- Calibration setup (simple main-effects formula)
#'   ## Model matrix columns will be: (Intercept), sexM, regionS, age
#'   Npop <- 5000
#'   pop_mean_age <- 40
#'   calibration_formula <- ~ sex + region + age
#'   calibration_pop_totals <- c(
#'     "(Intercept)" = Npop,
#'     "sexM"        = round(0.45 * Npop), # 45% of population is male
#'     "regionS"     = round(0.40 * Npop), # 40% in region S
#'     "age"         = pop_mean_age * Npop # totals (mean * N)
#'   )
#'
#'   ## --- Register population means: total + by domain (single register var)
#'   register_vars <- "reg_income"
#'   register_pop_means <- list(
#'     total = c(reg_income = 51000), # overall pop mean
#'     by_domain = list(
#'       region = c(N = 50000, S = 52000) # domain-specific pop means
#'     )
#'   )
#'
#'   out1 <- assess_aux_vector(
#'     design                  = design,
#'     df                      = df,
#'     calibration_formula     = calibration_formula,
#'     calibration_pop_totals  = calibration_pop_totals,
#'     register_vars           = register_vars,
#'     register_pop_means      = register_pop_means,
#'     survey_vars             = c("y1", "y2"),
#'     domain_vars             = c("region"),
#'     diagnostics             = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
#'     already_calibrated      = FALSE,
#'     verbose                 = FALSE
#'   )
#'
#'   ## Peek at key outputs:
#'   out1$weight_variation
#'   out1$register_diagnostics$total
#'   out1$register_diagnostics$by_domain$region
#'   out1$survey_diagnostics$total
#' }
#'
#' ## ============================================================
#' ## Example 2: Skip calibration; survey diagnostics by domain
#' ## ============================================================
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   set.seed(99)
#'   options(survey.lonely.psu = "adjust")
#'
#'   n <- 120
#'   region <- factor(sample(c("N", "S"), n, replace = TRUE))
#'   region[1:2] <- c("N", "S")
#'   region <- factor(region, levels = c("N", "S"))
#'   sex <- factor(sample(c("F", "M"), n, replace = TRUE))
#'   sex[1:2] <- c("F", "M")
#'   sex <- factor(sex, levels = c("F", "M"))
#'   age <- round(rnorm(n, 39, 11))
#'   yA <- rnorm(n, mean = 50 + 3 * (region == "S"))
#'   yB <- rnorm(n, mean = 30 + 1.5 * (sex == "M"))
#'   w <- runif(n, 0.7, 1.8) * 40
#'
#'   toy <- data.frame(region, sex, age, yA, yB, w)
#'   des <- survey::svydesign(ids = ~1, weights = ~w, data = toy)
#'
#'   out2 <- assess_aux_vector(
#'     design = des,
#'     df = toy,
#'     calibration_formula = NULL, # skip calibration
#'     calibration_pop_totals = NULL,
#'     register_vars = NULL, # no register diagnostics
#'     survey_vars = c("yA", "yB"),
#'     domain_vars = "region",
#'     diagnostics = c("weight_variation", "survey_diagnostics"),
#'     already_calibrated = TRUE, # explicitly skip calibration
#'     verbose = FALSE
#'   )
#'
#'   out2$weight_variation
#'   out2$survey_diagnostics$by_domain$region
#' }
#'
#' @details
#' The function supports several diagnostic checks, including weight variation diagnostics, register diagnostics (total and by domain), and survey diagnostics (total and by domain).
#'
#' The function may also calibrate survey weights based on a provided calibration formula and population totals. Calibration can be skipped if the weights are already calibrated.
#'
#' The weight diagnostics contain the following measures:
#' - Descriptive statistics (min, max, median, mean, standard deviation (sd), range, bottom percentile, top percentile)
#' - Inequality measures (coefficient of variation, Gini index, entropy)
#' - Skewness and (excess) kurtosis
#'
#' @seealso \code{\link[survey]{calibrate}} for the calibration function.
#'
#' @importFrom stats setNames
#'
#' @export
assess_aux_vector <- function(
    design,
    df,
    calibration_formula = NULL,
    calibration_pop_totals = NULL,
    register_vars = NULL,
    register_pop_means = NULL,
    survey_vars = NULL,
    domain_vars = NULL,
    diagnostics = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
    already_calibrated = FALSE,
    verbose = FALSE) {
  .validate_assess_inputs(
    design, df,
    calibration_formula, calibration_pop_totals,
    register_vars, survey_vars, domain_vars, diagnostics, already_calibrated
  )

  register_vars <- unique(register_vars)
  survey_vars <- unique(survey_vars)
  domain_vars <- unique(domain_vars)

  # Calibrate weights if requested
  if (!already_calibrated && !is.null(calibration_formula)) {
    if (verbose) message("Preparing calibration inputs...")
    cal <- .prepare_calibration_inputs(design, calibration_formula, calibration_pop_totals)

    # Recompute sample model matrix columns to be explicit about what's usable
    X_samp_tmp <- stats::model.matrix(
      stats::terms(calibration_formula, data = design$variables),
      data = design$variables
    )
    usable_cols <- intersect(colnames(X_samp_tmp), cal$needed)
    n_aux <- length(setdiff(usable_cols, "(Intercept)"))
    has_int <- "(Intercept)" %in% usable_cols

    if (verbose) {
      message(sprintf(
        "Calibrating weights using %d auxiliary variable(s)%s.",
        n_aux, if (has_int) " and intercept" else ""
      ))
    }
    if (n_aux == 0L) {
      stop("No matching auxiliary columns between sample model matrix and population_totals. ",
        "Check factor levels and names.",
        call. = FALSE
      )
    }

    design <- survey::calibrate(
      design,
      formula    = calibration_formula, # keep as a formula for survey::calibrate
      population = cal$pop,
      bounds     = c(0, Inf),
      calfun     = "linear"
    )
    if (verbose) message("Calibration complete.")
  } else if (verbose) {
    message("Skipping calibration step.")
  }

  domain_formula <- .make_domain_formula(domain_vars)
  result <- .init_assess_result(design)

  # Weight variation
  if ("weight_variation" %in% diagnostics) {
    if (verbose) message("Computing weight variation diagnostics...")
    result$weight_variation <- compute_weight_variation(stats::weights(design))
  }

  # Register diagnostics
  if ("register_diagnostics" %in% diagnostics && !is.null(register_vars) && length(register_vars) > 0) {
    if (verbose) message("Computing register diagnostics (total)...")
    pop_means_total <- if (!is.null(register_pop_means)) register_pop_means$total else NULL
    if (is.list(pop_means_total)) {
      tmp <- unlist(pop_means_total, use.names = TRUE)
      if (is.numeric(tmp) && length(tmp)) pop_means_total <- tmp
    }

    result$register_diagnostics$total <- estimate_mean_stats(
      design, register_vars,
      population_means = pop_means_total
    )

    if (!is.null(domain_vars) && !is.null(register_pop_means) && !is.null(register_pop_means$by_domain)) {
      if (verbose) message("Computing register diagnostics (by domain)...")
      result$register_diagnostics$by_domain <- stats::setNames(vector("list", length(domain_vars)), domain_vars)
      for (dom in domain_vars) {
        pop_dom <- NULL
        if (length(register_vars) == 1L && dom %in% names(register_pop_means$by_domain)) {
          pop_dom <- .coerce_pop_means_margin(dom, register_pop_means$by_domain[[dom]], register_vars[1])
        }
        result$register_diagnostics$by_domain[[dom]] <- estimate_mean_stats(
          design, register_vars,
          by = dom, population_means = pop_dom
        )
      }
    }
  }


  # Survey diagnostics
  if ("survey_diagnostics" %in% diagnostics && !is.null(survey_vars) && length(survey_vars) > 0) {
    if (verbose) message("Computing survey diagnostics (total)...")
    result$survey_diagnostics$total <- estimate_mean_stats(design, survey_vars)

    if (!is.null(domain_vars)) {
      if (verbose) message("Computing survey diagnostics (by domain)...")
      result$survey_diagnostics$by_domain <- stats::setNames(vector("list", length(domain_vars)), domain_vars)
      for (dom in domain_vars) {
        result$survey_diagnostics$by_domain[[dom]] <- estimate_mean_stats(
          design, survey_vars,
          by = dom
        )
      }
    }
  }

  if (verbose) message("Diagnostics assessment complete.")

  class(result) <- "assess_aux_vector"
  result
}
