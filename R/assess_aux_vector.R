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
#' # Example 1: Assessing diagnostics without calibration
#' result <- assess_aux_vector(
#'   design = survey_design_object,
#'   df = survey_data,
#'   register_vars = c("age", "income"),
#'   survey_vars = c("age", "income"),
#'   diagnostics = c("weight_variation", "register_diagnostics"),
#'   verbose = TRUE
#' )
#'
#' # Example 2: Calibrating weights and assessing diagnostics
#' result <- assess_aux_vector(
#'   design = survey_design_object,
#'   df = survey_data,
#'   calibration_formula = ~age + gender,
#'   calibration_pop_totals = list(age = c(18 = 1000, 25 = 1200)),
#'   register_vars = c("age", "gender"),
#'   survey_vars = c("income"),
#'   diagnostics = c("weight_variation", "survey_diagnostics"),
#'   verbose = TRUE
#' )
#'
#' @seealso \code{\link[survey]{calibrate}} for the calibration function.
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

  # Calibrate weights if requested
  if (!already_calibrated && !is.null(calibration_formula)) {
    if (verbose) message("Preparing calibration inputs...")
    cal <- .prepare_calibration_inputs(design, calibration_formula, calibration_pop_totals)
    if (verbose) {
      message("Calibrating weights using ", length(cal$needed) - 1, " auxiliary variable(s) ",
              "and intercept.")
    }
    design <- survey::calibrate(design, formula = calibration_formula,
                                population = cal$pop, bounds = c(0, Inf),
                                calfun = "linear")
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
    result$register_diagnostics$total <- estimate_mean_stats(
      design, register_vars, population_means = pop_means_total
    )

    if (!is.null(domain_formula) && !is.null(register_pop_means) && !is.null(register_pop_means$by_domain)) {
      if (verbose) message("Computing register diagnostics (by domain)...")
      result$register_diagnostics$by_domain <- estimate_mean_stats(
        design, register_vars, by = domain_formula, population_means = register_pop_means$by_domain
      )
    }
  }

  # Survey diagnostics
  if ("survey_diagnostics" %in% diagnostics && !is.null(survey_vars) && length(survey_vars) > 0) {
    if (verbose) message("Computing survey diagnostics (total)...")
    result$survey_diagnostics$total <- estimate_mean_stats(design, survey_vars)

    if (!is.null(domain_formula)) {
      if (verbose) message("Computing survey diagnostics (by domain)...")
      result$survey_diagnostics$by_domain <- estimate_mean_stats(design, survey_vars, by = domain_formula)
    }
  }

  if (verbose) message("Diagnostics assessment complete.")

  class(result) <- "assess_aux_vector"
  result
}
