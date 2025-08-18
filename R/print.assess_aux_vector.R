#' Print Summary of Auxiliary Vector Assessment
#'
#' S3 print method for objects of class \code{assess_aux_vector}. Displays a
#' formatted, colorized summary of weight variation metrics, register diagnostics
#' (overall and by domain), and survey diagnostics (overall and by domain).
#'
#' In addition to means and standard errors, the printer shows the
#' \strong{relative standard error} (\strong{RSE} = SE / |mean|) and—when population
#' means are supplied to \code{estimate_mean_stats()}—two-sided p-values for the bias
#' testing \eqn{H_0:\ \text{mean} = \text{population mean}}.
#'
#' Requires the \pkg{crayon} package for colored output.
#'
#' @param x An object of class \code{assess_aux_vector} containing diagnostic results.
#'   Expected to have components:
#'   \describe{
#'     \item{weight_variation}{Named numeric vector or list of weight variation metrics.}
#'     \item{register_diagnostics}{List with \code{total} and \code{by_domain} components.
#'       Each inner data frame typically includes columns \code{variable}, \code{mean},
#'       \code{se}, \code{rse}, \code{bias}, \code{mse}, and (when population means are available)
#'       \code{p_bias}.}
#'     \item{survey_diagnostics}{List with \code{total} and \code{by_domain} components.
#'       Each inner data frame typically includes columns \code{variable}, \code{mean},
#'       \code{se}, \code{rse}; \code{bias}, \code{mse}, and \code{p_bias} are \code{NA}
#'       unless population means were provided.}
#'   }
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' The print method outputs sections with colored headers for easier readability:
#' \itemize{
#'   \item Weight Variation Metrics
#'   \item Register Diagnostics summarized for all units and by domain
#'   \item Survey Diagnostics summarized for all units and by domain
#' }
#' For each variable shown, the following metrics are printed when present:
#' \itemize{
#'   \item \strong{Mean} — survey-weighted mean.
#'   \item \strong{SE} — design-based standard error from \pkg{survey}.
#'   \item \strong{RSE} — relative standard error, \eqn{\mathrm{SE}/|\mathrm{Mean}|}.
#'   \item \strong{Bias} — difference between estimate and population mean (if supplied).
#'   \item \strong{MSE} — \eqn{\mathrm{Bias}^2 + \mathrm{SE}^2} (if population means supplied).
#'   \item \strong{p(Bias)} — two-sided p-value testing \eqn{H_0:\ \mathrm{Bias}=0},
#'     computed as \eqn{2\Phi(-|z|)} with \eqn{z=\mathrm{Bias}/\mathrm{SE}} (shown when
#'     population means are available).
#' }
#'
#' Edge cases: if \code{mean == 0} the RSE is reported as \code{NA}; if \code{SE == 0},
#' \code{p(Bias)} is \code{1} when \code{|Bias|} is numerically zero and \code{0} otherwise.
#' Objects created with earlier versions that lack \code{rse} or \code{p_bias} columns
#' are handled gracefully (those fields are simply not printed).
#'
#' If the \pkg{crayon} package is not installed, the function will stop with an error.
#'
#' @examples
#' ## ============================================================
#' ## Example 1: Print with register + survey diagnostics
#' ##            (includes population means -> prints p(Bias))
#' ## ============================================================
#' if (requireNamespace("survey", quietly = TRUE) &&
#'   requireNamespace("crayon", quietly = TRUE)) {
#'   set.seed(7)
#'   options(survey.lonely.psu = "adjust")
#'
#'   ## --- Simulate a small survey
#'   n <- 180
#'   sex <- factor(sample(c("F", "M"), n, replace = TRUE), levels = c("F", "M"))
#'   region <- factor(sample(c("N", "S"), n, replace = TRUE), levels = c("N", "S"))
#'   age <- round(rnorm(n, mean = 42, sd = 12))
#'   reg_income <- 52000 + 1500 * (region == "S") + rnorm(n, sd = 3500) # register var
#'   y1 <- 10 + 1.8 * (sex == "M") + rnorm(n, sd = 2) # survey vars
#'   y2 <- 95 + 4.5 * (region == "S") + rnorm(n, sd = 3.5)
#'   w <- runif(n, 0.7, 2.1) * 40
#'   df <- data.frame(sex, region, age, reg_income, y1, y2, w)
#'   des <- survey::svydesign(ids = ~1, weights = ~w, data = df)
#'
#'   ## --- Optional calibration inputs (simple main effects)
#'   ## Model matrix columns: (Intercept), sexM, regionS, age
#'   Npop <- 4000
#'   calibration_formula <- ~ sex + region + age
#'   calibration_pop_totals <- c(
#'     "(Intercept)" = Npop,
#'     "sexM"        = round(0.48 * Npop),
#'     "regionS"     = round(0.52 * Npop),
#'     "age"         = 41 * Npop
#'   )
#'
#'   ## --- Population means for the register var: total + by domain
#'   register_vars <- "reg_income"
#'   register_pop_means <- list(
#'     total = c(reg_income = 52500),
#'     by_domain = list(
#'       region = c(N = 51500, S = 53500)
#'     )
#'   )
#'
#'   ## --- Build assessment object
#'   aux1 <- assess_aux_vector(
#'     design                 = des,
#'     df                     = df,
#'     calibration_formula    = calibration_formula,
#'     calibration_pop_totals = calibration_pop_totals,
#'     register_vars          = register_vars,
#'     register_pop_means     = register_pop_means,
#'     survey_vars            = c("y1", "y2"),
#'     domain_vars            = "region",
#'     diagnostics            = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
#'     already_calibrated     = FALSE,
#'     verbose                = FALSE
#'   )
#'
#'   ## Colorized, formatted summary:
#'   print(aux1)
#' }
#'
#' ## ============================================================
#' ## Example 2: Print with survey diagnostics only (by domain)
#' ##            (no population means -> p(Bias) omitted)
#' ## ============================================================
#' if (requireNamespace("survey", quietly = TRUE) &&
#'   requireNamespace("crayon", quietly = TRUE)) {
#'   set.seed(11)
#'   options(survey.lonely.psu = "adjust")
#'
#'   n <- 120
#'   region <- factor(sample(c("N", "S"), n, replace = TRUE), levels = c("N", "S"))
#'   sex <- factor(sample(c("F", "M"), n, replace = TRUE), levels = c("F", "M"))
#'   yA <- 50 + 2.5 * (region == "S") + rnorm(n, sd = 2)
#'   yB <- 30 + 1.5 * (sex == "M") + rnorm(n, sd = 1.5)
#'   w <- runif(n, 0.8, 1.9) * 35
#'   toy <- data.frame(region, sex, yA, yB, w)
#'
#'   des2 <- survey::svydesign(ids = ~1, weights = ~w, data = toy)
#'
#'   aux2 <- assess_aux_vector(
#'     design                 = des2,
#'     df                     = toy,
#'     calibration_formula    = NULL, # skip calibration
#'     calibration_pop_totals = NULL,
#'     register_vars          = NULL, # no register diagnostics
#'     survey_vars            = c("yA", "yB"),
#'     domain_vars            = "region",
#'     diagnostics            = c("weight_variation", "survey_diagnostics"),
#'     already_calibrated     = TRUE,
#'     verbose                = FALSE
#'   )
#'
#'   print(aux2)
#' }
#'
#' @export
print.assess_aux_vector <- function(x, ...) {
  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop("Package 'crayon' must be installed for colored output")
  }

  fmt <- function(z) ifelse(is.na(z), "        NA", sprintf("%10.6f", z))

  cat(crayon::blue$bold("\n===================================\n"))
  cat(crayon::blue$bold("Auxiliary Vector Assessment Summary\n"))
  cat(crayon::blue$bold("===================================\n\n"))

  ## Weight Variation
  cat(crayon::green$bold("Weight Variation Metrics:\n"))
  if (length(x$weight_variation)) {
    for (metric in names(x$weight_variation)) {
      cat(sprintf("  %-25s %s\n", metric, fmt(x$weight_variation[[metric]])))
    }
  }
  cat("\n")

  ## helper to print TOTAL blocks
  .print_totals <- function(lst, title) {
    cat(crayon::green$bold(title, "\n"), sep = "")
    if (is.null(lst) || !length(lst)) {
      cat("  (none)\n\n")
      return()
    }
    for (varname in names(lst)) {
      df <- lst[[varname]]
      cat(crayon::yellow$bold("  - ", varname, " :\n"), sep = "")
      for (i in seq_len(nrow(df))) {
        cat(sprintf("      Mean:    %s\n", fmt(df$mean[i])))
        cat(sprintf("      SE:      %s\n", fmt(df$se[i])))
        if ("rse" %in% names(df)) cat(sprintf("      RSE:     %s\n", fmt(df$rse[i])))
        cat(sprintf("      Bias:    %s\n", fmt(df$bias[i])))
        cat(sprintf("      MSE:     %s\n", fmt(df$mse[i])))
        if ("p_bias" %in% names(df)) cat(sprintf("      p(Bias): %s\n", fmt(df$p_bias[i])))
      }
    }
    cat("\n")
  }

  ## helper to print BY-DOMAIN blocks
  .print_by_domain <- function(by_obj, title) {
    cat(crayon::green$bold(title, "\n"), sep = "")
    if (is.null(by_obj) || !length(by_obj)) {
      cat("  (none)\n\n")
      return()
    }

    # Two supported shapes:
    # NEW (recommended): by_obj is a list keyed by domain var, each an inner list keyed by variable
    # OLD: by_obj is a list keyed by variable, each a data.frame with a single domain column
    is_new_shape <- {
      first <- by_obj[[1]]
      is.list(first) && !is.data.frame(first)
    }

    metric_cols <- c("variable", "mean", "se", "rse", "bias", "mse", "p_bias", "truth")

    if (is_new_shape) {
      for (dom in names(by_obj)) {
        varlist <- by_obj[[dom]]
        cat(crayon::magenta$bold("  * by ", dom, ":\n"), sep = "")
        for (varname in names(varlist)) {
          df <- varlist[[varname]]
          cat(crayon::yellow$bold("    - ", varname, " :\n"), sep = "")
          domain_cols <- setdiff(names(df), metric_cols)
          if (!length(domain_cols)) {
            cat("      (no domain column found)\n")
            next
          }
          for (i in seq_len(nrow(df))) {
            dom_lbl <- paste(sapply(domain_cols, function(cc) {
              paste0(cc, "=", as.character(df[[cc]][i]))
            }), collapse = ", ")
            cat(sprintf("      Domain: %s\n", dom_lbl))
            cat(sprintf("        Mean: %s\n", fmt(df$mean[i])))
            cat(sprintf("        SE:   %s\n", fmt(df$se[i])))
            if ("rse" %in% names(df)) cat(sprintf("        RSE:  %s\n", fmt(df$rse[i])))
            cat(sprintf("        Bias: %s\n", fmt(df$bias[i])))
            cat(sprintf("        MSE:  %s\n", fmt(df$mse[i])))
            if ("p_bias" %in% names(df)) cat(sprintf("        p(Bias): %s\n", fmt(df$p_bias[i])))
          }
        }
      }
    } else {
      for (varname in names(by_obj)) {
        df <- by_obj[[varname]]
        cat(crayon::yellow$bold("  - ", varname, " :\n"), sep = "")
        domain_cols <- setdiff(names(df), metric_cols)
        if (!length(domain_cols)) {
          cat("    (no domain column found)\n")
          next
        }
        for (i in seq_len(nrow(df))) {
          dom_lbl <- paste(sapply(domain_cols, function(cc) {
            paste0(cc, "=", as.character(df[[cc]][i]))
          }), collapse = ", ")
          cat(sprintf("      Domain: %s\n", dom_lbl))
          cat(sprintf("        Mean: %s\n", fmt(df$mean[i])))
          cat(sprintf("        SE:   %s\n", fmt(df$se[i])))
          if ("rse" %in% names(df)) cat(sprintf("RSE:  %s\n", fmt(df$rse[i])))
          cat(sprintf("        Bias: %s\n", fmt(df$bias[i])))
          cat(sprintf("        MSE:  %s\n", fmt(df$mse[i])))
          if ("p_bias" %in% names(df)) cat(sprintf("p(Bias): %s\n", fmt(df$p_bias[i])))
        }
      }
    }
    cat("\n")
  }

  .print_totals(x$register_diagnostics$total, "Register Diagnostics (Total):")
  .print_by_domain(x$register_diagnostics$by_domain, "Register Diagnostics (By Domain):")

  .print_totals(x$survey_diagnostics$total, "Survey Diagnostics (Total):")
  .print_by_domain(x$survey_diagnostics$by_domain, "Survey Diagnostics (By Domain):")

  invisible(x)
}
