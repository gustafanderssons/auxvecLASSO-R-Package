#' Print Summary of Auxiliary Vector Assessment
#'
#' S3 print method for objects of class \code{assess_aux_vector}.
#' Displays a formatted and colorized summary of diagnostics related to
#' auxiliary variable assessments including weight variation metrics,
#' register diagnostics (overall and by domain), and survey diagnostics
#' (overall and by domain).
#'
#' Requires the \pkg{crayon} package for colored output.
#'
#' @param x An object of class \code{assess_aux_vector} containing diagnostic results.
#'   Expected to have components:
#'   \describe{
#'     \item{weight_variation}{Named numeric vector or list of weight variation metrics.}
#'     \item{register_diagnostics}{List with \code{total} and \code{by_domain} components,
#'       each containing diagnostic summaries (mean, SE, bias, MSE) for variables.}
#'     \item{survey_diagnostics}{List with \code{total} and \code{by_domain} components,
#'       each containing diagnostic summaries (mean, SE, bias, MSE) for variables.}
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
#' Each diagnostic includes mean, standard error, bias, and mean squared error.
#' If the \pkg{crayon} package is not installed, the function will stop with an error.
#'
#' @examples
#' \dontrun{
#' # Assuming 'aux_vector_assessment' is an assess_aux_vector object:
#' print(aux_vector_assessment)
#' }
#'
#' @export
print.assess_aux_vector <- function(x, ...) {
  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop("Package 'crayon' must be installed for colored output")
  }

  cat(crayon::blue$bold("\n===================================\n"))
  cat(crayon::blue$bold("Auxiliary Vector Assessment Summary\n"))
  cat(crayon::blue$bold("===================================\n\n"))

  ## Weight Variation Metrics
  cat(crayon::green$bold("Weight Variation Metrics:\n"))
  for (metric in names(x$weight_variation)) {
    cat(sprintf("  %-25s %10.6f\n", metric, x$weight_variation[[metric]]))
  }
  cat("\n")

  ## Register Diagnostics (Total)
  cat(crayon::green$bold("Register Diagnostics (Total):\n"))
  for (varname in names(x$register_diagnostics$total)) {
    diag <- x$register_diagnostics$total[[varname]]
    cat(crayon::yellow$bold("  - ", varname, ":\n"), sep = "")
    cat(sprintf("      Mean:   %10.6f\n", diag$mean))
    cat(sprintf("      SE:     %10.6f\n", diag$se))
    cat(sprintf("      Bias:   %10.6f\n", diag$bias))
    cat(sprintf("      MSE:    %10.6f\n", diag$mse))
  }
  cat("\n")

  ## Register Diagnostics (By Domain)
  cat(crayon::green$bold("Register Diagnostics (By Domain):\n"))
  for (varname in names(x$register_diagnostics$by_domain)) {
    df <- x$register_diagnostics$by_domain[[varname]]
    cat(crayon::yellow$bold("  - ", varname, ":\n"), sep = "")
    for (i in seq_len(nrow(df))) {
      cat(sprintf("      Domain: %s\n", as.character(df$stype[i])))
      cat(sprintf("        Mean: %10.6f\n", df$mean[i]))
      cat(sprintf("        SE:   %10.6f\n", df$se[i]))
      cat(sprintf("        Bias: %10.6f\n", df$bias[i]))
      cat(sprintf("        MSE:  %10.6f\n", df$mse[i]))
    }
  }
  cat("\n")

  ## Survey Diagnostics (Total)
  cat(crayon::green$bold("Survey Diagnostics (Total):\n"))
  for (varname in names(x$survey_diagnostics$total)) {
    diag <- x$survey_diagnostics$total[[varname]]
    cat(crayon::yellow$bold("  - ", varname, ":\n"), sep = "")
    cat(sprintf("      Mean:   %10.6f\n", diag$mean))
    cat(sprintf("      SE:     %10.6f\n", diag$se))
    cat(sprintf("      Bias:   %10.6f\n", ifelse(is.na(diag$bias), NA, diag$bias)))
    cat(sprintf("      MSE:    %10.6f\n", ifelse(is.na(diag$mse), NA, diag$mse)))
  }
  cat("\n")

  ## Survey Diagnostics (By Domain)
  cat(crayon::green$bold("Survey Diagnostics (By Domain):\n"))
  for (varname in names(x$survey_diagnostics$by_domain)) {
    df <- x$survey_diagnostics$by_domain[[varname]]
    cat(crayon::yellow$bold("  - ", varname, ":\n"), sep = "")
    for (i in seq_len(nrow(df))) {
      domain <- as.character(df$stype[i])
      cat(sprintf("      Domain: %s\n", domain))
      cat(sprintf("        Mean: %10.6f\n", df$mean[i]))
      cat(sprintf("        SE:   %10.6f\n", df$se[i]))
      cat(sprintf("        Bias: %10.6f\n", ifelse(is.na(df$bias[i]), NA, df$bias[i])))
      cat(sprintf("        MSE:  %10.6f\n", ifelse(is.na(df$mse[i]), NA, df$mse[i])))
    }
  }

  cat("\n")
  invisible(x)
}
