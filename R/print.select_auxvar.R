#' Print Summary of LASSO Auxiliary Variable Selection Object
#'
#' S3 print method for objects of class \code{select_auxiliary_variables_lasso_cv}.
#' Displays a formatted and colorized summary of the selected auxiliary variables,
#' their grouping by outcome, selected penalty parameters (lambdas),
#' penalty factors, stored models, goodness-of-fit metrics, coefficient estimates,
#' and interaction metadata.
#'
#' Requires the \pkg{crayon} package for colored output.
#'
#' @param x An object of class \code{select_auxiliary_variables_lasso_cv} containing
#'   results from LASSO auxiliary variable selection with cross-validation.
#'   Expected to have components:
#'   \describe{
#'     \item{selected_variables}{Character vector of variables selected across outcomes.}
#'     \item{by_outcome}{Named list, with each element a character vector of selected variables for that outcome.}
#'     \item{selected_lambdas}{Named numeric vector or list of selected lambda values by outcome.}
#'     \item{penalty_factors}{Numeric vector of penalty factors (0 = must-keep, 1 = regular penalty).}
#'     \item{models}{List of fitted models stored for each outcome.}
#'     \item{goodness_of_fit}{Named list of goodness-of-fit results by outcome, each containing
#'     \code{cross_validated} (with \code{cv_error}, \code{cv_error_sd}) and \code{full_data}
#'     (with \code{deviance_explained}, \code{auc}, \code{accuracy}, \code{brier_score}, \code{raw_coefs}).}
#'     \item{interaction_metadata}{List with \code{interaction_terms}, \code{main_effects_in_interactions}, and \code{full_formula}.}
#'   }
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' The print method outputs information using colored text (via \pkg{crayon}), making
#' it easier to visually parse the summary. It organizes output into sections:
#' \itemize{
#'   \item Selected variables and their counts
#'   \item Variables selected by each outcome
#'   \item Selected lambda tuning parameters
#'   \item Summary of penalty factors
#'   \item Number of stored models
#'   \item Goodness-of-fit metrics for each outcome, including cross-validation
#'   error statistics and metrics on full data fit
#'   \item Coefficients at the lambda minimizing error, ordered by magnitude
#'   \item Interaction terms and main effects metadata
#' }
#' If the \pkg{crayon} package is not installed, the function will stop with an error.
#'
#' @examples
#' \dontrun{
#' # Assuming 'lasso_cv_obj' is a select_auxiliary_variables_lasso_cv object:
#' print(lasso_cv_obj)
#' }
#'
#' @export
print.select_auxiliary_variables_lasso_cv <- function(x, ...) {
  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop("Package 'crayon' must be installed for colored output")
  }

  cat(crayon::blue$bold("\n=========================\n"))
  cat(crayon::blue$bold("LASSO Auxiliary Variable Selection\n"))
  cat(crayon::blue$bold("=========================\n\n"))

  # Selected variables
  cat(crayon::green$bold("Selected Variables ("), length(x$selected_variables),
      crayon::green$bold("):\n"),
      sep = ""
  )
  if (length(x$selected_variables) == 0) {
    cat(crayon::red("  None selected\n\n"))
  } else {
    cat("  ", paste(x$selected_variables, collapse = ", "), "\n\n")
  }

  # By outcome
  cat(crayon::green$bold("By Outcome:\n"))
  for (outcome in names(x$by_outcome)) {
    vars <- x$by_outcome[[outcome]]
    cat(crayon::yellow$bold("  - ", outcome, ": "), length(vars),
        " variables\n",
        sep = ""
    )
    if (length(vars) > 0) {
      cat("    ", paste(vars, collapse = ", "), "\n", sep = "")
    }
  }
  cat("\n")

  # Selected lambdas
  cat(crayon::green$bold("Selected Lambdas:\n"))
  for (outcome in names(x$selected_lambdas)) {
    cat(crayon::yellow$bold("  - ", outcome, ": "),
        signif(x$selected_lambdas[[outcome]], 4), "\n",
        sep = ""
    )
  }
  cat("\n")

  # Penalty factors
  cat(crayon::green$bold("Penalty Factors:\n"))
  n_zero <- sum(x$penalty_factors == 0)
  n_one <- sum(x$penalty_factors == 1)
  cat("  Zero-penalty (must-keep): ", n_zero, "\n", sep = "")
  cat("  Regular-penalty: ", n_one, "\n\n", sep = "")

  # Models
  cat(crayon::green$bold("Stored Models: "), length(x$models), "\n\n", sep = "")

  # Goodness-of-fit
  cat(crayon::green$bold("Goodness-of-Fit:\n"))
  for (outcome in names(x$goodness_of_fit)) {
    gf <- x$goodness_of_fit[[outcome]]
    cat(crayon::yellow$bold("  Outcome: ", outcome, "\n"), sep = "")

    cat("    ", crayon::cyan("Cross-validated:"), "\n", sep = "")
    cat("      cv_error:    ", round(gf$cross_validated$cv_error, 4),
        "\n",
        sep = ""
    )
    cat("      cv_error_sd: ", round(gf$cross_validated$cv_error_sd, 4),
        "\n",
        sep = ""
    )

    cat("    ", crayon::cyan("Full data:"), "\n", sep = "")
    cat("      deviance_explained: ", round(gf$full_data$deviance_explained, 4),
        "\n",
        sep = ""
    )
    cat("      auc:                ", round(gf$full_data$auc, 4),
        "\n",
        sep = ""
    )
    cat("      accuracy:           ", round(gf$full_data$accuracy, 4),
        "\n",
        sep = ""
    )
    cat("      brier_score:        ", round(gf$full_data$brier_score, 4),
        "\n",
        sep = ""
    )

    # Coefficients
    raw_coefs <- gf$full_data$raw_coefs

    if (!is.null(raw_coefs) && length(raw_coefs) > 0) {
      coef_table <- data.frame(
        Variable = names(raw_coefs),
        `Coef` = round(raw_coefs, 4),
        stringsAsFactors = FALSE
      )

      coef_table <- coef_table[order(abs(raw_coefs), decreasing = TRUE), ]

      cat(crayon::blue$bold("    Coefficients at Lambda Min:\n"))
      for (i in seq_len(nrow(coef_table))) {
        cat(sprintf(
          "      %-25s %6.4f \n",
          coef_table$Variable[i],
          coef_table$`Coef`[i]
        ))
      }
      cat("\n")
    } else {
      cat(crayon::silver("    No matching coefficients found.\n\n"))
    }
  }

  # Interaction metadata
  cat(crayon::green$bold("Interaction Metadata:\n"))
  if (length(x$interaction_metadata$interaction_terms) > 0) {
    cat("  Interaction terms selected (",
        length(x$interaction_metadata$interaction_terms), "):\n",
        sep = ""
    )
    cat("    ",
        paste(x$interaction_metadata$interaction_terms, collapse = ", "),
        "\n",
        sep = ""
    )
  } else {
    cat(crayon::red("  No interaction terms\n"))
  }

  if (length(x$interaction_metadata$main_effects_in_interactions) > 0) {
    cat("  Main effects present in selected interactions: ",
        paste(x$interaction_metadata$main_effects_in_interactions,
              collapse = ", "
        ), "\n",
        sep = ""
    )
  }

  cat("  Full formula tested: ", x$interaction_metadata$full_formula, "\n\n", sep = "")

  invisible(x)
}
