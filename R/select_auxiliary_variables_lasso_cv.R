#' Select Auxiliary Variables via LASSO with Cross-Validation (Binary Outcomes)
#'
#' This function performs LASSO-penalized logistic regression with cross-validation to select
#' auxiliary variables for modeling one or more binary outcome variables. It allows for the
#' inclusion of all two-way interactions among the auxiliary variables and the option to
#' force certain variables to remain in the model through the use of zero penalty factors.
#'
#' The function outputs a list with the selected variables across outcomes, the associated
#' lambda values, the goodness-of-fit statistics, and optionally the fitted models and
#' interaction terms.
#'
#' @param df A data frame containing the data for modeling.
#' @param outcome_vars Character vector of binary outcome variable names to model. Each
#'   must exist in `df` and have at least two unique values (after factor conversion).
#' @param auxiliary_vars Character vector of auxiliary variable names to be used as predictors.
#' @param must_have_vars Optional character vector of variable names that must be included
#'   in the model (penalty factor 0). If interactions are included, any interaction containing
#'   a must-have variable is also assigned zero penalty.
#' @param check_twoway_int Logical; include all two-way interactions among auxiliary variables.
#'   Defaults to `TRUE`.
#' @param nfolds Number of folds for cross-validation. Defaults to 5.
#' @param verbose Logical; print progress messages. Defaults to `TRUE`.
#' @param standardize Logical; standardize predictors before fitting. Defaults to `TRUE`.
#' @param return_models Logical; return fitted `cv.glmnet` objects. Defaults to `FALSE`.
#' @param parallel Logical; run cross-validation in parallel (requires **doParallel**). Defaults to `FALSE`.
#'
#' @return An object of class \code{"select_auxiliary_variables_lasso_cv"} with the following components:
#' \describe{
#'   \item{selected_variables}{Character vector of variables selected across all outcome models.}
#'   \item{by_outcome}{Named list of character vectors, each containing the selected variables for
#'         each outcome.}
#'   \item{selected_lambdas}{Named numeric vector of lambda values (specifically, lambda.min)
#'         for each outcome.}
#'   \item{penalty_factors}{Named numeric vector with penalty factors (0 for must-keep, 1 otherwise).}
#'   \item{models}{List of `cv.glmnet` objects per outcome if `return_models = TRUE`, otherwise an empty list.}
#'   \item{goodness_of_fit}{Named list per outcome with cross-validation metrics (cv_error, cv_error_sd)
#'         and full data metrics (deviance_explained, auc, accuracy, brier_score, raw_coefs).}
#'   \item{interaction_metadata}{List containing metadata on interaction terms, main effects in interactions,
#'         and the full formula used.}
#' }
#'
#' @examples
#' # Example 1: Select auxiliary variables using LASSO for binary outcomes
#' result <- select_auxiliary_variables_lasso_cv(
#'   df = survey_data,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   auxiliary_vars = c("age", "gender", "income"),
#'   must_have_vars = c("age"),
#'   check_twoway_int = TRUE,
#'   nfolds = 5,
#'   verbose = TRUE,
#'   standardize = TRUE,
#'   return_models = FALSE,
#'   parallel = FALSE
#' )
#'
#' # Example 2: Run with parallel cross-validation and return models
#' result_parallel <- select_auxiliary_variables_lasso_cv(
#'   df = survey_data,
#'   outcome_vars = c("outcome1"),
#'   auxiliary_vars = c("age", "gender", "income"),
#'   must_have_vars = NULL,
#'   check_twoway_int = TRUE,
#'   nfolds = 10,
#'   verbose = TRUE,
#'   standardize = TRUE,
#'   return_models = TRUE,
#'   parallel = TRUE
#' )
#'
#' @importFrom stats as.formula model.matrix
#' @importFrom glmnet cv.glmnet
#' @import doParallel
#' @export
select_auxiliary_variables_lasso_cv <- function(df,
                                                outcome_vars,
                                                auxiliary_vars,
                                                must_have_vars = NULL,
                                                check_twoway_int = TRUE,
                                                nfolds = 5,
                                                verbose = TRUE,
                                                standardize = TRUE,
                                                return_models = FALSE,
                                                parallel = FALSE) {
  # Validate & filter outcomes
  valid_outcomes <- validate_and_filter_outcomes(df, outcome_vars, auxiliary_vars, verbose)
  if (length(valid_outcomes) == 0L) {
    return(empty_result(outcome_vars))
  }

  # Build model matrix
  mm <- build_model_matrix(df, auxiliary_vars, check_twoway_int)
  X <- mm$X
  formula_str <- mm$formula_str
  colnames_X <- colnames(X)

  # Penalty factors (named), including interactions if requested
  penalty_factors <- create_penalty_factors(
    colnames_X = colnames_X,
    must_have_vars = must_have_vars,
    include_interactions = check_twoway_int
  )
  names(penalty_factors) <- colnames_X
  must_have_idx <- which(penalty_factors == 0)

  # Optional parallel backend
  cl <- setup_parallel(parallel, verbose)
  if (!is.null(cl)) on.exit(parallel::stopCluster(cl), add = TRUE)

  # Fit each outcome
  all_selected <- setNames(vector("list", length(valid_outcomes)), valid_outcomes)
  selected_lambdas <- setNames(rep(NA_real_, length(valid_outcomes)), valid_outcomes)
  goodness_of_fit <- setNames(vector("list", length(valid_outcomes)), valid_outcomes)
  cv_models <- setNames(vector("list", length(valid_outcomes)), valid_outcomes)

  for (yvar in valid_outcomes) {
    res <- fit_outcome(
      yvar = yvar,
      df = df,
      X = X,
      penalty_factors = penalty_factors,
      nfolds = nfolds,
      standardize = standardize,
      parallel = parallel,
      return_models = return_models,
      verbose = verbose
    )
    all_selected[[yvar]] <- res$selected
    selected_lambdas[[yvar]] <- res$lambda_min
    goodness_of_fit[[yvar]] <- res$goodness
    cv_models[[yvar]] <- res$model
  }

  # Union of selected variables; if interactions present, add their main effects
  combined <- unique(unlist(all_selected, use.names = FALSE))
  if (check_twoway_int) {
    interaction_terms <- grep(":", combined, value = TRUE)
    main_effects <- unique(unlist(strsplit(interaction_terms, ":"), use.names = FALSE))
    combined <- unique(c(combined, main_effects))
  }
  # Always include must-have names
  must_have_names <- colnames_X[must_have_idx]
  combined <- unique(c(combined, must_have_names))

  # Return object matching S3 print method’s expectations
  out <- list(
    selected_variables = combined,
    by_outcome = all_selected,
    selected_lambdas = selected_lambdas,
    penalty_factors = penalty_factors,
    models = if (isTRUE(return_models)) cv_models else list(),
    goodness_of_fit = goodness_of_fit,
    interaction_metadata = list(
      interaction_terms = grep(":", combined, value = TRUE),
      main_effects_in_interactions = unique(unlist(strsplit(grep(":", combined, value = TRUE), ":"), use.names = FALSE)),
      full_formula = formula_str
    )
  )
  class(out) <- "select_auxiliary_variables_lasso_cv"
  out
}
