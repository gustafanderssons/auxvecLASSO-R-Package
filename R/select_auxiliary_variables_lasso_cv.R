#' Select Auxiliary Variables via LASSO with Cross-Validation (Binary and Continuous Outcomes)
#'
#' This function performs LASSO-penalized regression (logistic regression for binary outcomes or linear regression for continuous outcomes) with cross-validation to select
#' auxiliary variables for modeling one or more outcome variables. It allows for the
#' inclusion of all two-way interactions among the auxiliary variables and the option to
#' force certain variables to remain in the model through the use of zero penalty factors.
#'
#' The function supports both binary and continuous outcomes. For binary outcomes, logistic regression is used, and for continuous outcomes, linear regression is used.
#' The function outputs a list with the selected variables across outcomes, the associated
#' lambda values, the goodness-of-fit statistics, and optionally the fitted models and
#' interaction terms.
#'
#' @param df A data frame containing the data for modeling.
#' @param outcome_vars Character vector of outcome variable names to model. These can be either binary or continuous outcomes. Each
#'   must exist in `df` and have at least two unique values (after factor conversion for binary outcomes).
#' @param auxiliary_vars Character vector of auxiliary variable names to be used as predictors.
#' @param must_have_vars Optional character vector of variable names that must be included
#'   in the model (penalty factor 0). If interactions are included, any interaction containing
#'   a must-have variable is also assigned zero penalty. The variables in `must_have_vars` should refer to either individual variables or the main effect part of interaction terms.
#' @param check_twoway_int Logical; include all two-way interactions among auxiliary variables. Defaults to `TRUE`.
#' @param nfolds Number of folds for cross-validation. Defaults to 5.
#' @param verbose Logical; print progress messages. Defaults to `TRUE`.
#' @param standardize Logical; standardize predictors before fitting. Defaults to `TRUE`.
#' @param return_models Logical; return fitted `cv.glmnet` objects. Defaults to `FALSE`.
#' @param parallel Logical; run cross-validation in parallel (requires **doParallel**). Defaults to `FALSE`.
#'
#' @return An object of class \code{"select_auxiliary_variables_lasso_cv"} with the following components:
#' \describe{
#'   \item{selected_variables}{Character vector of variables selected across all outcome models. This includes the main effect variables and any interaction terms.}
#'   \item{by_outcome}{Named list of character vectors, each containing the selected variables for
#'         each outcome.}
#'   \item{selected_lambdas}{Named numeric vector of lambda values (specifically, lambda.min)
#'         for each outcome.}
#'   \item{penalty_factors}{Named numeric vector with penalty factors (0 for must-keep, 1 otherwise).}
#'   \item{models}{List of `cv.glmnet` objects per outcome if `return_models = TRUE`, otherwise an empty list.}
#'   \item{goodness_of_fit}{Named list per outcome with cross-validation metrics (cv_error, cv_error_sd)
#'         and full data metrics (deviance_explained for binary outcomes, auc, accuracy, brier_score, rss, mse, r_squared, raw_coefs).}
#'   \item{interaction_metadata}{List containing metadata on interaction terms, main effects in interactions,
#'         and the full formula used.}
#' }
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## Example 1: Binary + continuous outcomes, with interactions
#' ##             and must-have variables (factor expanded to dummies)
#' ## ------------------------------------------------------------
#' set.seed(123)
#' n <- 150
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' group <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
#'
#' ## Generate outcomes with some signal in x1, x2 and group, plus an interaction
#' eta_bin <- -0.5 + 1.2 * x2 - 0.8 * (group == "C") + 0.5 * x1 * x2
#' p <- 1 / (1 + exp(-eta_bin))
#' y_bin <- rbinom(n, 1, p)
#' y_cont <- 1.5 * x1 - 2 * (group == "B") + 0.7 * x1 * x2 + rnorm(n, sd = 0.7)
#'
#' df <- data.frame(y_bin = y_bin, y_cont = y_cont, x1 = x1, x2 = x2, group = group)
#'
#' res1 <- select_auxiliary_variables_lasso_cv(
#'   df = df,
#'   outcome_vars = c("y_bin", "y_cont"),
#'   auxiliary_vars = c("x1", "x2", "group"),
#'   must_have_vars = c("x1", "group"), # 'group' (factor) expands to its dummies
#'   check_twoway_int = TRUE,
#'   nfolds = 3,
#'   verbose = FALSE,
#'   standardize = TRUE,
#'   return_models = FALSE
#' )
#'
#' ## Inspect selections and metadata
#' res1$selected_variables
#' res1$by_outcome
#' res1$selected_lambdas
#' names(which(res1$penalty_factors == 0)) # must-keep terms (incl. factor dummies & interactions)
#' res1$interaction_metadata$full_formula
#'
#' ## ------------------------------------------------------------
#' ## Example 2: Single continuous outcome, main effects only
#' ## ------------------------------------------------------------
#' set.seed(456)
#' n2 <- 120
#' a <- rnorm(n2)
#' b <- rnorm(n2)
#' f <- factor(sample(c("a", "b"), n2, replace = TRUE))
#' y <- 2 * a - 1 * (f == "b") + rnorm(n2, sd = 1)
#'
#' toy <- data.frame(y = y, a = a, b = b, f = f)
#'
#' res2 <- select_auxiliary_variables_lasso_cv(
#'   df = toy,
#'   outcome_vars = "y",
#'   auxiliary_vars = c("a", "b", "f"),
#'   check_twoway_int = FALSE, # main effects only
#'   nfolds = 3,
#'   verbose = FALSE
#' )
#'
#' res2$selected_variables
#' res2$selected_lambdas
#' res2$goodness_of_fit$y
#'
#' @details
#' The function supports two types of outcome variables:
#' - **Binary outcomes**: LASSO logistic regression is used. The outcome variable must have exactly two levels after missing values are removed.
#' - **Continuous outcomes**: LASSO linear regression is used. The outcome variable should be numeric.
#'
#' For factor variables in `auxiliary_vars`, dummy variables are created to represent each level of the factor. If a factor variable is specified in `must_have_vars`,
#' its dummy variables will be included in the model, ensuring that any interactions containing those variables are also forced into the model.
#'
#' @importFrom stats as.formula model.matrix setNames
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

  # Ensure must_have_vars includes all dummy variables for factor variables
  if (!is.null(must_have_vars)) {
    must_have_vars_expanded <- must_have_vars

    # Expand must_have_vars to include all factor levels (dummies) if necessary
    for (var in must_have_vars) {
      if (var %in% auxiliary_vars && var %in% names(df) && is.factor(df[[var]])) {
        # Get the dummy variable names corresponding to the factor
        factor_cols <- grep(paste0("^", var), colnames_X)
        must_have_vars_expanded <- c(must_have_vars_expanded, colnames_X[factor_cols])
      }
    }

    must_have_vars <- must_have_vars_expanded
  }

  # Penalty factors (named), including interactions if requested
  penalty_factors <- create_penalty_factors(
    colnames_X = colnames_X,
    must_have_vars = must_have_vars,
    check_twoway_int = check_twoway_int
  )
  names(penalty_factors) <- colnames_X
  must_have_idx <- which(penalty_factors == 0)

  # Optional parallel backend
  cl <- setup_parallel(parallel, verbose)
  if (!is.null(cl)) on.exit(parallel::stopCluster(cl), add = TRUE)

  # Fit each outcome
  all_selected <- stats::setNames(vector("list", length(valid_outcomes)), valid_outcomes)
  selected_lambdas <- stats::setNames(rep(NA_real_, length(valid_outcomes)), valid_outcomes)
  goodness_of_fit <- stats::setNames(vector("list", length(valid_outcomes)), valid_outcomes)
  cv_models <- stats::setNames(vector("list", length(valid_outcomes)), valid_outcomes)

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

    # Only add main effects if they are not already represented by dummies
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
