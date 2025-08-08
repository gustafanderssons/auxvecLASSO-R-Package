#' Fit LASSO model for a single binary outcome with cross-validation
#'
#' Fits a LASSO logistic regression model for a single binary outcome using cross-validation.
#' The function drops rows where the outcome is missing, and ensures that the predictors do not
#' have missing values. The model is fitted using the `glmnet` package, with the option to apply
#' cross-validation for selecting the optimal regularization parameter (`lambda`).
#'
#' @param yvar Character scalar. The name of the binary outcome variable in `df`.
#' @param df Data frame containing the outcome and predictors.
#'   The outcome variable (`yvar`) and predictor variables must be included in `df`.
#' @param X Model matrix (rows must align with `df`).
#'   The matrix must not contain missing values, and its rows must match those in `df`.
#' @param penalty_factors Named numeric vector of penalty factors for each predictor variable.
#'   The names should match the column names of `X`.
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' @param standardize Logical; should the predictors be standardized before fitting the model? Default is `TRUE`.
#' @param parallel Logical; should the cross-validation be performed in parallel? Default is `FALSE`.
#' @param return_models Logical; should the fitted `cv.glmnet` object be returned? Default is `FALSE`.
#' @param verbose Logical; if `TRUE`, prints progress messages. Default is `FALSE`.
#'
#' @return A list containing:
#' \describe{
#'   \item{selected}{A character vector of the names of the selected variables (non-zero coefficients).}
#'   \item{lambda_min}{The value of `lambda` that minimizes the cross-validation error.}
#'   \item{goodness}{A list containing performance metrics for the model:
#'     \itemize{
#'       \item \code{cross_validated}: A list with `cv_error` (cross-validation error) and `cv_error_sd` (standard deviation of CV error).
#'       \item \code{full_data}: A list with:
#'         \item `deviance_explained`: Proportion of deviance explained by the model.
#'         \item `auc`: Area under the ROC curve.
#'         \item `accuracy`: Classification accuracy.
#'         \item `brier_score`: Brier score (mean squared error for probabilities).
#'         \item `raw_coefs`: Raw coefficients from the LASSO model.
#'         \item `abs_coefs`: Absolute values of the coefficients.
#'     }
#'   }
#'   \item{model}{The fitted `cv.glmnet` model, if `return_models = TRUE`. Otherwise, `NULL`.}
#' }
#'
#' @details
#' - The outcome variable (`yvar`) must be binary (two levels) after dropping missing values.
#' - Rows with missing values for the outcome (`yvar`) or predictors (`X`) will be dropped.
#' - The function uses the `glmnet` package to fit the LASSO logistic regression model.
#' - Cross-validation is used to select the optimal regularization parameter (`lambda`).
#' - Model performance metrics, including AUC, accuracy, and Brier score, are computed on the non-missing rows.
#'
#' @importFrom glmnet cv.glmnet
#' @importFrom pROC auc
#' @export
fit_outcome <- function(
    yvar,
    df,
    X,
    penalty_factors,
    nfolds = 5,
    standardize = TRUE,
    parallel = FALSE,
    return_models = FALSE,
    verbose = FALSE) {

  # 0) Outcome vector + drop NAs
  y_full <- df[[yvar]]
  if (!is.factor(y_full)) y_full <- as.factor(y_full)

  keep <- !is.na(y_full)
  n_drop <- sum(!keep)
  if (n_drop > 0L) {
    message_verbose(verbose, sprintf("Outcome '%s': dropping %d row(s) with NA.", yvar, n_drop))
  }

  y <- droplevels(y_full[keep])

  # Must remain binary after dropping NAs
  if (nlevels(y) != 2) {
    stop("Outcome variable '", yvar, "' must be binary (2 levels) after removing NAs.")
  }

  # Subset X to the same rows
  if (nrow(X) != length(y_full)) {
    stop("Number of rows in X (", nrow(X), ") must match nrow(df) (", length(y_full), ").")
  }
  X_sub <- X[keep, , drop = FALSE]

  # 1) Predictors must have no missing values
  if (anyNA(X_sub)) {
    stop("Model matrix X contains missing values after outcome NA filtering. ",
         "Please ensure auxiliary variables have no missing values.")
  }

  # 2) glmnet needs at least 2 columns
  if (ncol(X_sub) < 2) {
    stop("Model matrix X must have at least 2 columns for glmnet (got ", ncol(X_sub), "). ",
         "Add more auxiliary variables or enable interactions.")
  }

  message_verbose(verbose, paste0("Fitting LASSO for binary outcome '", yvar, "' on ", nrow(X_sub), " rows."))

  # 3) Cross-validated LASSO logistic regression
  cv_fit <- tryCatch(
    glmnet::cv.glmnet(
      x = X_sub,
      y = y,
      family = "binomial",
      alpha = 1,
      nfolds = nfolds,
      penalty.factor = penalty_factors,
      standardize = standardize,
      parallel = parallel
    ),
    error = function(e) {
      warning("Skipping '", yvar, "' due to glmnet error: ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(cv_fit)) {
    return(list(
      selected = character(0),
      lambda_min = NA_real_,
      goodness = list(),
      model = NULL
    ))
  }

  # 4) Extracts
  lambda_min <- cv_fit$lambda.min
  lam_idx <- which.min(abs(cv_fit$lambda - lambda_min))

  coef_mat <- as.matrix(stats::coef(cv_fit, s = "lambda.min"))
  raw <- as.numeric(coef_mat)
  names(raw) <- rownames(coef_mat)
  raw <- raw[names(raw) != "(Intercept)"]
  selected <- names(raw)[raw != 0]

  # 5) Predictions & metrics computed on the same (non-missing) rows
  prob <- as.vector(stats::predict(cv_fit, newx = X_sub, s = lambda_min, type = "response"))
  pos <- levels(y)[2]
  y01 <- as.integer(y == pos)
  pred_class <- ifelse(prob > 0.5, pos, levels(y)[1])

  auc_val <- tryCatch(pROC::auc(y, prob), error = function(e) NA_real_)
  accuracy <- mean(pred_class == y)
  brier_score <- mean((prob - y01)^2)
  deviance_explained <- 1 - (cv_fit$cvm[lam_idx] / cv_fit$cvm[1])

  goodness <- list(
    cross_validated = list(
      cv_error = unname(cv_fit$cvm[lam_idx]),
      cv_error_sd = unname(cv_fit$cvsd[lam_idx])
    ),
    full_data = list(
      deviance_explained = unname(deviance_explained),
      auc = as.numeric(auc_val),
      accuracy = unname(accuracy),
      brier_score = unname(brier_score),
      raw_coefs = raw,
      abs_coefs = abs(raw)
    )
  )

  list(
    selected = selected,
    lambda_min = lambda_min,
    goodness = goodness,
    model = if (return_models) cv_fit else NULL
  )
}
