#' Fit LASSO model for a single outcome with cross-validation
#'
#' Fits a LASSO regression model (logistic regression for binary outcomes or linear regression for continuous outcomes) for a single outcome variable using cross-validation.
#' The function drops rows where the outcome is missing, and ensures that the predictors do not
#' have missing values. The model is fitted using the `glmnet` package, with the option to apply
#' cross-validation for selecting the optimal regularization parameter (`lambda`).
#'
#' @param yvar Character scalar. The name of the outcome variable in `df`. This can be either a binary or continuous outcome variable.
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
#'         \item `deviance_explained`: Proportion of deviance explained by the model (only for binary outcomes).
#'         \item `auc`: Area under the ROC curve (only for binary outcomes).
#'         \item `accuracy`: Classification accuracy (only for binary outcomes).
#'         \item `brier_score`: Brier score (mean squared error for probabilities) (only for binary outcomes).
#'         \item `rss`: Residual sum of squares (only for continuous outcomes).
#'         \item `mse`: Mean squared error (only for continuous outcomes).
#'         \item `rmse`: Root mean squared error (only for continuous outcomes).
#'         \item `mae`: Mean absolute error (only for continuous outcomes).
#'         \item `r_squared`: R-squared (only for continuous outcomes).
#'         \item `raw_coefs`: Raw coefficients from the LASSO model.
#'         \item `abs_coefs`: Absolute values of the coefficients.
#'     }
#'   }
#'   \item{model}{The fitted `cv.glmnet` model, if `return_models = TRUE`. Otherwise, `NULL`.}
#' }
#'
#' @details
#' - The outcome variable (`yvar`) can be either **binary** or **continuous**:
#'   - **Binary outcomes**: LASSO logistic regression is used. The outcome variable must have exactly two levels after missing values are removed.
#'   - **Continuous outcomes**: LASSO linear regression is used. The outcome variable should be numeric.
#' - Rows with missing values for the outcome (`yvar`) or predictors (`X`) will be dropped.
#' - The function uses the `glmnet` package to fit the LASSO model.
#' - Cross-validation is used to select the optimal regularization parameter (`lambda`).
#' - Model performance metrics, including **AUC**, **accuracy**, **Brier score** (for binary outcomes) or **RSS**, **MSE**, **RMSE**, **MAE**, **R-squared** (for continuous outcomes), are computed on the non-missing rows.
#'
#' @examples
#' ## ============================================================
#' ## Example 1: Binary outcome (binomial LASSO with CV)
#' ## ============================================================
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'   requireNamespace("pROC", quietly = TRUE)) {
#'   set.seed(101)
#'
#'   n <- 180
#'   x1 <- rnorm(n)
#'   x2 <- rnorm(n)
#'   f <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
#'
#'   ## Construct a binary outcome with signal in x2 and x1:x2
#'   lin <- -0.3 + 1.0 * x2 + 0.6 * (x1 * x2) - 0.7 * (f == "C")
#'   p <- 1 / (1 + exp(-lin))
#'   yfac <- factor(rbinom(n, 1, p), labels = c("No", "Yes")) # 2-level factor
#'
#'   df <- data.frame(y = yfac, x1 = x1, x2 = x2, f = f)
#'
#'   ## Model matrix with main effects + one interaction, no intercept
#'   X <- model.matrix(~ x1 + x2 + f + x1:x2 - 1, data = df)
#'
#'   ## Penalty factors must match X's columns (names + length).
#'   penalty_factors <- rep(1, ncol(X))
#'   names(penalty_factors) <- colnames(X)
#'   ## (Optional) keep x1 unpenalized:
#'   if ("x1" %in% names(penalty_factors)) penalty_factors["x1"] <- 0
#'
#'   fit_bin <- fit_outcome(
#'     yvar            = "y",
#'     df              = df,
#'     X               = X,
#'     penalty_factors = penalty_factors,
#'     nfolds          = 3,
#'     standardize     = TRUE,
#'     parallel        = FALSE,
#'     return_models   = FALSE,
#'     verbose         = FALSE
#'   )
#'
#'   ## Peek at the results
#'   fit_bin$selected
#'   fit_bin$lambda_min
#'   fit_bin$goodness$full_data$auc
#'   fit_bin$goodness$full_data$accuracy
#' }
#'
#' ## ============================================================
#' ## Example 2: Continuous outcome (gaussian LASSO with CV)
#' ## ============================================================
#' if (requireNamespace("glmnet", quietly = TRUE)) {
#'   set.seed(202)
#'
#'   n <- 160
#'   x1 <- rnorm(n)
#'   x2 <- rnorm(n)
#'   f <- factor(sample(c("L", "H"), n, replace = TRUE))
#'
#'   y <- 1.5 * x1 + 0.8 * x2 - 1.0 * (f == "H") + 0.6 * (x1 * x2) + rnorm(n, sd = 0.7)
#'   df <- data.frame(y = y, x1 = x1, x2 = x2, f = f)
#'
#'   ## Main effects only, no intercept
#'   X <- model.matrix(~ x1 + x2 + f - 1, data = df)
#'
#'   penalty_factors <- rep(1, ncol(X))
#'   names(penalty_factors) <- colnames(X)
#'
#'   fit_cont <- fit_outcome(
#'     yvar            = "y",
#'     df              = df,
#'     X               = X,
#'     penalty_factors = penalty_factors,
#'     nfolds          = 3,
#'     standardize     = TRUE,
#'     parallel        = FALSE,
#'     return_models   = FALSE,
#'     verbose         = FALSE
#'   )
#'
#'   ## Key metrics
#'   fit_cont$selected
#'   fit_cont$lambda_min
#'   fit_cont$goodness$full_data$mse
#'   fit_cont$goodness$full_data$r_squared
#' }
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

  # Check if y is a factor or numeric
  if (is.factor(y_full)) {
    y_full <- droplevels(y_full) # Remove any unused factor levels
  }

  keep <- !is.na(y_full)
  n_drop <- sum(!keep)
  if (n_drop > 0L) {
    message_verbose(verbose, sprintf("Outcome '%s': dropping %d row(s) with NA.", yvar, n_drop))
  }

  y <- y_full[keep]

  # Determine outcome type: binary or continuous
  is_binary <- is.factor(y) && nlevels(y) == 2

  # If it's continuous, ensure it's numeric
  if (!is_binary && !is.numeric(y)) {
    stop("For continuous outcomes, the outcome variable must be numeric.")
  }

  # Subset X to the same rows as y
  if (nrow(X) != length(y_full)) {
    stop("Number of rows in X (", nrow(X), ") must match nrow(df) (", length(y_full), ").")
  }
  X_sub <- X[keep, , drop = FALSE]

  # 1) Predictors must have no missing values
  if (anyNA(X_sub)) {
    stop(
      "Model matrix X contains missing values after outcome NA filtering. ",
      "Please ensure auxiliary variables have no missing values."
    )
  }

  # 2) glmnet needs at least 2 columns
  if (ncol(X_sub) < 2) {
    stop(
      "Model matrix X must have at least 2 columns for glmnet (got ", ncol(X_sub), "). ",
      "Add more auxiliary variables or enable interactions."
    )
  }

  # Select model type based on outcome
  if (is_binary) {
    # For binary outcomes, use logistic regression (binomial family)
    family_type <- "binomial"
  } else {
    # For continuous outcomes, use linear regression (gaussian family)
    family_type <- "gaussian"
  }

  message_verbose(verbose, paste0("Fitting LASSO for ", ifelse(is_binary, "binary", "continuous"), " outcome '", yvar, "' on ", nrow(X_sub), " rows."))

  # 3) Cross-validated LASSO (logistic or linear regression)
  cv_fit <- tryCatch(
    glmnet::cv.glmnet(
      x = X_sub,
      y = y,
      family = family_type,
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

  # Ensure main effects for interactions are included
  interaction_terms <- grep(":", selected, value = TRUE)
  if (length(interaction_terms) > 0) {
    for (interaction in interaction_terms) {
      # Split interaction into main effects
      main_effects <- strsplit(interaction, ":", fixed = TRUE)[[1]]

      # Add main effects to selected variables if not already included
      for (main_effect in main_effects) {
        if (!(main_effect %in% selected)) {
          selected <- c(selected, main_effect)
        }
      }
    }
  }

  # 5) Predictions & metrics computed on the same (non-missing) rows
  prob <- as.vector(stats::predict(cv_fit, newx = X_sub, s = lambda_min, type = "response"))

  if (is_binary) {
    # For binary outcomes, use classification metrics
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
  } else {
    # For continuous outcomes, use regression metrics
    residuals <- y - prob
    rss <- sum(residuals^2)
    mse <- mean(residuals^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(residuals))
    r_squared <- 1 - rss / sum((y - mean(y))^2)

    goodness <- list(
      cross_validated = list(
        cv_error = unname(cv_fit$cvm[lam_idx]),
        cv_error_sd = unname(cv_fit$cvsd[lam_idx])
      ),
      full_data = list(
        rss = unname(rss),
        mse = unname(mse),
        rmse = unname(rmse),
        mae = unname(mae),
        r_squared = unname(r_squared),
        raw_coefs = raw,
        abs_coefs = abs(raw)
      )
    )
  }

  list(
    selected = selected,
    lambda_min = lambda_min,
    goodness = goodness,
    model = if (return_models) cv_fit else NULL
  )
}
