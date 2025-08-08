test_that("fit_outcome returns expected structure for a binary outcome", {
  set.seed(123)
  n <- 120
  df <- data.frame(
    y  = factor(rbinom(n, 1, 0.5)),  # levels "0","1"
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)
  penalty_factors <- rep(1, ncol(X))

  res <- fit_outcome(
    yvar = "y",
    df = df,
    X = X,
    penalty_factors = penalty_factors,
    nfolds = 5,
    standardize = TRUE,
    parallel = FALSE,
    return_models = TRUE,
    verbose = FALSE
  )

  # top-level structure
  expect_type(res, "list")
  expect_true(all(c("selected", "lambda_min", "goodness", "model") %in% names(res)))
  expect_true(is.numeric(res$lambda_min))
  expect_false(is.na(res$lambda_min))
  expect_s3_class(res$model, "cv.glmnet")

  # goodness structure
  expect_true(all(c("cross_validated", "full_data") %in% names(res$goodness)))
  expect_true(all(c("cv_error", "cv_error_sd") %in% names(res$goodness$cross_validated)))
  expect_true(all(c("deviance_explained", "auc", "accuracy", "brier_score", "raw_coefs", "abs_coefs")
                  %in% names(res$goodness$full_data)))

  # metric ranges
  expect_gte(res$goodness$full_data$accuracy, 0)
  expect_lte(res$goodness$full_data$accuracy, 1)
  expect_gte(res$goodness$full_data$auc, 0)
  expect_lte(res$goodness$full_data$auc, 1)
  expect_gte(res$goodness$full_data$brier_score, 0)
  expect_lte(res$goodness$full_data$brier_score, 1)
})

test_that("fit_outcome excludes intercept and selects only columns from X", {
  set.seed(101)
  n <- 100
  df <- data.frame(
    y  = factor(rbinom(n, 1, 0.6)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)
  penalty_factors <- rep(1, ncol(X))

  res <- fit_outcome(
    yvar = "y",
    df = df,
    X = X,
    penalty_factors = penalty_factors,
    nfolds = 4,
    standardize = TRUE,
    parallel = FALSE,
    return_models = FALSE,
    verbose = FALSE
  )

  expect_false("(Intercept)" %in% res$selected)
  expect_true(all(res$selected %in% colnames(X)))

  # coef vectors: named, no intercept
  raw <- res$goodness$full_data$raw_coefs
  absraw <- res$goodness$full_data$abs_coefs
  expect_true(is.numeric(raw))
  expect_true(is.numeric(absraw))
  expect_false("(Intercept)" %in% names(raw))
  expect_false("(Intercept)" %in% names(absraw))
})

test_that("fit_outcome deviance_explained is numeric and within a sensible range", {
  set.seed(202)
  n <- 140
  df <- data.frame(
    y  = factor(rbinom(n, 1, 0.7)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)
  penalty_factors <- rep(1, ncol(X))

  res <- fit_outcome(
    yvar = "y",
    df = df,
    X = X,
    penalty_factors = penalty_factors,
    nfolds = 5,
    standardize = TRUE,
    parallel = FALSE,
    return_models = FALSE,
    verbose = FALSE
  )

  dev_exp <- res$goodness$full_data$deviance_explained
  expect_true(is.numeric(dev_exp))
  # could be negative depending on cv baseline; keep a wide but sensible range
  expect_gte(dev_exp, -1)
  expect_lte(dev_exp, 1)
})

test_that("fit_outcome returns NULL model when return_models = FALSE", {
  set.seed(303)
  n <- 90
  df <- data.frame(
    y  = factor(rbinom(n, 1, 0.5)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)
  penalty_factors <- rep(1, ncol(X))

  res <- fit_outcome(
    yvar = "y",
    df = df,
    X = X,
    penalty_factors = penalty_factors,
    nfolds = 3,
    standardize = TRUE,
    parallel = FALSE,
    return_models = FALSE,
    verbose = FALSE
  )

  expect_null(res$model)
})

test_that("fit_outcome errors on non-binary outcomes", {
  set.seed(404)
  n <- 60
  df <- data.frame(
    y  = factor(sample(letters[1:3], n, TRUE)),  # 3-level factor
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)
  penalty_factors <- rep(1, ncol(X))

  expect_error(
    fit_outcome(
      yvar = "y",
      df = df,
      X = X,
      penalty_factors = penalty_factors,
      nfolds = 3,
      standardize = TRUE,
      parallel = FALSE,
      return_models = FALSE,
      verbose = FALSE
    ),
    "must be binary",
    fixed = TRUE
  )
})
