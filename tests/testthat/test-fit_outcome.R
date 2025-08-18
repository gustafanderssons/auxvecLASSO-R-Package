test_that("errors when X rows don't match df rows", {
  df <- data.frame(y = 1:3, x1 = 1:3, x2 = 4:6)
  X <- model.matrix(~ x1 - 1, data = df) # 3 rows
  X <- X[-1, , drop = FALSE] # 2 rows -> mismatch

  expect_error(
    fit_outcome("y", df, X, penalty_factors = rep(1, ncol(X))),
    "Number of rows in X \\(2\\) must match nrow\\(df\\) \\(3\\)\\."
  )
})

test_that("errors when X has any NA after y filtering", {
  df <- data.frame(
    y = c(1, NA, 3),
    x1 = c(1, 2, 3),
    x2 = c(4, 5, 6)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)
  X[1, 1] <- NA # will remain after dropping row 2 due to NA y

  expect_error(
    fit_outcome("y", df, X, penalty_factors = rep(1, ncol(X))),
    "Model matrix X contains missing values"
  )
})

test_that("errors when X has < 2 columns", {
  df <- data.frame(y = 1:5, x1 = rnorm(5))
  X <- model.matrix(~ x1 - 1, data = df) # 1 column only

  expect_error(
    fit_outcome("y", df, X, penalty_factors = rep(1, ncol(X))),
    "Model matrix X must have at least 2 columns"
  )
})

test_that("factor outcome with >2 levels is rejected as non-numeric continuous", {
  set.seed(1)
  df <- data.frame(
    y  = factor(sample(letters[1:3], 20, TRUE)), # 3-level factor
    x1 = rnorm(20),
    x2 = rnorm(20)
  )
  X <- model.matrix(~ x1 + x2 - 1, data = df)

  expect_error(
    fit_outcome("y", df, X, penalty_factors = rep(1, ncol(X))),
    "For continuous outcomes, the outcome variable must be numeric"
  )
})
