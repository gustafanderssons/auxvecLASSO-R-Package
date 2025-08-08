test_that("returns empty and messages when df has zero rows", {
  df <- data.frame(x = numeric(0))
  outcome_vars <- c("y1", "y2")
  aux <- c("x")

  expect_message(
    res <- validate_and_filter_outcomes(df, outcome_vars, aux, verbose = TRUE),
    "zero rows"
  )
  expect_identical(res, character(0))

  # Silent when verbose = FALSE
  expect_silent(
    res2 <- validate_and_filter_outcomes(df, outcome_vars, aux, verbose = FALSE)
  )
  expect_identical(res2, character(0))
})

test_that("returns empty and messages when no auxiliary variables provided", {
  df <- data.frame(y = sample(0:1, 10, TRUE), x = rnorm(10))
  outcome_vars <- c("y")
  aux <- character(0)

  expect_message(
    res <- validate_and_filter_outcomes(df, outcome_vars, aux, verbose = TRUE),
    "No auxiliary variables"
  )
  expect_identical(res, character(0))
})

test_that("filters outcomes by presence in df and >= 2 unique values", {
  set.seed(1)
  df <- data.frame(
    y1 = sample(0:1, 20, TRUE),     # 2 unique values
    y2 = rep(1, 20),                # 1 unique value
    x1 = rnorm(20),
    x2 = rnorm(20)
  )
  outcome_vars <- c("y1", "y2", "y3")  # y3 not present
  aux <- c("x1", "x2")

  res <- validate_and_filter_outcomes(df, outcome_vars, aux, verbose = FALSE)
  expect_identical(res, "y1")
})

test_that("returns empty and messages when no valid outcomes remain", {
  df <- data.frame(
    y_const = rep(0, 15),
    x = rnorm(15)
  )
  outcome_vars <- c("y_missing", "y_const")
  aux <- "x"

  expect_message(
    res <- validate_and_filter_outcomes(df, outcome_vars, aux, verbose = TRUE),
    "No valid outcome variables"
  )
  expect_identical(res, character(0))
})

test_that("handles factor and numeric outcomes and preserves order", {
  set.seed(2)
  df <- data.frame(
    a = factor(sample(c("A", "B"), 30, TRUE)),  # 2 levels
    b = sample(0:1, 30, TRUE),                  # 2 unique numeric
    c = rep(5, 30),                             # 1 unique -> invalid
    x = rnorm(30)
  )
  # Supply in specific order; expect same order back
  outcome_vars <- c("b", "a", "c")
  aux <- "x"

  res <- validate_and_filter_outcomes(df, outcome_vars, aux, verbose = FALSE)
  expect_identical(res, c("b", "a"))
})
