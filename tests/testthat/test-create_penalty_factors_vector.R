test_that("returns 1s when must_have_vars is NULL or empty", {
  colnames_X <- c("age", "sex", "bmi")

  res1 <- create_penalty_factors(colnames_X, NULL)
  expect_type(res1, "double")
  expect_named(res1, colnames_X)
  expect_identical(unname(res1), rep(1, length(colnames_X)))

  res2 <- create_penalty_factors(colnames_X, character(0))
  expect_type(res2, "double")
  expect_named(res2, colnames_X)
  expect_identical(unname(res2), rep(1, length(colnames_X)))
})

test_that("exact matches are set to 0 (case-insensitive)", {
  colnames_X <- c("age", "sex", "bmi")

  res <- create_penalty_factors(colnames_X, must_have_vars = "SeX")
  expect_identical(res["sex"], setNames(0, "sex"))
  expect_identical(unname(res[c("age", "bmi")]), c(1, 1))
})

test_that("include_interactions=TRUE zeros interactions containing must-have vars", {
  colnames_X <- c("age", "sex", "age:sex", "sex:age", "bmi")

  res <- create_penalty_factors(colnames_X, must_have_vars = "sex", include_interactions = TRUE)
  expect_identical(res["sex"], setNames(0, "sex"))
  expect_identical(unname(res[c("age:sex", "sex:age")]), c(0, 0))
  expect_identical(unname(res[c("age", "bmi")]), c(1, 1))
})

test_that("include_interactions=FALSE does not zero interaction terms", {
  colnames_X <- c("age", "sex", "age:sex", "bmi")

  res <- create_penalty_factors(colnames_X, must_have_vars = "sex", include_interactions = FALSE)
  expect_identical(res["sex"], setNames(0, "sex"))
  expect_identical(unname(res[c("age:sex", "age", "bmi")]), c(1, 1, 1))
})

test_that("multiple must_have_vars work together (case-insensitive) with interactions", {
  colnames_X <- c("Age", "sex", "Age:sex", "bmi", "sex:Age")
  res <- create_penalty_factors(colnames_X, must_have_vars = c("AGE", "SeX"), include_interactions = TRUE)
  expect_identical(unname(res[c("Age", "sex")]), c(0, 0))
  expect_identical(unname(res[c("Age:sex", "sex:Age")]), c(0, 0))
  expect_identical(res["bmi"], setNames(1, "bmi"))
})

test_that("no matches: warns and returns all 1s", {
  colnames_X <- c("age", "sex", "bmi")
  expect_warning(
    res <- create_penalty_factors(colnames_X, must_have_vars = "unknown"),
    "No must-have variables found"
  )
  expect_identical(unname(res), rep(1, length(colnames_X)))
})

test_that("names and length always match colnames_X", {
  colnames_X <- c("v1", "v2", "v3")
  res <- create_penalty_factors(colnames_X, must_have_vars = "v2")
  expect_named(res, colnames_X)
  expect_length(res, length(colnames_X))
})
