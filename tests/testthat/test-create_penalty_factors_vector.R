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

test_that("include_interactions = FALSE leaves interaction terms alone", {
  colnames_X <- c("age", "sex", "age:sex", "bmi")
  res <- create_penalty_factors(
    colnames_X,
    must_have_vars   = "sex",
    check_twoway_int = FALSE
  )

  # only 'sex' is forced (penalty 0); everything else stays 1, including the interaction
  expect_identical(res["sex"], setNames(0, "sex"))
  expect_identical(res[c("age:sex", "age", "bmi")], c("age:sex" = 1, age = 1, bmi = 1))

  # the function should not synthesize reverse-order names when interactions are disabled
  expect_false("sex:age" %in% names(res))
})

test_that("include_interactions = TRUE zeroes BOTH orders when a specific interaction is must-have", {
  # Put BOTH orderings in the design to make the expectation explicit
  colnames_X <- c("age", "sex", "age:sex", "sex:age", "bmi")
  res <- create_penalty_factors(
    colnames_X,
    must_have_vars   = "age:sex",
    check_twoway_int = TRUE
  )

  # both 'age:sex' and its reverse get the same penalty (0)
  expect_identical(res[c("age:sex", "sex:age")], c("age:sex" = 0, "sex:age" = 0))
  # unrelated terms untouched
  expect_identical(res[c("age", "sex", "bmi")], c(age = 1, sex = 1, bmi = 1))
})

test_that("include_interactions = TRUE does NOT zero an interaction just because one main effect is must-have", {
  colnames_X <- c("age", "sex", "age:sex", "bmi")
  res <- create_penalty_factors(
    colnames_X,
    must_have_vars   = "sex",
    check_twoway_int = TRUE
  )

  # 'sex' gets 0, but the interaction should remain penalized unless it was explicitly must-have
  expect_identical(res["sex"], setNames(0, "sex"))
  expect_identical(res["age:sex"], setNames(1, "age:sex"))
})

test_that("include_interactions = TRUE can also propagate from a *set* of must-have interactions", {
  colnames_X <- c("a", "b", "c", "a:b", "b:a", "b:c", "c:b")
  res <- create_penalty_factors(
    colnames_X,
    must_have_vars   = c("a:b", "b:c"),
    check_twoway_int = TRUE
  )

  # both directions for both interactions go to 0
  expect_identical(res[c("a:b", "b:a")], c("a:b" = 0, "b:a" = 0))
  expect_identical(res[c("b:c", "c:b")], c("b:c" = 0, "c:b" = 0))
  # mains remain 1 unless included explicitly
  expect_identical(res[c("a", "b", "c")], c(a = 1, b = 1, c = 1))
})

test_that("multiple must_have_vars work together (case-insensitive) with interactions", {
  colnames_X <- c("Age", "sex", "Age:sex", "bmi", "sex:Age")
  res <- create_penalty_factors(colnames_X, must_have_vars = c("AGE", "SeX"), check_twoway_int = TRUE)
  expect_identical(unname(res[c("Age", "sex")]), c(0, 0))
  expect_identical(unname(res[c("Age:sex", "sex:Age")]), c(0, 0))
  expect_identical(res["bmi"], setNames(1, "bmi"))
})

test_that("no matches: warns and returns all 1s", {
  colnames_X <- c("age", "sex", "bmi")
  expect_warning(
    res <- create_penalty_factors(colnames_X, must_have_vars = "unknown"),
    "No columns matched for must_have_vars"
  )
  expect_identical(unname(res), rep(1, length(colnames_X)))
})

test_that("names and length always match colnames_X", {
  colnames_X <- c("v1", "v2", "v3")
  res <- create_penalty_factors(colnames_X, must_have_vars = "v2")
  expect_named(res, colnames_X)
  expect_length(res, length(colnames_X))
})
