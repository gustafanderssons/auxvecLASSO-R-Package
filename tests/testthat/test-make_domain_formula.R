# tests/testthat/test-make_domain_formula.R

test_that(".make_domain_formula returns NULL for NULL or empty input", {
  expect_null(.make_domain_formula(NULL))
  expect_null(.make_domain_formula(character(0)))
})

test_that(".make_domain_formula builds correct formula for single variable", {
  f <- .make_domain_formula("region")
  expect_s3_class(f, "formula")
  expect_identical(as.character(f), c("~", "region"))
})

test_that(".make_domain_formula builds correct formula for multiple variables", {
  vars <- c("region", "gender", "age_group")
  f <- .make_domain_formula(vars)

  expect_s3_class(f, "formula")
  expect_identical(as.character(f), c("~", "region + gender + age_group"))
})

test_that(".make_domain_formula preserves order of variables", {
  vars <- c("varB", "varA")
  f <- .make_domain_formula(vars)
  expect_identical(as.character(f), c("~", "varB + varA"))
})
