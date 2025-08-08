# tests/testthat/test-prepare_calibration_inputs.R
skip_if_not_installed("survey")

test_that(".prepare_calibration_inputs aligns named vector to model.matrix columns", {
  data("api", package = "survey")  # loads apistrat into this environment

  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  cal_formula <- ~ ell + meals

  # Deliberately shuffled; must include (Intercept)
  pop_vec <- c(meals = 100, "(Intercept)" = 200, ell = 50)

  out <- .prepare_calibration_inputs(dsgn, cal_formula, pop_vec)

  expect_type(out, "list")
  expect_true("pop" %in% names(out))

  mf <- stats::model.frame(cal_formula, data = dsgn$variables)
  X  <- stats::model.matrix(cal_formula, data = mf)
  needed <- colnames(X)

  expect_identical(names(out$pop), needed)
  expect_identical(as.numeric(out$pop), as.numeric(pop_vec[needed]))
})

test_that(".prepare_calibration_inputs errors if named vector missing a required total", {
  data("api", package = "survey")

  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  cal_formula <- ~ ell + meals
  bad_vec <- c(ell = 50, meals = 100)  # missing (Intercept)

  expect_error(
    .prepare_calibration_inputs(dsgn, cal_formula, bad_vec),
    "Population totals names must include"
  )
})

test_that(".prepare_calibration_inputs aligns data.frame columns and drops extras", {
  data("api", package = "survey")
  dsgn <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                            data = apistrat, fpc = ~fpc)

  cal_formula <- ~ ell + meals

  pop_df <- data.frame(
    meals = 100,
    `(Intercept)` = 200,
    ell = 50,
    extra_col = 999,
    check.names = FALSE          # <-- keep "(Intercept)" literally
  )

  out <- .prepare_calibration_inputs(dsgn, cal_formula, pop_df)

  needed <- colnames(stats::model.matrix(cal_formula,
                                         stats::model.frame(cal_formula, dsgn$variables)))
  expect_identical(colnames(out$pop), needed)
  expect_identical(as.numeric(out$pop[1, ]), as.numeric(pop_df[1, needed]))
})



test_that(".prepare_calibration_inputs errors if data.frame missing a required column", {
  data("api", package = "survey")

  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  cal_formula <- ~ ell + meals
  bad_df <- data.frame(ell = 50, meals = 100)  # missing `(Intercept)`

  expect_error(
    .prepare_calibration_inputs(dsgn, cal_formula, bad_df),
    "Population totals data.frame columns must include"
  )
})

test_that(".prepare_calibration_inputs errors for unsupported pop totals type", {
  data("api", package = "survey")
  dsgn <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                            data = apistrat, fpc = ~fpc)

  cal_formula <- ~ ell + meals
  bad_obj <- list(ell = 50, meals = 100)  # list, not numeric vector or data.frame

  expect_error(
    .prepare_calibration_inputs(dsgn, cal_formula, bad_obj),
    "must be a named numeric vector or data.frame"
  )
})

test_that(".prepare_calibration_inputs accepts named numeric vector and reorders", {
  data("api", package = "survey")
  dsgn <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                            data = apistrat, fpc = ~fpc)
  cal_formula <- ~ ell + meals

  # Deliberately shuffled names
  pop_vec <- c(meals = 100, `(Intercept)` = 200, ell = 50)

  out <- .prepare_calibration_inputs(dsgn, cal_formula, pop_vec)

  needed <- colnames(stats::model.matrix(cal_formula, stats::model.frame(cal_formula, dsgn$variables)))
  expect_identical(names(out$pop), needed)
  expect_identical(as.numeric(out$pop), as.numeric(pop_vec[needed]))
})

