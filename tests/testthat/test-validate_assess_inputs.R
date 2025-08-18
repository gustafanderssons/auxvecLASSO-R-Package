test_that(".validate_assess_inputs errors if design is not a survey.design", {
  fake <- list(variables = data.frame(a = 1:3))
  expect_error(
    .validate_assess_inputs(
      design = fake,
      df = data.frame(),
      calibration_formula = NULL,
      calibration_pop_totals = NULL,
      register_vars = NULL,
      survey_vars = NULL,
      domain_vars = NULL,
      diagnostics = c("weight_variation"),
      already_calibrated = TRUE
    ),
    "design must be a survey.design object"
  )
})

test_that(".validate_assess_inputs errors for unknown diagnostics", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )
  expect_error(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = NULL,
      calibration_pop_totals = NULL,
      register_vars = NULL,
      survey_vars = NULL,
      domain_vars = NULL,
      diagnostics = c("weight_variation", "not_a_thing"),
      already_calibrated = TRUE
    ),
    "Unknown diagnostics"
  )
})

test_that(".validate_assess_inputs requires calibration_pop_totals when calibration_formula is provided and not already calibrated", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  # Missing pop totals -> error
  expect_error(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = ~ ell + meals,
      calibration_pop_totals = NULL,
      register_vars = NULL,
      survey_vars = NULL,
      domain_vars = NULL,
      diagnostics = c("weight_variation"),
      already_calibrated = FALSE
    ),
    "Provide calibration_pop_totals"
  )

  # If already calibrated, no error
  expect_silent(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = ~ ell + meals,
      calibration_pop_totals = NULL,
      register_vars = NULL,
      survey_vars = NULL,
      domain_vars = NULL,
      diagnostics = c("weight_variation"),
      already_calibrated = TRUE
    )
  )
})

test_that(".validate_assess_inputs errors when domain_vars not present in design data", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  expect_error(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = NULL,
      calibration_pop_totals = NULL,
      register_vars = NULL,
      survey_vars = NULL,
      domain_vars = c("missing_domain"),
      diagnostics = c("weight_variation"),
      already_calibrated = TRUE
    ),
    "must exist in the survey design data"
  )
})

test_that(".validate_assess_inputs warns for register_vars or survey_vars not in design", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  # Capture warnings for both register_vars and survey_vars paths
  expect_warning(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = NULL,
      calibration_pop_totals = NULL,
      register_vars = c("def_not_here"),
      survey_vars = NULL,
      domain_vars = NULL,
      diagnostics = c("weight_variation"),
      already_calibrated = TRUE
    ),
    "register_vars not found"
  )

  expect_warning(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = NULL,
      calibration_pop_totals = NULL,
      register_vars = NULL,
      survey_vars = c("def_not_here_either"),
      domain_vars = NULL,
      diagnostics = c("weight_variation"),
      already_calibrated = TRUE
    ),
    "survey_vars not found"
  )
})

test_that(".validate_assess_inputs is silent on valid inputs", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  expect_silent(
    .validate_assess_inputs(
      design = dsgn,
      df = survey::apistrat,
      calibration_formula = NULL,
      calibration_pop_totals = NULL,
      register_vars = c("ell", "meals"), # exist
      survey_vars = c("api00"), # exists
      domain_vars = c("stype"), # exists
      diagnostics = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
      already_calibrated = TRUE
    )
  )
})
