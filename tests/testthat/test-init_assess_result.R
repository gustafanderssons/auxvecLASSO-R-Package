# tests/testthat/test-init_assess_result.R
skip_if_not_installed("survey")

test_that(".init_assess_result returns the expected structure", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  res <- .init_assess_result(dsgn)

  # Top-level structure
  expect_type(res, "list")
  expect_true(all(c(
    "calibrated_weights",
    "weight_variation",
    "register_diagnostics",
    "survey_diagnostics"
  ) %in% names(res)))

  # calibrated_weights mirrors weights(design)
  expect_type(res$calibrated_weights, "double")
  expect_equal(length(res$calibrated_weights), nrow(dsgn$variables))
  expect_identical(res$calibrated_weights, stats::weights(dsgn))

  # empty placeholders for diagnostics with expected shapes
  expect_type(res$weight_variation, "list")
  expect_length(res$weight_variation, 0)

  expect_type(res$register_diagnostics, "list")
  expect_true(all(c("total","by_domain") %in% names(res$register_diagnostics)))
  expect_type(res$register_diagnostics$total, "list")
  expect_length(res$register_diagnostics$total, 0)
  expect_type(res$register_diagnostics$by_domain, "list")
  expect_length(res$register_diagnostics$by_domain, 0)

  expect_type(res$survey_diagnostics, "list")
  expect_true(all(c("total","by_domain") %in% names(res$survey_diagnostics)))
  expect_type(res$survey_diagnostics$total, "list")
  expect_length(res$survey_diagnostics$total, 0)
  expect_type(res$survey_diagnostics$by_domain, "list")
  expect_length(res$survey_diagnostics$by_domain, 0)
})

test_that(".init_assess_result reflects changes in design weights", {
  # Build a tiny synthetic design so we can control weights precisely
  df <- data.frame(x = 1:5, w = c(1, 2, 3, 4, 5))
  d1 <- survey::svydesign(id = ~1, weights = ~w, data = df)

  res1 <- .init_assess_result(d1)
  expect_identical(res1$calibrated_weights, stats::weights(d1))

  # Modify weights and check the result updates accordingly
  df$w <- df$w * 10
  d2 <- survey::svydesign(id = ~1, weights = ~w, data = df)

  res2 <- .init_assess_result(d2)
  expect_identical(res2$calibrated_weights, stats::weights(d2))
  expect_false(isTRUE(all.equal(res1$calibrated_weights, res2$calibrated_weights)))
})
