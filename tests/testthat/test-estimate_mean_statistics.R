skip_if_not_installed("survey")

test_that("errors if design not survey-like or vars missing", {
  fake <- list(variables = data.frame(a = 1:3))
  expect_error(estimate_mean_stats(fake, vars = "b"), "Variables not found")

  # Proper design but variable missing
  data(api, package = "survey")
  d <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                         data = apistrat, fpc = ~fpc)
  expect_error(estimate_mean_stats(d, vars = "definitely_not_here"),
               "Variables not found")
})

test_that("non-finite weights are dropped once and estimation proceeds", {
  data(api, package = "survey")
  d <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                         data = apistrat, fpc = ~fpc)

  # Make one weight infinite by forcing prob = 0 (w = 1/prob)
  d$prob[1] <- 0
  expect_true(is.infinite(stats::weights(d)[1]))

  res <- estimate_mean_stats(d, vars = c("api00"))
  expect_type(res, "list")
  expect_true("api00" %in% names(res))
  expect_true(all(c("variable","mean","se","bias","mse") %in% names(res$api00)))
})

test_that("by accepts one-sided formula or character vector; rejects others", {
  data(api, package = "survey")
  d <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                         data = apistrat, fpc = ~fpc)

  # character vector
  res_chr <- estimate_mean_stats(d, vars = "api00", by = "stype")
  expect_true(is.data.frame(res_chr$api00))
  expect_true("stype" %in% names(res_chr$api00))

  # one-sided formula
  res_form <- estimate_mean_stats(d, vars = "api00", by = ~stype)
  expect_true(is.data.frame(res_form$api00))
  expect_true("stype" %in% names(res_form$api00))

  # two-sided formula should error
  expect_error(estimate_mean_stats(d, vars = "api00", by = api00 ~ stype),
               "one-sided formula")
})

test_that("overall: bias & mse computed from named population vector if provided", {
  # Tiny known example: weights = 1, mean known
  df <- data.frame(x = c(1, 2, 3, 4))
  d  <- survey::svydesign(id = ~1, weights = ~1, data = df)
  res <- estimate_mean_stats(d, vars = "x", population_means = c(x = 2.5))

  out <- res$x
  expect_equal(out$variable, "x")
  expect_equal(out$mean, 2.5, tolerance = 1e-8)
  expect_equal(out$bias, 0, tolerance = 1e-8)
  expect_equal(out$mse, out$se^2, tolerance = 1e-8)  # bias^2 + var
})

test_that("overall: missing name in population vector -> NA bias/mse", {
  df <- data.frame(x = c(1,2), y = c(3,4))
  d  <- survey::svydesign(id = ~1, weights = ~1, data = df)
  res <- estimate_mean_stats(d, vars = "x", population_means = c(y = 10))
  expect_true(is.na(res$x$bias))
  expect_true(is.na(res$x$mse))
})

test_that("by-domain: requires population_means data.frame (if provided) with domain cols", {
  data("api", package = "survey")
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  # Missing domain column in population_means -> error
  bad_pm <- data.frame(not_stype = c("E","H","M"), api00 = c(1,2,3))
  expect_error(
    estimate_mean_stats(d, vars = "api00", by = ~stype, population_means = bad_pm),
    regexp = "`population_means` must include all domain variables used in `by`\\."
  )

  # OK structure but missing var column -> bias/mse NA (no error)
  ok_pm <- data.frame(stype = c("E","H","M"), api99 = c(1,2,3))
  res <- estimate_mean_stats(d, vars = "api00", by = ~stype, population_means = ok_pm)
  expect_true(all(is.na(res$api00$bias)))
  expect_true(all(is.na(res$api00$mse)))
})

test_that("estimate_mean_stats errors cleanly when variable is missing from design", {
  data("api", package = "survey")
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  expect_error(
    estimate_mean_stats(d, vars = "nonexistent_var"),
    regexp = "Variables not found in design: nonexistent_var"
  )
})

test_that("estimate_mean_stats accepts one-sided by=~stype and returns domain column", {
  data("api", package = "survey")
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  out <- estimate_mean_stats(d, vars = "api00", by = ~stype)
  expect_s3_class(out$api00, "data.frame")
  expect_true("stype" %in% names(out$api00))
  expect_true(all(c("variable","mean","se","bias","mse") %in% names(out$api00)))
})

test_that("per-variable finite filtering: errors if no finite data remain", {
  # Build design with finite weights but all-NA var
  df <- data.frame(w = c(1,1,1), x = c(NA, NA, NA))
  d  <- survey::svydesign(id = ~1, weights = ~w, data = df)
  expect_error(estimate_mean_stats(d, vars = "x"),
               "no finite observations")
})
