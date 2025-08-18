test_that("errors if design not survey-like or vars missing", {
  fake <- list(variables = data.frame(a = 1:3))
  expect_error(estimate_mean_stats(fake, vars = "b"), "Variables not found in design: b")

  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )
  expect_error(
    estimate_mean_stats(d, vars = "definitely_not_here"),
    "Variables not found in design: definitely_not_here"
  )
})

test_that("non-finite weights are dropped once and estimation proceeds", {
  dat <- apistrat
  dat$pw[1] <- Inf # ensure a truly non-finite weight in the design
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = dat, fpc = ~fpc
  )
  expect_true(is.infinite(stats::weights(d)[1]))

  res <- estimate_mean_stats(d, vars = "api00")
  expect_type(res, "list")
  expect_true("api00" %in% names(res))
  expect_true(all(c("variable", "mean", "se", "bias", "mse") %in% names(res$api00)))
})

test_that("by accepts one-sided formula or character vector; rejects others", {
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  res_chr <- estimate_mean_stats(d, vars = "api00", by = "stype")
  expect_true(is.data.frame(res_chr$api00))
  expect_true("stype" %in% names(res_chr$api00))

  res_form <- estimate_mean_stats(d, vars = "api00", by = ~stype)
  expect_true(is.data.frame(res_form$api00))
  expect_true("stype" %in% names(res_form$api00))

  expect_error(
    estimate_mean_stats(d, vars = "api00", by = api00 ~ stype),
    "one-sided formula"
  )
})

test_that("overall: bias & mse computed from named population vector if provided", {
  skip_if_not_installed("survey")
  df <- data.frame(x = c(1, 2, 3, 4))
  d <- survey::svydesign(id = ~1, weights = ~1, data = df)
  res <- estimate_mean_stats(d, vars = "x", population_means = c(x = 2.5))

  out <- res$x
  expect_equal(out$variable, "x")
  expect_equal(out$mean, 2.5, tolerance = 1e-8)
  expect_equal(out$bias, 0, tolerance = 1e-8)
  expect_equal(out$mse, out$se^2, tolerance = 1e-8)
})

test_that("overall: missing name in population vector -> NA bias/mse", {
  skip_if_not_installed("survey")
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  d <- survey::svydesign(id = ~1, weights = ~1, data = df)
  res <- estimate_mean_stats(d, vars = "x", population_means = c(y = 10))
  expect_true(is.na(res$x$bias))
  expect_true(is.na(res$x$mse))
})

test_that("by-domain: requires population_means data.frame (if provided) with domain cols", {
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  bad_pm <- data.frame(not_stype = c("E", "H", "M"), api00 = c(1, 2, 3))
  expect_error(
    estimate_mean_stats(d, vars = "api00", by = ~stype, population_means = bad_pm),
    "`population_means` must include all domain variables used in `by`\\."
  )

  ok_pm <- data.frame(stype = c("E", "H", "M"), api99 = c(1, 2, 3))
  res <- estimate_mean_stats(d, vars = "api00", by = ~stype, population_means = ok_pm)
  expect_true(all(is.na(res$api00$bias)))
  expect_true(all(is.na(res$api00$mse)))
})

test_that("estimate_mean_stats errors cleanly when variable is missing from design", {
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  expect_error(
    estimate_mean_stats(d, vars = "nonexistent_var"),
    "Variables not found in design: nonexistent_var"
  )
})

test_that("estimate_mean_stats accepts one-sided by=~stype and returns domain column", {
  skip_if_not_installed("survey")
  data("api", package = "survey")
  d <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = apistrat, fpc = ~fpc
  )

  out <- estimate_mean_stats(d, vars = "api00", by = ~stype)
  expect_s3_class(out$api00, "data.frame")
  expect_true("stype" %in% names(out$api00))
  expect_true(all(c("variable", "mean", "se", "bias", "mse") %in% names(out$api00)))
})

test_that("per-variable finite filtering: errors if no finite data remain", {
  skip_if_not_installed("survey")
  df <- data.frame(w = c(1, 1, 1), x = c(NA, NA, NA))
  d <- survey::svydesign(id = ~1, weights = ~w, data = df)
  expect_error(
    estimate_mean_stats(d, vars = "x"),
    "Variable 'x' has no valid observations with finite weights"
  )
})

test_that("errors when design is not survey-like (missing $variables)", {
  bad <- list() # no $variables at all
  expect_error(
    estimate_mean_stats(bad, vars = "x"),
    "design does not look like a survey.design object"
  )
})

test_that("norm_by() accepts character / one-sided, rejects bad inputs / two-sided", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  d <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)

  out_chr <- estimate_mean_stats(d, vars = "api00", by = "stype")
  expect_true(is.data.frame(out_chr$api00))
  expect_true("stype" %in% names(out_chr$api00))

  out_form <- estimate_mean_stats(d, vars = "api00", by = ~stype)
  expect_true("stype" %in% names(out_form$api00))

  expect_error(
    estimate_mean_stats(d, vars = "api00", by = 123),
    "by must be NULL, a one-sided formula, or a character vector"
  )
  expect_error(
    estimate_mean_stats(d, vars = "api00", by = api00 ~ stype),
    "one-sided formula"
  )
})

test_that("errors when domain vars in `by` are not present in design", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  d <- survey::svydesign(id = ~1, weights = ~pw, data = apistrat)

  expect_error(
    estimate_mean_stats(d, vars = "api00", by = ~nope),
    "Domain vars not found in design: nope"
  )
})

test_that("drops non-finite weights once and still estimates", {
  skip_if_not_installed("survey")
  data(api, package = "survey")

  dat <- apistrat
  dat$pw[1] <- Inf
  d <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = dat, fpc = ~fpc)

  res <- estimate_mean_stats(d, vars = "api00")
  expect_true(is.list(res))
  expect_true(all(c("variable", "mean", "se", "bias", "mse", "rse", "p_bias") %in% names(res$api00)))
})

test_that("per-variable finite filtering: errors when no valid obs remain", {
  skip_if_not_installed("survey")
  df <- data.frame(w = c(1, 1, 1), x = c(NA, NA, NA))
  d <- survey::svydesign(id = ~1, weights = ~w, data = df)
  expect_error(
    estimate_mean_stats(d, vars = "x"),
    "Variable 'x' has no valid observations with finite weights"
  )
})

test_that("overall: population vector used; RSE NA when mean ~ 0; bias/mse NA when name missing", {
  skip_if_not_installed("survey")
  df <- data.frame(w = 1, z = c(-1, 1)) # mean 0
  d <- survey::svydesign(id = ~1, weights = ~w, data = df)
  out <- estimate_mean_stats(d, vars = "z", population_means = c(z = 0))
  expect_true(is.na(out$z$rse))

  res2 <- estimate_mean_stats(d, vars = "z", population_means = c(other = 1))
  expect_true(is.na(res2$z$bias))
  expect_true(is.na(res2$z$mse))
})

test_that("by-domain: handles both 'statistic/se' and nonstandard names; merges pop means; p_bias edges", {
  skip_if_not_installed("survey")

  df <- data.frame(
    dom = rep(c("A", "B"), each = 4),
    w = 1,
    statistic = c(1, 1, 1, 1, 2, 2, 2, 2),
    y = c(10, 11, 9, 10, 20, 20, 20, 20)
  )
  d <- survey::svydesign(id = ~1, weights = ~w, data = df)

  pop_df <- data.frame(
    dom = c("A", "B"),
    statistic = c(1, 2),
    y = c(11, 19)
  )

  res <- estimate_mean_stats(d, vars = c("statistic", "y"), by = ~dom, population_means = pop_df)

  r1 <- res$statistic
  expect_true(all(c("dom", "variable", "mean", "se", "rse", "bias", "mse", "p_bias", "truth") %in% names(r1)))

  # was: expect_equal(sort(unique(r1$dom)), c("A","B"))
  expect_equal(sort(unique(as.character(r1$dom))), c("A", "B"))

  expect_true(all(r1$se == 0))
  expect_true(all(r1$bias == 0))
  expect_true(all(r1$p_bias == 1))

  r2 <- res$y
  expect_true(all(c("dom", "variable", "mean", "se", "rse", "bias", "mse", "p_bias", "truth") %in% names(r2)))

  # robust to factor dom
  expect_equal(r2$p_bias[as.character(r2$dom) == "B"], 0)
  expect_true(is.finite(r2$p_bias[as.character(r2$dom) == "A"]))
})

test_that("by-domain: when population_means lacks the variable column, bias/mse/p remain NA", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  d <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)

  pm <- data.frame(stype = c("E", "H", "M"), other = c(1, 2, 3))
  res <- estimate_mean_stats(d, vars = "api00", by = ~stype, population_means = pm)
  expect_true(all(is.na(res$api00$bias)))
  expect_true(all(is.na(res$api00$mse)))
  expect_true(all(is.na(res$api00$p_bias)))
})

test_that("overall: population vector computes bias/mse/p_bias correctly", {
  skip_if_not_installed("survey")
  set.seed(1)
  df <- data.frame(w = 1, x = rnorm(50, mean = 2))
  d <- survey::svydesign(id = ~1, weights = ~w, data = df)

  res <- estimate_mean_stats(d, vars = "x", population_means = c(x = 2))
  out <- res$x
  expect_equal(out$variable, "x")
  expect_true(is.finite(out$mean))
  expect_equal(out$bias, out$mean - 2, tolerance = 1e-10)
  expect_equal(out$mse, out$bias^2 + out$se^2, tolerance = 1e-10)
  expect_true(out$p_bias >= 0 && out$p_bias <= 1)
})
