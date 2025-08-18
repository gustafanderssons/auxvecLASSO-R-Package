test_that("compute_weight_variation returns correct stats for typical weights", {
  weights <- c(1, 2, 2, 3, 5, 10, 10, 20)
  res <- compute_weight_variation(weights)

  expect_type(res, "list")
  expect_true(all(c(
    "min", "max", "median", "mean", "sd", "range",
    "coefficient_of_variation", "gini_index", "entropy",
    "skewness", "kurtosis", "bottom_1pct", "top_1pct"
  ) %in% names(res)))

  expect_equal(res$min, min(weights))
  expect_equal(res$max, max(weights))
  expect_equal(res$median, median(weights))
  expect_equal(res$mean, mean(weights))
  expect_equal(res$sd, sd(weights))
  expect_equal(res$range, diff(range(weights)))

  if (!isTRUE(all.equal(res$mean, 0))) {
    expect_equal(res$coefficient_of_variation, res$sd / res$mean)
  } else {
    expect_true(is.na(res$coefficient_of_variation))
  }

  expect_gte(res$gini_index, 0)
  expect_lte(res$gini_index, 1)
  expect_gte(res$entropy, 0)
  expect_true(is.finite(res$bottom_1pct))
  expect_true(is.finite(res$top_1pct))
  expect_gte(res$top_1pct, res$bottom_1pct)
})

test_that("all-equal weights behave sensibly", {
  weights <- rep(10, 20)
  res <- compute_weight_variation(weights)

  expect_equal(res$min, 10)
  expect_equal(res$max, 10)
  expect_equal(res$sd, 0)
  expect_true(is.na(res$skewness))
  expect_true(is.na(res$kurtosis))
  expect_true(res$coefficient_of_variation == 0) # mean==sd==0 -> NA
  expect_equal(res$gini_index, 0) # no inequality
  expect_true(is.finite(res$entropy))
})

test_that("empty weights warn and return all-NA metrics", {
  expect_warning(
    res <- compute_weight_variation(numeric(0)),
    "No weights provided; diagnostics not calculated\\."
  )
  expect_type(res, "list")
  expect_true(all(vapply(res, function(x) is.na(x), logical(1))))
})

test_that("weights with NA warn and return all-NA metrics", {
  expect_warning(
    res <- compute_weight_variation(c(1, 2, NA, 4)),
    "Weights contain NA/NaN/Inf; diagnostics not calculated\\."
  )
  expect_type(res, "list")
  expect_true(all(vapply(res, function(x) is.na(x), logical(1))))
})

test_that("weights with Inf warn and return all-NA metrics", {
  expect_warning(
    res <- compute_weight_variation(c(1, 2, Inf, 4)),
    "Weights contain NA/NaN/Inf; diagnostics not calculated\\."
  )
  expect_type(res, "list")
  expect_true(all(vapply(res, function(x) is.na(x), logical(1))))
})

test_that("negative weights warn but still compute metrics", {
  expect_warning(
    res <- compute_weight_variation(c(-1, 2, 3)),
    "Weights contain negative values; results may be undefined\\."
  )
  expect_type(res, "list")
  expect_true(is.numeric(res$mean) && !is.na(res$mean))
})

test_that("zero-sum weights (all zeros) produce NA for gini/entropy; sd=0 -> skew/kurt NA", {
  res <- compute_weight_variation(rep(0, 50))
  expect_equal(res$mean, 0)
  expect_equal(res$sd, 0)
  expect_true(is.na(res$gini_index))
  expect_true(is.na(res$entropy))
  expect_true(is.na(res$skewness))
  expect_true(is.na(res$kurtosis))
})

test_that("very skewed weights show high inequality and heavy tails", {
  w <- c(rep(1, 98), 100, 200) # very skewed
  res <- compute_weight_variation(w)
  expect_gt(res$gini_index, 0.5)
  expect_gte(res$kurtosis, 0) # often >= 0 for heavy tails
})
