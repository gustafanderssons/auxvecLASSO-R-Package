library(testthat)

test_that("compute_weight_variation returns correct stats for typical weights", {
  weights <- c(1, 2, 3, 4, 5, 6)
  res <- compute_weight_variation(weights)

  expect_type(res, "list")
  expect_true(all(c(
    "min","max","median","mean","sd","range",
    "coefficient_of_variation","gini_index","entropy",
    "skewness","kurtosis","bottom_1pct","top_1pct"
  ) %in% names(res)))

  expect_equal(res$min, min(weights))
  expect_equal(res$max, max(weights))
  expect_equal(res$median, median(weights))
  expect_equal(res$mean, mean(weights))
  expect_equal(res$sd, sd(weights))
  expect_equal(res$range, max(weights) - min(weights))
  expect_equal(res$coefficient_of_variation, sd(weights) / mean(weights))

  # Gini index should be between 0 and 1 (allow tiny numerical blips)
  expect_gte(res$gini_index, -1e-12)
  expect_lte(res$gini_index, 1 + 1e-12)

  # Entropy should be >= 0
  expect_gte(res$entropy, 0)

  # Skewness and kurtosis should be numeric and not NA when sd > 0
  expect_true(is.numeric(res$skewness) && !is.na(res$skewness))
  expect_true(is.numeric(res$kurtosis) && !is.na(res$kurtosis))

  # Percentiles within range
  expect_gte(res$bottom_1pct, min(weights))
  expect_lte(res$bottom_1pct, max(weights))
  expect_gte(res$top_1pct, min(weights))
  expect_lte(res$top_1pct, max(weights))
})

test_that("compute_weight_variation handles all-equal weights", {
  weights <- rep(10, 100)
  res <- compute_weight_variation(weights)

  expect_equal(res$min, 10)
  expect_equal(res$max, 10)
  expect_equal(res$median, 10)
  expect_equal(res$mean, 10)
  expect_equal(res$sd, 0)
  expect_equal(res$range, 0)
  expect_equal(res$coefficient_of_variation, 0)

  # Gini for equal weights is 0
  expect_equal(res$gini_index, 0)

  # Entropy = log(n) for uniform distribution
  expect_equal(res$entropy, log(length(weights)), tolerance = 1e-12)

  # With sd = 0, skewness and kurtosis are undefined -> NA
  expect_true(is.na(res$skewness))
  expect_true(is.na(res$kurtosis))
})

test_that("empty weights warn and return all-NA metrics", {
  expect_warning(
    res <- compute_weight_variation(numeric(0)),
    regexp = "No weights provided"
  )
  expect_type(res, "list")
  expect_true(all(vapply(res, function(x) is.na(x), logical(1))))
})

test_that("weights with NA warn and return all-NA metrics", {
  expect_warning(
    res <- compute_weight_variation(c(1, 2, NA, 4)),
    regexp = "Weights contain NA/NaN/Inf"
  )
  expect_type(res, "list")
  expect_true(all(vapply(res, function(x) is.na(x), logical(1))))
})

test_that("weights with Inf warn and return all-NA metrics", {
  expect_warning(
    res <- compute_weight_variation(c(1, 2, Inf, 4)),
    regexp = "Weights contain NA/NaN/Inf"
  )
  expect_type(res, "list")
  expect_true(all(vapply(res, function(x) is.na(x), logical(1))))
})

test_that("negative weights warn but still compute metrics", {
  expect_warning(
    res <- compute_weight_variation(c(-1, 2, 3)),
    regexp = "negative values"
  )
  expect_type(res, "list")
  # mean/sd should be numeric; CV can be numeric if mean != 0
  expect_true(is.numeric(res$mean) && !is.na(res$mean))
  expect_true(is.numeric(res$sd) && !is.na(res$sd))
  # gini is computed; may not strictly be [0,1] with negatives, we only assert numeric
  expect_true(is.numeric(res$gini_index))
  # entropy defined if sum_w > 0
  # skewness/kurtosis defined if sd > 0
})

test_that("zero-sum weights (all zeros) produce NA for gini and entropy; sd=0 -> skew/kurt NA", {
  w <- rep(0, 10)
  res <- compute_weight_variation(w)
  expect_equal(res$mean, 0)
  expect_equal(res$sd, 0)
  expect_true(is.na(res$coefficient_of_variation)) # 0 / 0 -> NA by design
  expect_true(is.na(res$gini_index))
  expect_true(is.na(res$entropy))
  expect_true(is.na(res$skewness))
  expect_true(is.na(res$kurtosis))
})

test_that("very skewed weights show high inequality and heavy tails", {
  set.seed(1)
  weights <- c(rep(1, 99), 1000)
  res <- compute_weight_variation(weights)

  expect_gt(res$gini_index, 0.5)  # high inequality
  expect_gt(res$skewness, 0)
  expect_gt(res$kurtosis, 0)

  expect_equal(res$top_1pct, max(weights))           # top_1% should include the max
  expect_gte(res$bottom_1pct, min(weights))          # within range
  expect_lte(res$bottom_1pct, max(weights))
})
