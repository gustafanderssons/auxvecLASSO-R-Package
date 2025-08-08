test_that("setup_parallel returns NULL and is silent when parallel = FALSE", {
  expect_silent({
    cl <- setup_parallel(parallel = FALSE, verbose = TRUE)
  })
  expect_null(cl)
})

test_that("setup_parallel creates a cluster and prints message when verbose = TRUE", {
  skip_if_not_installed("doParallel")

  n_expected <- max(parallel::detectCores() - 1, 1)

  cl <- NULL
  expect_message(
    cl <- setup_parallel(parallel = TRUE, verbose = TRUE),
    regexp = "Using \\d+ cores for parallel CV\\."
  )

  # Always stop the cluster after the test
  on.exit({
    if (!is.null(cl)) parallel::stopCluster(cl)
    if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()
  }, add = TRUE)

  expect_true(inherits(cl, "cluster"))
  expect_equal(length(cl), n_expected)
})

test_that("setup_parallel creates a cluster and is silent when verbose = FALSE", {
  skip_if_not_installed("doParallel")

  n_expected <- max(parallel::detectCores() - 1, 1)

  expect_silent({
    cl <- setup_parallel(parallel = TRUE, verbose = FALSE)
  })

  on.exit({
    if (!is.null(cl)) parallel::stopCluster(cl)
    if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()
  }, add = TRUE)

  expect_true(inherits(cl, "cluster"))
  expect_equal(length(cl), n_expected)
})
