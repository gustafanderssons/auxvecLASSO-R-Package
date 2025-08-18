# tests/testthat/test-setup-parallel.R

test_that("setup_parallel returns NULL and is silent when parallel = FALSE", {
  expect_silent({
    cl <- setup_parallel(parallel = FALSE, verbose = TRUE)
  })
  expect_null(cl)
})

test_that("setup_parallel creates a cluster and prints message when verbose = TRUE", {
  testthat::skip_if_not_installed("doParallel")
  # Keep the message but don't depend on the exact core count.
  cl <- NULL
  expect_message(
    cl <- setup_parallel(parallel = TRUE, verbose = TRUE),
    regexp = "Using"
  )

  # Cleanup: stop the cluster and reset foreach backend
  on.exit(
    {
      if (!is.null(cl)) parallel::stopCluster(cl)
      if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()
    },
    add = TRUE
  )

  expect_true(inherits(cl, "cluster"))
})

test_that("setup_parallel honors max_cores cap deterministically (message exact)", {
  # By setting max_cores = 1, we ensure "Using 1 cores..." regardless of host CPU.
  cl <- NULL
  expect_message(
    cl <- setup_parallel(parallel = TRUE, verbose = TRUE, max_cores = 1),
    regexp = "^\\s*Using\\s+1\\s+core(?:s)?\\s+for\\s+parallel\\s+CV\\.?\\s*$",
    perl = TRUE
  )

  on.exit(
    {
      if (!is.null(cl)) parallel::stopCluster(cl)
      if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()
    },
    add = TRUE
  )

  expect_true(inherits(cl, "cluster"))
  expect_true(length(cl) > 0)
})

test_that("setup_parallel creates a cluster and is silent when verbose = FALSE", {
  testthat::skip_if_not_installed("doParallel")

  expect_silent({
    cl <- setup_parallel(parallel = TRUE, verbose = FALSE)
  })

  on.exit(
    {
      if (!is.null(cl)) parallel::stopCluster(cl)
      if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()
    },
    add = TRUE
  )

  expect_true(inherits(cl, "cluster"))
  expect_true(length(cl) > 0)
})
