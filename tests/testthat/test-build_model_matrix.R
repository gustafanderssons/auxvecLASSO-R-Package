skip_if_not_installed("withr")

test_that("build_model_matrix returns expected structure without interactions", {
  withr::local_options(contrasts = c("contr.treatment", "contr.poly"))

  set.seed(1)
  df <- data.frame(
    x_num = rnorm(10),
    x_fac = factor(sample(c("A", "B"), 10, replace = TRUE), levels = c("A", "B"))
  )
  aux <- c("x_num", "x_fac")

  res <- build_model_matrix(df, auxiliary_vars = aux, check_twoway_int = FALSE)

  expect_type(res, "list")
  expect_true(all(c("X", "formula_str") %in% names(res)))
  expect_identical(res$formula_str, "~ x_num + x_fac -1")

  expect_true(is.matrix(res$X))
  expect_equal(nrow(res$X), nrow(df))

  cn <- colnames(res$X)
  expect_true("x_num" %in% cn)

  # Factor main effects: columns that start with x_fac AND have no ':'
  f_cols <- grep("^x_fac", cn, value = TRUE)
  f_cols <- f_cols[!grepl(":", f_cols)]
  # With -1, expect either k or (k-1) depending on contrasts; here we pinned treatment, so k
  expect_true(length(f_cols) %in% c(nlevels(df$x_fac) - 1, nlevels(df$x_fac)))

  expect_false("(Intercept)" %in% cn)
})

test_that("build_model_matrix returns expected structure with two-way interactions", {
  withr::local_options(contrasts = c("contr.treatment", "contr.poly"))

  set.seed(2)
  df <- data.frame(
    x1 = rnorm(12),
    x2 = rnorm(12),
    g  = factor(sample(c("G1", "G2"), 12, replace = TRUE), levels = c("G1", "G2"))
  )
  aux <- c("x1", "x2", "g")

  res <- build_model_matrix(df, auxiliary_vars = aux, check_twoway_int = TRUE)
  expect_identical(res$formula_str, "~ (x1 + x2 + g)^2 -1")

  X <- res$X
  cn <- colnames(X)
  expect_true(is.matrix(X))

  # Main effects (exclude any column with ':')
  expect_true("x1" %in% cn)
  expect_true("x2" %in% cn)
  g_cols <- grep("^g", cn, value = TRUE)
  g_cols <- g_cols[!grepl(":", g_cols)]
  expect_true(length(g_cols) %in% c(nlevels(df$g) - 1, nlevels(df$g)))

  # Interactions: numeric:factor -> expect k-1 or k columns
  x1g <- grep("^(x1:g|g:x1)", cn, value = TRUE)
  x2g <- grep("^(x2:g|g:x2)", cn, value = TRUE)
  expect_true(length(unique(x1g)) %in% c(nlevels(df$g) - 1, nlevels(df$g)))
  expect_true(length(unique(x2g)) %in% c(nlevels(df$g) - 1, nlevels(df$g)))

  # numeric:numeric -> one column, either order
  expect_true(any(c("x1:x2", "x2:x1") %in% cn))

  expect_false("(Intercept)" %in% cn)
  expect_equal(nrow(X), nrow(df))
})

test_that("build_model_matrix handles single auxiliary variable", {
  withr::local_options(contrasts = c("contr.treatment", "contr.poly"))

  df <- data.frame(z = rnorm(7))
  aux <- "z"

  res <- build_model_matrix(df, auxiliary_vars = aux, check_twoway_int = FALSE)
  expect_identical(res$formula_str, "~ z -1")
  expect_true(is.matrix(res$X))
  expect_identical(colnames(res$X), "z")
  expect_equal(nrow(res$X), nrow(df))
})

test_that("build_model_matrix errors when an auxiliary variable is missing in df", {
  withr::local_options(contrasts = c("contr.treatment", "contr.poly"))

  df <- data.frame(a = rnorm(5))
  aux <- c("a", "b_missing")

  expect_error(
    build_model_matrix(df, auxiliary_vars = aux, check_twoway_int = FALSE),
    regexp = "object 'b_missing' not found|undefined columns selected"
  )
})

test_that("column set contains a reasonable number of interactions for factor:factor combos", {
  withr::local_options(contrasts = c("contr.treatment", "contr.poly"))

  set.seed(3)
  df <- data.frame(
    f1 = factor(sample(c("L1", "L2"), 20, TRUE), levels = c("L1", "L2")),
    f2 = factor(sample(c("M1", "M2", "M3"), 20, TRUE), levels = c("M1", "M2", "M3"))
  )
  aux <- c("f1", "f2")

  res <- build_model_matrix(df, auxiliary_vars = aux, check_twoway_int = TRUE)
  cn <- colnames(res$X)

  # Main effects: only columns without ':' and starting with f1 / f2
  f1_cols <- grep("^f1", cn, value = TRUE)
  f1_cols <- f1_cols[!grepl(":", f1_cols)]
  f2_cols <- grep("^f2", cn, value = TRUE)
  f2_cols <- f2_cols[!grepl(":", f2_cols)]

  # Allow either k or k-1 columns depending on contrasts
  expect_true(length(f1_cols) %in% c(nlevels(df$f1) - 1, nlevels(df$f1)))
  expect_true(length(f2_cols) %in% c(nlevels(df$f2) - 1, nlevels(df$f2)))

  # Interactions between f1 and f2: columns containing BOTH f1 and f2 with a ':'
  int_cols <- grep(":", cn, value = TRUE)
  int_cols <- int_cols[grepl("^f1", int_cols) | grepl("^f2", int_cols)]
  int_cols <- int_cols[grepl("f1", int_cols) & grepl("f2", int_cols)]
  int_cols <- unique(int_cols)

  # Lower bound (treatment contrasts): (k1-1)*(k2-1)
  lower_bound <- (nlevels(df$f1) - 1) * (nlevels(df$f2) - 1)
  # Upper bound (no contrasts / full dummies): k1 * k2
  upper_bound <- nlevels(df$f1) * nlevels(df$f2)

  expect_gte(length(int_cols), lower_bound)
  expect_lte(length(int_cols), upper_bound)
  expect_gt(length(int_cols), 0)
})
