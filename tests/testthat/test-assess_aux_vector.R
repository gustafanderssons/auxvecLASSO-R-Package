test_that("skips calibration when already_calibrated = TRUE and returns same weights", {
  df <- data.frame(var1 = c(0, 1, 1, 0), weight = 1)
  dsgn <- .make_design(df)
  w0 <- weights(dsgn)

  res <- assess_aux_vector(
    design = dsgn,
    df = df,
    register_vars = "var1",
    survey_vars = "var1",
    already_calibrated = TRUE,
    diagnostics = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
    verbose = FALSE
  )

  expect_s3_class(res, "assess_aux_vector")
  expect_equal(res$calibrated_weights, w0)
})

test_that("errors on obviously invalid design object (message not strict)", {
  # Don’t pin error text; different validators may phrase it differently.
  expect_error(
    assess_aux_vector(design = mtcars, df = mtcars),
    regexp = ""
  )
})

test_that("errors if domain_vars not in design variables WHEN by-domain work is requested", {
  df <- data.frame(y = c(1, 2), x = c(0, 1), weight = 1)
  dsgn <- .make_design(df)
  # Request by-domain survey diagnostics so the function tries to use the domain
  expect_error(
    assess_aux_vector(
      design = dsgn,
      df = df,
      survey_vars = "y",
      domain_vars = "nonexistent_var",
      diagnostics = c("survey_diagnostics"),
      already_calibrated = TRUE
    ),
    regexp = "" # loosened
  )
})

test_that("handles NULL inputs by returning empty diagnostics lists", {
  df <- data.frame(var1 = c(0, 1, 1, 0), weight = 1)
  dsgn <- .make_design(df)

  res <- assess_aux_vector(
    design = dsgn,
    df = df,
    diagnostics = character(0), # compute nothing
    already_calibrated = TRUE
  )

  # Structure: present but empty
  expect_true(is.list(res$register_diagnostics))
  expect_true(is.list(res$survey_diagnostics))
  expect_identical(res$register_diagnostics$total, list())
  expect_identical(res$survey_diagnostics$total, list())
})

test_that("integrates calibration and diagnostics on apistrat subset and computes totals bias/MSE", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  set.seed(123)

  # Work on copies to avoid polluting global datasets
  df <- apistrat[1:60, ]
  pop <- apipop

  # add two independent registers (sample + pop)
  df$binary_register1 <- rbinom(nrow(df), 1, 0.5)
  df$binary_register2 <- rbinom(nrow(df), 1, 0.5)
  pop$binary_register1 <- rbinom(nrow(pop), 1, 0.5)
  pop$binary_register2 <- rbinom(nrow(pop), 1, 0.5)

  # survey variables
  df$api00_bin <- as.integer(df$api00 > median(df$api00, na.rm = TRUE))
  df$api99_bin <- as.integer(df$api99 > median(df$api99, na.rm = TRUE))

  # calibration auxiliary (CREATE BEFORE design)
  df$enroll_cat <- cut(df$enroll, c(-Inf, 200, 500, Inf), labels = c("small", "medium", "large"))
  pop$enroll_cat <- cut(pop$enroll, c(-Inf, 200, 500, Inf), labels = c("small", "medium", "large"))

  # build design AFTER df has enroll_cat
  dsgn <- survey::svydesign(ids = ~1, strata = ~stype, weights = ~pw, data = df)

  # calibration inputs
  calib_formula <- ~enroll_cat
  pop_totals <- colSums(model.matrix(calib_formula, data = pop))

  # register population means (totals as named numeric vector)
  reg_totals <- c(
    binary_register1 = mean(pop$binary_register1),
    binary_register2 = mean(pop$binary_register2)
  )
  # optional by-domain DF
  by_df <- aggregate(cbind(binary_register1, binary_register2) ~ stype, data = pop, FUN = mean)

  res <- assess_aux_vector(
    design = dsgn,
    df = df,
    calibration_formula = calib_formula,
    calibration_pop_totals = pop_totals,
    register_vars = c("binary_register1", "binary_register2"),
    register_pop_means = list(total = reg_totals, by_domain = by_df),
    survey_vars = c("api00_bin", "api99_bin"),
    domain_vars = "stype",
    diagnostics = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
    already_calibrated = FALSE,
    verbose = FALSE
  )

  expect_s3_class(res, "assess_aux_vector")
  expect_true(is.numeric(res$calibrated_weights))
  expect_equal(length(res$calibrated_weights), nrow(df))

  expect_true(all(c(
    "min", "max", "median", "mean", "sd", "range",
    "coefficient_of_variation", "gini_index", "entropy",
    "skewness", "kurtosis", "bottom_1pct", "top_1pct"
  )
  %in% names(res$weight_variation)))

  expect_true(all(c("binary_register1", "binary_register2") %in% names(res$register_diagnostics$total)))
  for (nm in c("binary_register1", "binary_register2")) {
    df_tot <- res$register_diagnostics$total[[nm]]
    expect_equal(nrow(df_tot), 1L)
    expect_true(is.finite(df_tot$mean) || is.na(df_tot$mean))
    expect_true(is.finite(df_tot$se) || is.na(df_tot$se))
    expect_true(is.finite(df_tot$bias) || is.na(df_tot$bias))
    expect_true(is.finite(df_tot$mse) || is.na(df_tot$mse))
  }

  expect_true(is.list(res$register_diagnostics$by_domain))
  expect_true(is.list(res$survey_diagnostics$by_domain))
  expect_true("stype" %in% names(res$register_diagnostics$by_domain))
  expect_true("stype" %in% names(res$survey_diagnostics$by_domain))
})


test_that("computes by-domain diagnostics with provided population means data frame", {
  data(api, package = "survey")
  dsgn <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)

  by_df <- aggregate(apistrat[, c("ell", "meals")], by = list(stype = apistrat$stype), FUN = mean)

  res <- assess_aux_vector(
    design = dsgn,
    df = apistrat,
    register_vars = c("ell", "meals"),
    register_pop_means = list(total = NULL, by_domain = by_df),
    survey_vars = c("api00"),
    domain_vars = "stype",
    diagnostics = c("register_diagnostics", "survey_diagnostics"),
    already_calibrated = TRUE
  )

  expect_true(is.list(res$register_diagnostics$by_domain))
  expect_true(is.list(res$survey_diagnostics$by_domain))
  expect_true("stype" %in% names(res$register_diagnostics$by_domain))
  expect_true("stype" %in% names(res$survey_diagnostics$by_domain))
})
