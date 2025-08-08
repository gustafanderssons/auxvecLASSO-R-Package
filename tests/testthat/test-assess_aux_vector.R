library(testthat)
library(survey)

test_that("assess_aux_vector skips calibration if already_calibrated = TRUE", {
  df <- data.frame(
    var1 = c(0, 1, 1, 0),
    weight = c(1, 1, 1, 1)
  )
  design <- svydesign(ids = ~1, data = df, weights = ~weight)

  # Pretend weights are already calibrated
  result <- assess_aux_vector(
    design = design,
    df = df,
    register_vars = "var1",
    survey_vars = "var1",
    already_calibrated = TRUE
  )

  # Weights unchanged (should equal original weights)
  expect_equal(result$calibrated_weights, weights(design))
})

test_that("assess_aux_vector errors on invalid design input", {
  expect_error(
    assess_aux_vector(design = mtcars, df = mtcars),
    "design must be a survey.design object"
  )
})

test_that("assess_aux_vector errors if domain_vars not in design variables", {
  df <- data.frame(
    var1 = c(0, 1, 1, 0),
    weight = c(1, 1, 1, 1)
  )
  design <- svydesign(ids = ~1, data = df, weights = ~weight)

  expect_error(
    assess_aux_vector(
      design = design,
      df = df,
      domain_vars = "nonexistent_var"
    ),
    "All domain_vars must exist in the survey design data"
  )
})

test_that("assess_aux_vector handles NULL inputs gracefully", {
  df <- data.frame(
    var1 = c(0, 1, 1, 0),
    weight = c(1, 1, 1, 1)
  )
  design <- svydesign(ids = ~1, data = df, weights = ~weight)

  result <- assess_aux_vector(
    design = design,
    df = df
  )

  # No diagnostics computed, lists empty
  expect_equal(result$register_diagnostics$total, list())
  expect_equal(result$survey_diagnostics$total, list())
})

test_that("assess_aux_vector returns all diagnostics with internal calibration (realistic data)", {
  library(survey)

  data(api)

  # Subset sample for speed
  df <- apistrat[1:50, ]

  # Create two independent binary register variables unrelated to 'stype'
  set.seed(123) # for reproducibility
  df$binary_register1 <- rbinom(nrow(df), size = 1, prob = 0.5)
  df$binary_register2 <- rbinom(nrow(df), size = 1, prob = 0.5)

  # Survey variables: binary versions of api00 and api99
  df$api00_bin <- ifelse(df$api00 > median(df$api00), 1, 0)
  df$api99_bin <- ifelse(df$api99 > median(df$api99), 1, 0)
  survey_vars <- c("api00_bin", "api99_bin")

  # Create categorical enroll variable by binning
  df$enroll_cat <- cut(df$enroll,
    breaks = c(-Inf, 200, 500, Inf),
    labels = c("small", "medium", "large")
  )

  # Do the same for the population
  apipop$enroll_cat <- cut(apipop$enroll,
    breaks = c(-Inf, 200, 500, Inf),
    labels = c("small", "medium", "large")
  )

  # Create independent register variables in population as well (same probabilities)
  set.seed(123)
  apipop$binary_register1 <- rbinom(nrow(apipop), size = 1, prob = 0.5)
  apipop$binary_register2 <- rbinom(nrow(apipop), size = 1, prob = 0.5)

  # Create survey design with stratification and weights
  design <- svydesign(ids = ~1, strata = ~stype, weights = ~pw, data = df)

  # Calibration formula using enroll_cat categorical variable
  calib_formula <- ~enroll_cat

  # Population totals for calibration from full population data
  X_pop <- model.matrix(calib_formula, data = apipop)
  pop_totals <- colSums(X_pop)

  # Register population means for both registers (overall and by domain)
  register_pop_means <- list(
    total = c(
      binary_register1 = mean(apipop$binary_register1),
      binary_register2 = mean(apipop$binary_register2)
    ),
    by_domain = aggregate(
      cbind(binary_register1, binary_register2) ~ stype,
      data = apipop,
      FUN = mean
    )
  )

  # Run the function with all diagnostics requested
  result <- assess_aux_vector(
    design = design,
    df = df,
    calibration_formula = calib_formula,
    calibration_pop_totals = pop_totals,
    register_vars = c("binary_register1", "binary_register2"),
    register_pop_means = register_pop_means,
    survey_vars = survey_vars,
    domain_vars = "stype",
    diagnostics = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
    already_calibrated = FALSE
  )

  # Check class and structure
  expect_s3_class(result, "assess_aux_vector")
  expect_type(result$calibrated_weights, "double")
  expect_length(result$calibrated_weights, nrow(df))

  # Weight variation diagnostics keys (adapted to your compute_weight_variation output)
  expect_named(result$weight_variation, c(
    "min", "max", "median", "mean", "sd",
    "range", "coefficient_of_variation",
    "gini_index", "entropy", "skewness",
    "kurtosis", "bottom_1pct", "top_1pct"
  ))
  expect_true(all(sapply(result$weight_variation, is.numeric)))

  # Register diagnostics checks
  expect_true(all(c("total", "by_domain") %in% names(result$register_diagnostics)))
  expect_true(is.list(result$register_diagnostics$total))
  expect_true(is.list(result$register_diagnostics$by_domain))
  expect_true("binary_register1" %in% names(result$register_diagnostics$total))
  expect_true("binary_register2" %in% names(result$register_diagnostics$total))

  # Survey diagnostics checks
  expect_true(all(c("total", "by_domain") %in% names(result$survey_diagnostics)))
  expect_true(is.list(result$survey_diagnostics$total))
  expect_true(is.list(result$survey_diagnostics$by_domain))
  expect_true(all(survey_vars %in% names(result$survey_diagnostics$total)))
})

skip_if_not_installed("survey")

test_that("assess_aux_vector integrates calibration and diagnostics on apistrat", {
  data("api", package = "survey")
  df <- apistrat

  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = df, fpc = ~fpc
  )

  # Calibration on two auxiliaries (ell, meals)
  cal_formula <- ~ ell + meals

  # Build FEASIBLE population totals from the current design:
  # Intercept total = sum of weights; aux totals via svytotal.
  intercept_total <- sum(stats::weights(dsgn))
  aux_totals <- as.numeric(coef(survey::svytotal(~ ell + meals, dsgn)))
  names(aux_totals) <- c("ell", "meals")

  pop_vec <- c("(Intercept)" = intercept_total, aux_totals)

  # Register & survey vars
  register_vars <- c("ell", "meals")
  survey_vars   <- c("api00", "api99")

  res <- assess_aux_vector(
    design = dsgn,
    df = df,
    calibration_formula = cal_formula,
    calibration_pop_totals = pop_vec,
    register_vars = register_vars,
    survey_vars = survey_vars,
    diagnostics = c("weight_variation", "register_diagnostics", "survey_diagnostics"),
    already_calibrated = FALSE,
    verbose = FALSE
  )

  # Structure checks
  expect_s3_class(res, "assess_aux_vector")
  expect_true(all(c("calibrated_weights","weight_variation","register_diagnostics","survey_diagnostics") %in% names(res)))
  expect_true(is.numeric(res$calibrated_weights))
  expect_equal(length(res$calibrated_weights), nrow(df))

  # weight variation looks like the expected named list
  expect_true(is.list(res$weight_variation))
  expect_true(all(c("min","max","median","mean","sd","range",
                    "coefficient_of_variation","gini_index","entropy",
                    "skewness","kurtosis","bottom_1pct","top_1pct")
                  %in% names(res$weight_variation)))

  # register diagnostics present (overall)
  expect_true(is.list(res$register_diagnostics$total))
  # survey diagnostics present (overall)
  expect_true(is.list(res$survey_diagnostics$total))
})

test_that("assess_aux_vector computes by-domain diagnostics with provided population means data frame", {
  data(api, package = "survey")
  df <- apistrat

  dsgn <- survey::svydesign(
    id = ~1, strata = ~stype, weights = ~pw,
    data = df, fpc = ~fpc
  )

  domain_vars <- "stype"
  # Build pop means by domain for two register vars (toy example based on sample means)
  # In real use you'd supply true population means.
  by_df <- aggregate(
    df[, c("ell","meals")],
    by = list(stype = df$stype),
    FUN = mean
  )
  names(by_df)[names(by_df) == "ell"]   <- "ell"
  names(by_df)[names(by_df) == "meals"] <- "meals"

  res <- assess_aux_vector(
    design = dsgn,
    df = df,
    register_vars = c("ell","meals"),
    register_pop_means = list(total = NULL, by_domain = by_df),
    survey_vars = c("api00"),
    domain_vars = domain_vars,
    diagnostics = c("register_diagnostics","survey_diagnostics"),
    already_calibrated = TRUE,  # use original weights here
    verbose = FALSE
  )

  # by-domain results should be present
  expect_true(is.list(res$register_diagnostics$by_domain))
  expect_true(is.list(res$survey_diagnostics$by_domain))
})
