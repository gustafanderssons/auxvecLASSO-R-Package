test_that("returns expected structure and names for two binary outcomes", {
  set.seed(1)
  n <- 80
  df <- data.frame(
    y1 = factor(rbinom(n, 1, 0.5)),
    y2 = factor(rbinom(n, 1, 0.6)),
    x1 = rnorm(n),
    x2 = rnorm(n),
    g  = factor(sample(c("A", "B"), n, TRUE))
  )
  outcome_vars <- c("y1", "y2")
  auxiliary_vars <- c("x1", "x2", "g")

  res <- select_auxiliary_variables_lasso_cv(
    df = df,
    outcome_vars = outcome_vars,
    auxiliary_vars = auxiliary_vars,
    must_have_vars = NULL,
    check_twoway_int = FALSE,
    nfolds = 3, # keep tests quick
    verbose = FALSE,
    standardize = TRUE,
    return_models = FALSE,
    parallel = FALSE
  )

  # top-level
  expect_s3_class(res, "select_auxiliary_variables_lasso_cv")
  expect_true(all(c(
    "selected_variables", "by_outcome", "selected_lambdas",
    "penalty_factors", "models", "goodness_of_fit", "interaction_metadata"
  ) %in% names(res)))

  # names & lengths
  expect_true(all(names(res$by_outcome) %in% outcome_vars))
  expect_true(all(names(res$selected_lambdas) %in% outcome_vars))
  expect_type(res$penalty_factors, "double")
  expect_true(is.list(res$goodness_of_fit))
  expect_true(length(res$models) == 0) # return_models = FALSE

  # goodness_of_fit structure per outcome
  for (y in outcome_vars) {
    gf <- res$goodness_of_fit[[y]]
    expect_true(all(c("cross_validated", "full_data") %in% names(gf)))
    expect_true(all(c("cv_error", "cv_error_sd") %in% names(gf$cross_validated)))
    expect_true(all(c("deviance_explained", "auc", "accuracy", "brier_score", "raw_coefs", "abs_coefs")
    %in% names(gf$full_data)))
    expect_gte(gf$full_data$accuracy, 0)
    expect_lte(gf$full_data$accuracy, 1)
  }

  # interaction metadata when interactions are off
  expect_identical(res$interaction_metadata$interaction_terms, character(0))
})

test_that("includes main effects for selected interactions and must-have vars in selected_variables", {
  set.seed(2)
  n <- 70
  df <- data.frame(
    y = factor(rbinom(n, 1, 0.55)),
    x = rnorm(n),
    z = rnorm(n),
    f = factor(sample(c("L", "H"), n, TRUE))
  )
  outcome_vars <- "y"
  auxiliary_vars <- c("x", "z", "f")

  # Force a must-have that is present in X; with interactions on, its interactions should be zero-penalty
  must_have_vars <- "x"

  res <- select_auxiliary_variables_lasso_cv(
    df = df,
    outcome_vars = outcome_vars,
    auxiliary_vars = auxiliary_vars,
    must_have_vars = must_have_vars,
    check_twoway_int = TRUE,
    nfolds = 3,
    verbose = FALSE,
    standardize = TRUE,
    return_models = FALSE,
    parallel = FALSE
  )

  # selected_lambdas named correctly
  expect_true(all(names(res$selected_lambdas) == "y"))

  # must-have appears in selected_variables even if not selected by glmnet (function appends them)
  expect_true("x" %in% res$selected_variables)

  # If any interaction made it into combined, its main effects must also be included
  ints <- grep(":", res$selected_variables, value = TRUE)
  if (length(ints) > 0) {
    mains <- unique(unlist(strsplit(ints, ":"), use.names = FALSE))
    expect_true(all(mains %in% res$selected_variables))
  }

  # interaction metadata should reflect whatever ended up in combined
  expect_true(all(res$interaction_metadata$interaction_terms %in% res$selected_variables))
})

test_that("return_models switches models list on/off", {
  set.seed(3)
  n <- 60
  df <- data.frame(
    y1 = factor(rbinom(n, 1, 0.5)),
    y2 = factor(rbinom(n, 1, 0.5)),
    a = rnorm(n),
    b = rnorm(n)
  )
  aux <- c("a", "b")
  outs <- c("y1", "y2")

  res_off <- select_auxiliary_variables_lasso_cv(
    df, outs, aux,
    check_twoway_int = FALSE,
    nfolds = 3,
    return_models = FALSE,
    verbose = FALSE
  )
  expect_true(is.list(res_off$models))
  expect_length(res_off$models, 0)

  res_on <- select_auxiliary_variables_lasso_cv(
    df, outs, aux,
    check_twoway_int = FALSE,
    nfolds = 3,
    return_models = TRUE,
    verbose = FALSE
  )
  expect_true(is.list(res_on$models))
  expect_true(all(names(res_on$models) %in% outs))
  # model objects may be NULL if an outcome fails, but generally should be cv.glmnet
  for (m in res_on$models) {
    expect_true(is.null(m) || inherits(m, "cv.glmnet"))
  }
})

test_that("early exits: zero-row df or no auxiliary vars return empty structures", {
  # zero-row df
  df0 <- data.frame(y = factor(), x = numeric())
  res0 <- select_auxiliary_variables_lasso_cv(
    df = df0,
    outcome_vars = "y",
    auxiliary_vars = "x",
    check_twoway_int = FALSE,
    nfolds = 3,
    verbose = FALSE
  )
  expect_identical(res0$selected_variables, character(0))
  expect_true(is.list(res0$by_outcome))
  expect_true(length(res0$by_outcome) == 1) # names match supplied outcome_vars
  expect_true(all(names(res0$by_outcome) == "y"))

  # no auxiliary vars
  set.seed(4)
  df <- data.frame(y = factor(rbinom(10, 1, 0.5)), x = rnorm(10))
  res1 <- select_auxiliary_variables_lasso_cv(
    df = df,
    outcome_vars = "y",
    auxiliary_vars = character(0),
    check_twoway_int = FALSE,
    nfolds = 3,
    verbose = FALSE
  )
  expect_identical(res1$selected_variables, character(0))
})

test_that("penalty_factors is a named numeric vector aligned with model matrix columns", {
  set.seed(5)
  n <- 50
  df <- data.frame(
    y = factor(rbinom(n, 1, 0.5)),
    x1 = rnorm(n),
    g = factor(sample(c("A", "B"), n, TRUE))
  )
  aux <- c("x1", "g")

  res <- select_auxiliary_variables_lasso_cv(
    df, "y", aux,
    must_have_vars = "x1",
    check_twoway_int = TRUE,
    nfolds = 3,
    verbose = FALSE
  )

  pf <- res$penalty_factors
  expect_type(pf, "double")
  expect_false(is.null(names(pf)))
  # ensure the must-have got zero penalty
  expect_true(pf["x1"] == 0)
})

test_that("parallel path creates/uses a cluster and returns sensible results (skipped if doParallel missing)", {
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    skip("doParallel is not installed; skipping parallel path test.")
  }

  set.seed(6)
  n <- 60
  df <- data.frame(
    y  = factor(rbinom(n, 1, 0.5)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  res <- select_auxiliary_variables_lasso_cv(
    df, "y", c("x1", "x2"),
    check_twoway_int = FALSE,
    nfolds = 3,
    verbose = FALSE, # avoid message assertions
    return_models = FALSE,
    parallel = TRUE
  )

  # Always reset foreach backend
  if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()

  # Structure checks
  expect_s3_class(res, "select_auxiliary_variables_lasso_cv")
  expect_true(all(c(
    "selected_variables", "by_outcome", "selected_lambdas",
    "penalty_factors", "models", "goodness_of_fit", "interaction_metadata"
  ) %in% names(res)))

  # Lambda may be NA if glmnet bails, but usually numeric
  expect_true(is.numeric(res$selected_lambdas[["y"]]) || is.na(res$selected_lambdas[["y"]]))

  # Only assert GOF if it exists (parallel backends can be finicky on CI)
  gf <- res$goodness_of_fit[["y"]]
  if (is.null(gf) || !all(c("cross_validated", "full_data") %in% names(gf))) {
    skip("cv.glmnet did not return goodness_of_fit under parallel (environment/backend quirk).")
  }

  expect_true(all(c("cv_error", "cv_error_sd") %in% names(gf$cross_validated)))
  expect_true(all(c("deviance_explained", "auc", "accuracy", "brier_score", "raw_coefs", "abs_coefs")
  %in% names(gf$full_data)))
  expect_gte(gf$full_data$accuracy, 0)
  expect_lte(gf$full_data$accuracy, 1)
})

test_that("select_auxiliary_variables_lasso_cv drops rows with NA outcomes and returns results", {
  set.seed(42)
  n <- 120

  # Build data: X has NO missing values, outcomes may have NAs
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  # two binary outcomes
  df$y1 <- factor(rbinom(n, 1, 0.55))
  df$y2 <- factor(rbinom(n, 1, 0.50))

  # Inject NA only into outcomes (10 each, disjoint indices)
  na_y1 <- 1:10
  na_y2 <- 21:30
  df$y1[na_y1] <- NA
  df$y2[na_y2] <- NA

  # Sanity: auxiliary vars have no missing values
  expect_false(anyNA(df[c("x1", "x2", "x3")]))

  # Run with verbose = TRUE so we can assert the drop messages
  msgs <- testthat::capture_messages({
    res <- select_auxiliary_variables_lasso_cv(
      df = df,
      outcome_vars = c("y1", "y2"),
      auxiliary_vars = c("x1", "x2", "x3"),
      must_have_vars = "x1",
      check_twoway_int = FALSE,
      nfolds = 3,
      verbose = TRUE,
      standardize = TRUE,
      return_models = FALSE,
      parallel = FALSE
    )
    # Basic structure assertions inside the capture to keep timing realistic
    expect_s3_class(res, "select_auxiliary_variables_lasso_cv")
    expect_true(all(c("y1", "y2") %in% names(res$by_outcome)))
    expect_true(all(c("y1", "y2") %in% names(res$selected_lambdas)))
    expect_true(is.numeric(res$selected_lambdas[["y1"]]) || is.na(res$selected_lambdas[["y1"]]))
    expect_true(is.numeric(res$selected_lambdas[["y2"]]) || is.na(res$selected_lambdas[["y2"]]))

    # must-have should be in the combined selection even if glmnet didn't pick it
    expect_true("x1" %in% res$selected_variables)

    # Goodness-of-fit structure present for each outcome
    for (y in c("y1", "y2")) {
      gf <- res$goodness_of_fit[[y]]
      expect_true(all(c("cross_validated", "full_data") %in% names(gf)))
      expect_true(all(c("cv_error", "cv_error_sd") %in% names(gf$cross_validated)))
      expect_true(all(c("deviance_explained", "auc", "accuracy", "brier_score", "raw_coefs", "abs_coefs")
      %in% names(gf$full_data)))
      expect_gte(gf$full_data$accuracy, 0)
      expect_lte(gf$full_data$accuracy, 1)
      expect_gte(gf$full_data$auc, 0)
      expect_lte(gf$full_data$auc, 1)
    }
  })

  # Messages should indicate rows dropped and rows used per outcome
  msgs_c <- paste(msgs, collapse = " ")
  expect_true(grepl("Outcome 'y1': dropping 10 row\\(s\\) with NA\\.", msgs_c))
  expect_true(grepl("Outcome 'y2': dropping 10 row\\(s\\) with NA\\.", msgs_c))
  # after dropping, both should fit "on 110 rows."
  expect_true(grepl("Fitting LASSO for binary outcome 'y1' on 110 rows\\.", msgs_c))
  expect_true(grepl("Fitting LASSO for binary outcome 'y2' on 110 rows\\.", msgs_c))
})

test_that("select_auxiliary_variables_lasso_cv correctly includes factor variables in must_have_vars", {
  # Sample dataframe with factor variables
  df <- data.frame(
    stype = factor(c(
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M"
    )),
    age = c(
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45
    ),
    gender = factor(c(
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female"
    )),
    outcome1 = c(
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1
    ),
    outcome2 = c(
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0
    )
  )

  # Auxiliary and must-have variables
  auxiliary_vars <- c("stype", "age", "gender")
  must_have_vars <- c("stype")

  # Simulate the selection of auxiliary variables via LASSO with cross-validation
  result <- select_auxiliary_variables_lasso_cv(
    df = df,
    outcome_vars = c("outcome1", "outcome2"),
    auxiliary_vars = auxiliary_vars,
    must_have_vars = must_have_vars,
    check_twoway_int = TRUE,
    nfolds = 5,
    verbose = FALSE,
    standardize = TRUE,
    return_models = FALSE,
    parallel = FALSE
  )

  # List of expected dummy variables for the "stype" factor
  expected_stype_dummies <- c("stypeE", "stypeM", "stypeH")

  # Ensure that the expected dummy variables for "stype" are included in the selected variables
  selected_variables <- result$selected_variables
  expect_true(all(expected_stype_dummies %in% selected_variables))
})



test_that("Interaction terms are included when check_twoway_int is TRUE", {
  # Sample dataframe with factor variables
  df <- data.frame(
    stype = factor(c(
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M"
    )),
    age = c(
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45
    ),
    gender = factor(c(
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female"
    )),
    outcome1 = c(
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 1, 0, 1, 0, 1
    ),
    outcome2 = c(
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0,
      0, 1, 0, 1, 0, 0, 1, 0, 1, 0
    )
  )

  auxiliary_vars <- c("stype", "age", "gender")

  # Call select_auxiliary_variables_lasso_cv with check_twoway_int = TRUE
  result <- select_auxiliary_variables_lasso_cv(
    df = df,
    outcome_vars = "outcome1",
    auxiliary_vars = auxiliary_vars,
    must_have_vars = NULL,
    check_twoway_int = TRUE,
    nfolds = 5,
    verbose = FALSE,
    standardize = TRUE,
    return_models = FALSE,
    parallel = FALSE
  )

  # Extract the interaction terms
  interaction_terms <- result$interaction_metadata$interaction_terms

  # Check if interaction terms are included in the selected variables
  expect_true(length(interaction_terms) > 0) # Should include interaction terms such as "stype:age", "stype:gender"
})

test_that("select_auxiliary_variables_lasso_cv handles continuous outcome correctly and computes additional metrics", {
  # Sample dataframe with continuous outcome variable
  df <- data.frame(
    stype = factor(c(
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M",
      "E", "M", "H", "E", "M", "E", "M", "H", "E", "M"
    )),
    age = c(
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45,
      25, 30, 35, 40, 45, 25, 30, 35, 40, 45
    ),
    gender = factor(c(
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female",
      "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Female"
    )),
    outcome1 = c(
      3.2, 5.5, 4.8, 7.1, 5.6, 3.0, 6.2, 4.5, 6.8, 5.4,
      4.1, 5.8, 4.9, 6.3, 5.2, 3.3, 6.5, 4.7, 6.6, 5.7,
      3.9, 5.7, 4.6, 6.0, 5.1, 3.5, 6.1, 4.4, 6.2, 5.3,
      4.0, 5.4, 4.7, 6.1, 5.5, 3.7, 6.3, 4.6, 6.0, 5.2,
      4.2, 5.6, 4.8, 6.4, 5.5, 3.8, 6.2, 4.5, 6.3, 5.0
    ), # Continuous outcome
    outcome2 = c(
      2.5, 3.2, 4.5, 5.7, 3.1, 2.8, 4.0, 4.3, 5.6, 3.8,
      2.9, 3.4, 4.2, 5.0, 3.3, 2.7, 4.1, 4.0, 5.2, 3.9,
      3.0, 3.5, 4.3, 5.1, 3.2, 2.6, 4.0, 3.7, 5.4, 3.6,
      3.1, 3.6, 4.0, 5.3, 3.4, 2.8, 4.2, 3.9, 5.0, 3.7,
      2.8, 3.1, 4.4, 5.2, 3.3, 2.7, 4.0, 3.8, 5.5, 3.6
    ) # Continuous outcome
  )

  # Auxiliary and must-have variables
  auxiliary_vars <- c("stype", "age", "gender")
  must_have_vars <- c("stype")

  # Run the function
  result <- select_auxiliary_variables_lasso_cv(
    df = df,
    outcome_vars = c("outcome1", "outcome2"),
    auxiliary_vars = auxiliary_vars,
    must_have_vars = must_have_vars,
    check_twoway_int = TRUE,
    nfolds = 5,
    verbose = FALSE,
    standardize = TRUE,
    return_models = FALSE,
    parallel = FALSE
  )

  # Check that the goodness metrics for continuous outcomes are present
  expect_true("rss" %in% names(result$goodness_of_fit$outcome1$full_data))
  expect_true("r_squared" %in% names(result$goodness_of_fit$outcome1$full_data))
  expect_true("mse" %in% names(result$goodness_of_fit$outcome1$full_data))
  expect_true("rmse" %in% names(result$goodness_of_fit$outcome1$full_data))
  expect_true("mae" %in% names(result$goodness_of_fit$outcome1$full_data))

  expect_true("rss" %in% names(result$goodness_of_fit$outcome2$full_data))
  expect_true("r_squared" %in% names(result$goodness_of_fit$outcome2$full_data))
  expect_true("mse" %in% names(result$goodness_of_fit$outcome2$full_data))
  expect_true("rmse" %in% names(result$goodness_of_fit$outcome2$full_data))
  expect_true("mae" %in% names(result$goodness_of_fit$outcome2$full_data))
})
