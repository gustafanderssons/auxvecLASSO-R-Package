test_that("prints full summary across branches and returns invisibly", {
  testthat::skip_if_not_installed("crayon")
  withr::local_options(crayon.enabled = FALSE) # plain text output

  # ----- Build a rich object covering both outcome types -----

  sel_vars <- c("x1", "x2")
  by_outcome <- list(
    y_cont = c("x1"),
    y_bin  = character(0)
  )
  selected_lambdas <- list(
    y_cont = 0.012345,
    y_bin  = 0.1
  )
  penalty_factors <- c(0, 1, 1, 0, 2)
  models <- list(list(), list())

  gof_cont <- list(
    cross_validated = list(cv_error = 0.123456, cv_error_sd = 0.00987),
    full_data = list(
      r_squared = 0.87654,
      mse = 1.2345,
      rmse = 1.1111,
      mae = 0.9876,
      raw_coefs = c(`(Intercept)` = 0.0000, x2 = -1.2, x1 = 0.5)
    )
  )
  gof_bin <- list(
    cross_validated = list(cv_error = 0.2222, cv_error_sd = 0.0333),
    full_data = list(
      deviance_explained = 0.4321,
      auc = 0.9012,
      accuracy = 0.8123,
      brier_score = 0.1234,
      raw_coefs = NULL
    )
  )
  goodness_of_fit <- list(
    y_cont = gof_cont,
    y_bin  = gof_bin
  )
  interaction_metadata <- list(
    interaction_terms = c("x1:x2", "x2:x3"),
    main_effects_in_interactions = c("x1", "x2", "x3"),
    full_formula = "y ~ (x1 + x2 + x3)^2"
  )

  x <- structure(list(
    selected_variables = sel_vars,
    by_outcome = by_outcome,
    selected_lambdas = selected_lambdas,
    penalty_factors = penalty_factors,
    models = models,
    goodness_of_fit = goodness_of_fit,
    interaction_metadata = interaction_metadata
  ), class = "select_auxiliary_variables_lasso_cv")

  # Capture text and check invisibility of print(x) itself
  txt <- paste(capture.output(print(x)), collapse = "\n")
  expect_false(withVisible(print(x))$visible)

  # ---- Headings and section markers ----
  expect_match(txt, "LASSO Auxiliary Variable Selection", perl = TRUE)
  expect_match(txt, "Selected Variables \\(2\\):", perl = TRUE)
  expect_match(txt, "By Outcome:", perl = TRUE)
  expect_match(txt, "Selected Lambdas:", perl = TRUE)
  expect_match(txt, "Penalty Factors:", perl = TRUE)
  expect_match(txt, "Stored Models:\\s*2\\b", perl = TRUE)
  expect_match(txt, "Goodness-of-Fit:", perl = TRUE)
  expect_match(txt, "Interaction Metadata:", perl = TRUE)

  # ---- Selected variables line (allow arbitrary interior space) ----
  expect_match(txt, "\\bx1,\\s*x2\\b", perl = TRUE)

  # ---- By outcome listing (with and without variables) ----
  expect_match(txt, "\\-\\s*y_cont\\s*:\\s*1\\s+variables", perl = TRUE)
  expect_match(txt, "\\n\\s{2,}x1\\s", perl = TRUE) # an indented "x1" line
  expect_match(txt, "\\-\\s*y_bin\\s*:\\s*0\\s+variables", perl = TRUE)

  # ---- Lambdas (printer may round down/up around 0.012345) ----
  expect_match(txt, "\\-\\s*y_cont\\s*:\\s*0\\.0123[45]\\b", perl = TRUE)
  expect_match(txt, "\\-\\s*y_bin\\s*:\\s*0\\.1\\b", perl = TRUE)

  # ---- Penalty factor counts ----
  expect_match(txt, "Zero-penalty \\(must-keep\\):\\s*2\\b", perl = TRUE)
  expect_match(txt, "Regular-penalty:\\s*2\\b", perl = TRUE)

  # ---- Continuous outcome metrics ----
  expect_match(txt, "Outcome:\\s*y_cont\\b", perl = TRUE)
  expect_match(txt, "cv_error:\\s+0\\.1235", perl = TRUE)
  expect_match(txt, "cv_error_sd:\\s+0\\.0099", perl = TRUE)
  expect_match(txt, "r_squared:\\s+0\\.8765", perl = TRUE)
  expect_match(txt, "mse:\\s+1\\.2345", perl = TRUE)
  expect_match(txt, "rmse:\\s+1\\.1111", perl = TRUE)
  expect_match(txt, "mae:\\s+0\\.9876", perl = TRUE)

  # ---- Coefficient table: ordered by |coef| (x2 first, then x1) ----
  expect_match(txt, "Coefficients at Lambda Min:", perl = TRUE)
  expect_lt(
    regexpr("x2\\s+\\-1\\.2000", txt, perl = TRUE),
    regexpr("x1\\s+0\\.5000", txt, perl = TRUE)
  )

  # ---- Binary outcome metrics ----
  expect_match(txt, "Outcome:\\s*y_bin\\b", perl = TRUE)
  expect_match(txt, "deviance_explained:\\s+0\\.4321", perl = TRUE)
  expect_match(txt, "auc:\\s+0\\.9012", perl = TRUE)
  expect_match(txt, "accuracy:\\s+0\\.8123", perl = TRUE)
  expect_match(txt, "brier_score:\\s+0\\.1234", perl = TRUE)
  expect_match(txt, "No matching coefficients found\\.", perl = TRUE)

  # ---- Interaction metadata ----
  expect_match(txt, "Interaction terms selected \\(2\\):", perl = TRUE)
  expect_match(txt, "\\bx1:x2,\\s*x2:x3\\b", perl = TRUE)
  expect_match(txt, "Main effects present in selected interactions:\\s*x1,\\s*x2,\\s*x3", perl = TRUE)
  expect_match(txt, "Full formula tested: y \\~ \\(x1 \\+ x2 \\+ x3\\)\\^2", perl = TRUE)
})

test_that("prints 'None selected' and minimal branches when mostly empty", {
  testthat::skip_if_not_installed("crayon")
  withr::local_options(crayon.enabled = FALSE)

  x <- structure(list(
    selected_variables = character(0), # triggers "None selected"
    by_outcome = list(only_outcome = character(0)),
    selected_lambdas = list(only_outcome = 0.0054321),
    penalty_factors = numeric(0),
    models = list(),
    goodness_of_fit = list(
      only_outcome = list(
        cross_validated = list(cv_error = 0.5, cv_error_sd = 0.25),
        full_data = list(
          deviance_explained = 0.1,
          auc = 0.6,
          accuracy = 0.55,
          brier_score = 0.2,
          raw_coefs = NULL
        )
      )
    ),
    interaction_metadata = list(
      interaction_terms = character(0),
      main_effects_in_interactions = character(0),
      full_formula = "~ 1"
    )
  ), class = "select_auxiliary_variables_lasso_cv")

  txt <- paste(capture.output(print(x)), collapse = "\n")

  expect_match(txt, "Selected Variables \\(0\\):", perl = TRUE)
  expect_match(txt, "\\bNone selected\\b", perl = TRUE)

  expect_match(txt, "\\-\\s*only_outcome\\s*:\\s*0\\s+variables", perl = TRUE)
  expect_match(txt, "\\-\\s*only_outcome\\s*:\\s*0\\.005432", perl = TRUE)

  expect_match(txt, "Zero-penalty \\(must-keep\\):\\s*0\\b", perl = TRUE)
  expect_match(txt, "Regular-penalty:\\s*0\\b", perl = TRUE)
  expect_match(txt, "Stored Models:\\s*0\\b", perl = TRUE)

  expect_match(txt, "Outcome:\\s*only_outcome\\b", perl = TRUE)
  expect_match(txt, "cv_error:\\s+0\\.5\\b", perl = TRUE)
  expect_match(txt, "cv_error_sd:\\s+0\\.25\\b", perl = TRUE)
  expect_match(txt, "auc:\\s+0\\.6\\b", perl = TRUE)
  expect_match(txt, "No matching coefficients found\\.", perl = TRUE)

  expect_match(txt, "\\bNo interaction terms\\b", perl = TRUE)
  expect_match(txt, "Full formula tested: ~ 1", perl = TRUE)
})
