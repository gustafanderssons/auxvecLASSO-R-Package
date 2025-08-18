test_that("prints full summary for mixed shapes and returns invisibly", {
  testthat::skip_if_not_installed("crayon")
  # Disable color so we can assert on clean text.
  withr::local_options(crayon.enabled = FALSE)

  # --- Input assembly covering many branches ----

  # weight variation
  weight_var <- list(
    cv_weights   = 0.123456,
    gini_weights = 0.234567
  )

  # Register diagnostics (TOTAL) — include optional rse and p_bias to exercise those lines
  reg_total <- list(
    age = data.frame(
      mean   = 1.23,
      se     = 0.11,
      rse    = 0.11 / 1.23,
      bias   = 0.03,
      mse    = 0.011,
      p_bias = 0.50
    )
  )

  # Register diagnostics (BY DOMAIN) — NEW shape
  reg_by_new <- list(
    region = list(
      income = data.frame(
        region = c("North", "South"),
        mean   = c(100, 120),
        se     = c(10, 12),
        rse    = c(0.10, 0.10),
        bias   = c(NA_real_, 5),
        mse    = c(NA_real_, 169),
        p_bias = c(NA_real_, 0.04),
        truth  = c(NA_real_, NA_real_)
      ),
      # "no domain column found" branch (metrics only)
      empty_metrics = data.frame(
        mean = 1, se = 2, rse = NA_real_, bias = NA_real_, mse = NA_real_, p_bias = NA_real_
      )
    )
  )

  # Survey diagnostics (TOTAL) — omit rse and p_bias to test optional columns absent
  survey_total <- list(
    height = data.frame(
      mean = 170, se = 5,
      rse = NA_real_, # present but NA (fmt should show "NA")
      bias = NA_real_, mse = NA_real_
      # p_bias not present at all
    )
  )

  # Survey diagnostics (BY DOMAIN) — OLD shape (list keyed by variable, df contains domain col)
  survey_by_old <- list(
    weight = data.frame(
      region = c("North", "South"),
      mean   = c(70, 80),
      se     = c(3, 4),
      # No "rse" column (optional branch)
      bias   = c(NA_real_, NA_real_),
      mse    = c(NA_real_, NA_real_)
      # No "p_bias" column (optional branch)
    ),
    # OLD shape with *no* domain column -> "(no domain column found)"
    nodomain = data.frame(
      mean = 1, se = 1, bias = NA_real_, mse = NA_real_
    )
  )

  x <- structure(list(
    weight_variation     = weight_var,
    register_diagnostics = list(total = reg_total, by_domain = reg_by_new),
    survey_diagnostics   = list(total = survey_total, by_domain = survey_by_old)
  ), class = "assess_aux_vector")

  # Capture output text and separately check invisibility of print(x)
  txt <- paste(capture.output(print(x)), collapse = "\n")
  expect_false(withVisible(print(x))$visible)
  expect_s3_class(x, "assess_aux_vector")

  # --- Headings
  expect_match(txt, "Auxiliary Vector Assessment Summary", perl = TRUE)
  expect_match(txt, "Weight Variation Metrics:", perl = TRUE)
  expect_match(txt, "Register Diagnostics \\(Total\\):", perl = TRUE)
  expect_match(txt, "Register Diagnostics \\(By Domain\\):", perl = TRUE)
  expect_match(txt, "Survey Diagnostics \\(Total\\):", perl = TRUE)
  expect_match(txt, "Survey Diagnostics \\(By Domain\\):", perl = TRUE)

  # --- Weight variation lines (and formatting)
  expect_match(txt, "cv_weights\\s+0\\.123456", perl = TRUE)
  expect_match(txt, "gini_weights\\s+0\\.234567", perl = TRUE)

  # --- TOTAL (register) with rse + p_bias
  expect_match(txt, "\\-\\s+age\\s+:", perl = TRUE)
  expect_match(txt, "Mean:\\s+\\s*1\\.230000", perl = TRUE)
  expect_match(txt, "SE:\\s+\\s*0\\.110000", perl = TRUE)
  expect_match(txt, "RSE:\\s+\\s*0\\.089431", perl = TRUE) # formatted value
  expect_match(txt, "Bias:\\s+\\s*0\\.030000", perl = TRUE)
  expect_match(txt, "MSE:\\s+\\s*0\\.011000", perl = TRUE)
  expect_match(txt, "p\\(Bias\\):\\s+\\s*0\\.500000", perl = TRUE)

  # --- BY DOMAIN (NEW shape): domain label lines + optional fields
  expect_match(txt, "\\*\\s+by\\s+region\\s*:", perl = TRUE) # allow variable spaces around words/colon
  expect_match(txt, "income\\s+:", perl = TRUE)
  expect_match(txt, "Domain:\\s+region=North", perl = TRUE)
  expect_match(txt, "Mean:\\s+\\s*100\\.000000", perl = TRUE)
  expect_match(txt, "SE:\\s+\\s*10\\.000000", perl = TRUE)
  expect_match(txt, "RSE:\\s+\\s*0\\.100000", perl = TRUE)
  # second row with p(Bias)
  expect_match(txt, "Domain:\\s+region=South", perl = TRUE)
  expect_match(txt, "p\\(Bias\\):\\s+\\s*0\\.040000", perl = TRUE)
  # NEW shape "no domain column found"
  expect_match(txt, "\\(no domain column found\\)", perl = TRUE)

  # --- TOTAL (survey) without optional columns; ensure NAs formatted
  expect_match(txt, "\\-\\s+height\\s+:", perl = TRUE)
  expect_match(txt, "Mean:\\s+\\s*170\\.000000", perl = TRUE)
  expect_match(txt, "SE:\\s+\\s*5\\.000000", perl = TRUE)
  expect_match(txt, "RSE:\\s+\\s*NA", perl = TRUE) # "        NA" collapsed capture

  # --- BY DOMAIN (OLD shape)
  expect_match(txt, "\\-\\s+weight\\s+:", perl = TRUE)
  expect_match(txt, "Domain:\\s+region=North", perl = TRUE)
  expect_match(txt, "Domain:\\s+region=South", perl = TRUE)
  # OLD shape 'no domain column found'
  expect_match(txt, "\\-\\s+nodomain\\s+:", perl = TRUE)
  expect_match(txt, "\\(no domain column found\\)", perl = TRUE)
})

test_that("prints '(none)' for empty or NULL sections", {
  testthat::skip_if_not_installed("crayon")
  withr::local_options(crayon.enabled = FALSE)

  x <- structure(list(
    weight_variation     = list(), # none
    register_diagnostics = list(total = list(), by_domain = NULL),
    survey_diagnostics   = list(total = list(), by_domain = NULL)
  ), class = "assess_aux_vector")

  txt <- paste(capture.output(print(x)), collapse = "\n")

  # Allow optional spaces before newline after the header labels
  expect_match(txt, "Register Diagnostics \\(Total\\):\\s*\\(none\\)", perl = TRUE)
  expect_match(txt, "Register Diagnostics \\(By Domain\\):\\s*\\(none\\)", perl = TRUE)
  expect_match(txt, "Survey Diagnostics \\(Total\\):\\s*\\(none\\)", perl = TRUE)
  expect_match(txt, "Survey Diagnostics \\(By Domain\\):\\s*\\(none\\)", perl = TRUE)
})

test_that("method returns the original object invisibly", {
  testthat::skip_if_not_installed("crayon")
  withr::local_options(crayon.enabled = FALSE)

  x <- structure(list(
    weight_variation     = list(cv = 0.1),
    register_diagnostics = list(total = list(), by_domain = NULL),
    survey_diagnostics   = list(total = list(), by_domain = NULL)
  ), class = "assess_aux_vector")

  res <- withVisible(print(x))
  expect_false(res$visible)
  expect_s3_class(x, "assess_aux_vector")
})
