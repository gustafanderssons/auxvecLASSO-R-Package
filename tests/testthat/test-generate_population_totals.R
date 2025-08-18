test_that("basic structure and return types are correct", {
  df <- data.frame(
    f = factor(c("A", "B", "A", "B"), levels = c("A", "B", "C")),
    g = factor(c("X", "X", "Y", "Y")),
    z = c(1, 2, 3, 4)
  )
  cal_formula <- ~ f + g + f:g

  res <- generate_population_totals(
    population_df       = df,
    calibration_formula = cal_formula
  )

  expect_s3_class(res, "calib_totals")
  expect_type(res$population_totals, "double")
  expect_true(is.list(res$levels))
  expect_true(inherits(res$terms, "terms"))
  expect_true(!is.null(names(res$population_totals)))
  expect_true("(Intercept)" %in% names(res$population_totals))
})

test_that("intercept sums to N (unweighted) and to sum(weights) when provided", {
  set.seed(1)
  df <- data.frame(
    f = factor(sample(c("A", "B"), 50, replace = TRUE)),
    g = factor(sample(c("X", "Y"), 50, replace = TRUE))
  )
  w <- runif(nrow(df), 0.5, 2)
  cal_formula <- ~ f + g + f:g

  unwt <- generate_population_totals(df, cal_formula, include_intercept = TRUE)
  wt <- generate_population_totals(df, cal_formula, weights = w, include_intercept = TRUE)

  expect_equal(unname(unwt$population_totals["(Intercept)"]), nrow(df))
  expect_equal(unname(wt$population_totals["(Intercept)"]), sum(w))
})

test_that("include_intercept = FALSE removes the intercept column", {
  df <- data.frame(
    f = factor(c("A", "B", "A", "B")),
    g = factor(c("X", "X", "Y", "Y"))
  )
  cal_formula <- ~ f + g + f:g

  noint <- generate_population_totals(df, cal_formula, include_intercept = FALSE)
  expect_false("(Intercept)" %in% names(noint$population_totals))
})

test_that("contrasts argument is honored (column names and contrasts captured)", {
  df <- data.frame(f = factor(c("A", "B", "C", "A")), g = factor(c("X", "Y", "X", "Y")))
  cal_formula <- ~ f + g

  def <- generate_population_totals(df, cal_formula)
  sumc <- generate_population_totals(
    df, cal_formula,
    contrasts = list(f = contr.sum)
  )

  expect_false(identical(names(def$population_totals), names(sumc$population_totals)))
  expect_true("f" %in% names(def$contrasts))
  expect_true("f" %in% names(sumc$contrasts))
})

test_that("sparse path produces identical totals", {
  testthat::skip_if_not_installed("Matrix")

  set.seed(123)
  df <- data.frame(
    f = factor(sample(LETTERS[1:5], 200, replace = TRUE)),
    g = factor(sample(letters[1:4], 200, replace = TRUE))
  )
  cal_formula <- ~ f + g + f:g

  dense <- generate_population_totals(df, cal_formula, sparse = FALSE)
  sprs <- generate_population_totals(df, cal_formula, sparse = TRUE)

  expect_identical(names(dense$population_totals), names(sprs$population_totals))
  expect_equal(dense$population_totals, sprs$population_totals, tolerance = 1e-10)
})

test_that("zero-total columns are detected and optionally dropped", {
  df <- data.frame(
    f = factor(c("A", "A", "B", "B"), levels = c("A", "B", "C")),
    g = factor(c("X", "Y", "X", "Y"))
  )
  cal_formula <- ~ f + g

  expect_message(
    kept <- generate_population_totals(df, cal_formula, drop_zero_cols = FALSE),
    regexp = "zero-total column"
  )
  expect_true(any(kept$population_totals == 0))
  zero_cols <- names(kept$population_totals)[kept$population_totals == 0]
  expect_true(any(grepl("^f", zero_cols)))

  expect_message(
    dropped <- generate_population_totals(df, cal_formula, drop_zero_cols = TRUE),
    regexp = "Dropping .* zero-total column"
  )
  expect_false(any(dropped$population_totals == 0))
})

test_that("weights length validation triggers an error", {
  df <- data.frame(
    f = factor(c("A", "B", "A", "B")),
    g = factor(c("X", "X", "Y", "Y"))
  )
  cal_formula <- ~ f + g

  expect_error(
    generate_population_totals(df, cal_formula, weights = c(1, 2, 3)),
    regexp = "weights.*length"
  )
})

test_that("na_action controls how missing values affect totals", {
  df <- data.frame(
    f = factor(c("A", "B", "A", NA)),
    g = factor(c("X", "X", "Y", "Y"))
  )
  cal_formula <- ~ f + g

  # With na.omit: last row dropped; intercept should be 3
  omit <- generate_population_totals(df, cal_formula, na_action = stats::na.omit)
  expect_equal(unname(omit$population_totals["(Intercept)"]), 3)

  # With na.pass: NA rows propagate; some totals become NA
  pass <- generate_population_totals(df, cal_formula, na_action = stats::na.pass)
  expect_true(any(is.na(pass$population_totals)))
})

test_that("terms can be reused to force alignment on new data (even with missing levels)", {
  set.seed(42)
  pop <- data.frame(
    f = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    g = factor(sample(c("X", "Y"), 100, replace = TRUE))
  )
  cal_formula <- ~ f + g + f:g

  gp <- generate_population_totals(pop, cal_formula)

  resp <- data.frame(
    f = factor(sample(c("A", "B"), 30, replace = TRUE), levels = levels(pop$f)),
    g = factor(sample(c("X", "Y"), 30, replace = TRUE), levels = levels(pop$g))
  )

  X_resp <- model.matrix(gp$terms, data = resp)
  expect_identical(colnames(X_resp), names(gp$population_totals))
})

test_that("output names are stable and match model.matrix built on population terms", {
  df <- data.frame(
    f = factor(c("A", "B", "C", "A")),
    g = factor(c("X", "Y", "X", "Y"))
  )
  cal_formula <- ~ f + g + f:g

  gp <- generate_population_totals(df, cal_formula)

  X_pop2 <- model.matrix(gp$terms, data = df)
  expect_identical(colnames(X_pop2), names(gp$population_totals))
})

test_that("works with survey::api data when available (no env changes required)", {
  testthat::skip_if_not_installed("survey")

  utils::data("api", package = "survey") # loads apipop into this env
  pop <- apipop

  # Helper: safely numeric-ize possibly-factor numerics without emitting warnings.
  numify <- function(x) {
    if (is.numeric(x)) {
      return(x)
    }
    if (is.integer(x)) {
      return(as.numeric(x))
    }
    if (is.factor(x) || is.character(x)) {
      y <- suppressWarnings(as.numeric(as.character(x)))
      # If everything NA (labels non-numeric), fall back to integer codes
      if (all(is.na(y))) y <- as.numeric(x)
      return(y)
    }
    as.numeric(x)
  }

  # Coerce potential factor-numerics (suppress warnings and impute any rare NA with median)
  to_num <- function(v) {
    out <- numify(v)
    if (anyNA(out)) {
      # If all NA, just set to 0 to avoid binning errors; otherwise median-impute.
      if (all(is.na(out))) {
        out[] <- 0
      } else {
        out[is.na(out)] <- stats::median(out, na.rm = TRUE)
      }
    }
    out
  }
  pop$api00 <- to_num(pop$api00)
  pop$growth <- to_num(pop$growth)
  pop$ell <- to_num(pop$ell)
  pop$comp.imp <- to_num(pop$comp.imp)
  pop$hsg <- to_num(pop$hsg)

  # Create bins with explicit two-level factors (ensures valid contrasts even if one level absent)
  pop$api00_bin <- factor(ifelse(pop$api00 >= 700, "700plus", "lt700"),
    levels = c("lt700", "700plus")
  )
  pop$growth_bin <- factor(ifelse(pop$growth >= 0, "nonneg", "neg"),
    levels = c("neg", "nonneg")
  )
  pop$ell_bin <- factor(ifelse(pop$ell >= 10, "highELL", "lowELL"),
    levels = c("lowELL", "highELL")
  )
  pop$comp.imp_bin <- factor(ifelse(pop$comp.imp >= 50, "highComp", "lowComp"),
    levels = c("lowComp", "highComp")
  )
  pop$hsg_bin <- factor(ifelse(pop$hsg >= 60, "highHSG", "lowHSG"),
    levels = c("lowHSG", "highHSG")
  )

  # stype in apipop is already a factor
  cal_formula <- ~ stype + growth_bin + api00_bin + ell_bin + comp.imp_bin + hsg_bin +
    api00_bin:stype + hsg_bin:stype + comp.imp_bin:stype + api00_bin:growth_bin

  gp <- generate_population_totals(pop, cal_formula, include_intercept = TRUE)

  expect_equal(unname(gp$population_totals["(Intercept)"]), nrow(pop))
  expect_true(any(grepl("^stype", names(gp$population_totals))))

  # Interaction column names include level suffixes (e.g., api00_bin700plus:stypeH).
  # Check for any column containing both variables around a colon.
  has_api00_stype_inter <-
    any(grepl("api00_bin[^:]*:stype|stype[^:]*:api00_bin", names(gp$population_totals)))
  expect_true(has_api00_stype_inter)
})
