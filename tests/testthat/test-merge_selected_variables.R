test_that("expands main effects for 2-way interactions when requested", {
  all_selected <- list(
    c("A", "B:C"),
    c("D:E")
  )
  colnames_X <- c("(Intercept)", "A", "B", "C", "D", "E", "Z")
  must_have_idx <- integer(0)

  out <- merge_selected_variables(
    all_selected, colnames_X, must_have_idx,
    check_twoway_int = TRUE
  )

  # Expect original selections plus main effects B, C, D, E (deduped, order preserved)
  expect_equal(out, c("A", "B:C", "D:E", "B", "C", "D", "E"))
})

test_that("does not expand interactions when check_twoway_int = FALSE", {
  all_selected <- list(c("A", "B:C"), c("D:E"))
  colnames_X <- c("(Intercept)", "A", "B", "C", "D", "E", "Z")
  out <- merge_selected_variables(all_selected, colnames_X, integer(0), check_twoway_int = FALSE)

  expect_equal(out, c("A", "B:C", "D:E"))
})

test_that("expands main effects for 3-way interactions by splitting all parts", {
  all_selected <- list(c("A:B:C"))
  colnames_X <- c("(Intercept)", "A", "B", "C")
  out <- merge_selected_variables(all_selected, colnames_X, integer(0), check_twoway_int = TRUE)

  expect_equal(out, c("A:B:C", "A", "B", "C"))
})

test_that("must-have variables are appended by index and deduped", {
  all_selected <- list(c("A", "B:C"))
  colnames_X <- c("(Intercept)", "A", "B", "C", "Z")
  must_have_idx <- c(1, 5) # "(Intercept)", "Z"

  out <- merge_selected_variables(all_selected, colnames_X, must_have_idx, check_twoway_int = FALSE)

  # A already selected; expect to include (Intercept) and Z without duplication
  expect_equal(out, c("A", "B:C", "(Intercept)", "Z"))
})

test_that("duplicates across outcomes are removed with first-seen order", {
  all_selected <- list(c("A", "B"), c("B", "A"))
  colnames_X <- c("(Intercept)", "A", "B")
  out <- merge_selected_variables(all_selected, colnames_X, integer(0), check_twoway_int = FALSE)

  expect_equal(out, c("A", "B"))
})

test_that("no selected variables returns only must-have variables", {
  all_selected <- list()
  colnames_X <- c("(Intercept)", "A", "B", "C")
  must_have_idx <- c(1, 3) # (Intercept), B

  out <- merge_selected_variables(all_selected, colnames_X, must_have_idx, check_twoway_int = TRUE)

  expect_equal(out, c("(Intercept)", "B"))
})

test_that("selected variables not in colnames_X are kept (function does not filter to X)", {
  all_selected <- list(c("W", "A:B"))
  colnames_X <- c("(Intercept)", "A", "B")
  out <- merge_selected_variables(all_selected, colnames_X, integer(0), check_twoway_int = TRUE)

  # W is retained; A and B added as main effects
  expect_equal(out, c("W", "A:B", "A", "B"))
})

test_that("empty must_have_idx has no effect", {
  all_selected <- list(c("A"))
  colnames_X <- c("(Intercept)", "A", "B")
  out <- merge_selected_variables(all_selected, colnames_X, integer(0), check_twoway_int = FALSE)

  expect_equal(out, c("A"))
})

test_that("order remains stable when adding main effects and must-haves", {
  all_selected <- list(c("X:Y", "M"))
  colnames_X <- c("(Intercept)", "M", "X", "Y", "Z")
  must_have_idx <- c(5, 1) # Z, (Intercept)

  out <- merge_selected_variables(all_selected, colnames_X, must_have_idx, check_twoway_int = TRUE)

  # order: combined (X:Y, M) -> add main effects (X, Y) -> add must-haves (Z, (Intercept)) -> unique
  expect_equal(out, c("X:Y", "M", "X", "Y", "Z", "(Intercept)"))
})
