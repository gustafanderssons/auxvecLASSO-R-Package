test_that("empty_result returns correctly structured list for non-empty outcomes", {
  outcomes <- c("y1", "y2")
  res <- empty_result(outcomes)

  # top-level keys
  expect_type(res, "list")
  expect_true(all(c(
    "selected_variables",
    "by_outcome",
    "selected_lambdas",
    "penalty_factors",
    "models",
    "goodness_of_fit",
    "interaction_metadata"
  ) %in% names(res)))

  # selected_variables
  expect_identical(res$selected_variables, character(0))

  # by_outcome: named list with one element per outcome; elements are NULL by design
  expect_type(res$by_outcome, "list")
  expect_length(res$by_outcome, length(outcomes))
  expect_identical(names(res$by_outcome), outcomes)
  lapply(res$by_outcome, function(x) expect_null(x))

  # selected_lambdas: named numeric vector of NA_real_
  expect_type(res$selected_lambdas, "double")
  expect_length(res$selected_lambdas, length(outcomes))
  expect_identical(names(res$selected_lambdas), outcomes)
  expect_true(all(is.na(res$selected_lambdas)))

  # penalty_factors: empty numeric
  expect_identical(res$penalty_factors, numeric(0))

  # models: empty list
  expect_type(res$models, "list")
  expect_length(res$models, 0)

  # goodness_of_fit: empty list (to be filled per outcome later)
  expect_type(res$goodness_of_fit, "list")
  expect_length(res$goodness_of_fit, 0)

  # interaction_metadata
  expect_type(res$interaction_metadata, "list")
  expect_true(all(c(
    "interaction_terms",
    "main_effects_in_interactions",
    "full_formula"
  ) %in% names(res$interaction_metadata)))

  expect_identical(res$interaction_metadata$interaction_terms, character(0))
  expect_identical(res$interaction_metadata$main_effects_in_interactions, character(0))
  expect_null(res$interaction_metadata$full_formula)
})

test_that("empty_result handles zero-length outcomes", {
  outcomes <- character(0)
  res <- empty_result(outcomes)

  # by_outcome and selected_lambdas should be length 0 with no names
  expect_identical(res$by_outcome, setNames(list(), character(0)))
  expect_identical(res$selected_lambdas, setNames(numeric(0), character(0)))

  # still returns the same fixed shapes for other fields
  expect_identical(res$selected_variables, character(0))
  expect_identical(res$penalty_factors, numeric(0))
  expect_type(res$models, "list"); expect_length(res$models, 0)
  expect_type(res$goodness_of_fit, "list"); expect_length(res$goodness_of_fit, 0)

  expect_identical(res$interaction_metadata$interaction_terms, character(0))
  expect_identical(res$interaction_metadata$main_effects_in_interactions, character(0))
  expect_null(res$interaction_metadata$full_formula)
})

test_that("empty_result preserves outcome order in names", {
  outcomes <- c("b", "a", "c")
  res <- empty_result(outcomes)
  expect_identical(names(res$by_outcome), outcomes)
  expect_identical(names(res$selected_lambdas), outcomes)
})
