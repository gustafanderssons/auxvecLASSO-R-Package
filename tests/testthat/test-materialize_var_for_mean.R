test_that("returns list with design and new var name", {
  d <- .make_simple_design(data.frame(a = 1:3))
  res <- .materialize_var_for_mean(d, "a")
  expect_type(res, "list")
  expect_true("design" %in% names(res))
  expect_true("var" %in% names(res))
  expect_true(is.data.frame(res$design$variables))
  expect_match(res$var, "^\\.av_ind__a$")
})

test_that("logical: TRUE/FALSE become 1/0 and NA preserved", {
  d <- .make_simple_design(data.frame(flag = c(TRUE, FALSE, NA)))
  res <- .materialize_var_for_mean(d, "flag")
  v <- res$design$variables[[res$var]]
  expect_type(v, "double")
  expect_equal(v, c(1, 0, NA_real_))
})

test_that("numeric 0/1: equals indicator (x == 1)", {
  d <- .make_simple_design(data.frame(bin = c(0, 1, 1, 0, NA)))
  res <- .materialize_var_for_mean(d, "bin")
  v <- res$design$variables[[res$var]]
  expect_equal(v, c(0, 1, 1, 0, NA_real_))
})

test_that("numeric non-binary: passed through unchanged", {
  y <- c(10, 20, NA)
  d <- .make_simple_design(data.frame(y = y))
  res <- .materialize_var_for_mean(d, "y")
  v <- res$design$variables[[res$var]]
  expect_equal(v, y)
})

test_that("factor 2-level (No/Yes): indicator for 'Yes'", {
  z <- factor(c("No", "Yes", "Yes", NA), levels = c("No", "Yes"))
  d <- .make_simple_design(data.frame(z = z))
  res <- .materialize_var_for_mean(d, "z")
  v <- res$design$variables[[res$var]]
  expect_equal(v, c(0, 1, 1, NA_real_))
})

test_that("character 2-level ('0'/'1'): indicator for '1'", {
  ch <- c("0", "1", "1", NA)
  d <- .make_simple_design(data.frame(ch = ch, stringsAsFactors = FALSE))
  res <- .materialize_var_for_mean(d, "ch")
  v <- res$design$variables[[res$var]]
  expect_equal(v, c(0, 1, 1, NA_real_))
})

test_that("factor 3+ levels: indicator for last level (fallback)", {
  f3 <- factor(c("A", "B", "C", NA), levels = c("A", "B", "C"))
  d <- .make_simple_design(data.frame(f3 = f3))
  res <- .materialize_var_for_mean(d, "f3")
  v <- res$design$variables[[res$var]]
  expect_equal(v, c(0, 0, 1, NA_real_))
})

test_that("character 3+ levels: indicator for last sorted level (fallback)", {
  # levels (sorted) are "a","b","c" -> positive = "c"
  ch3 <- c("a", "b", "c", NA)
  d <- .make_simple_design(data.frame(ch3 = ch3, stringsAsFactors = FALSE))
  res <- .materialize_var_for_mean(d, "ch3")
  v <- res$design$variables[[res$var]]
  expect_equal(v, c(0, 0, 1, NA_real_))
})

test_that("does not mutate the input design object", {
  d <- .make_simple_design(data.frame(x = c(0, 1)))
  d_copy <- d
  res <- .materialize_var_for_mean(d, "x")
  # original 'd' should not have the new column
  expect_false(".av_ind__x" %in% names(d$variables))
  # but result design should
  expect_true(".av_ind__x" %in% names(res$design$variables))
})

test_that("new column is numeric and has expected length", {
  d <- .make_simple_design(data.frame(w = c(TRUE, FALSE, TRUE, NA)))
  res <- .materialize_var_for_mean(d, "w")
  v <- res$design$variables[[res$var]]
  expect_type(v, "double")
  expect_length(v, 4)
})

test_that("works with integer 0/1 vectors as well", {
  d <- .make_simple_design(data.frame(ii = c(0L, 1L, 0L, 1L, NA)))
  res <- .materialize_var_for_mean(d, "ii")
  v <- res$design$variables[[res$var]]
  expect_equal(v, c(0, 1, 0, 1, NA_real_))
})
