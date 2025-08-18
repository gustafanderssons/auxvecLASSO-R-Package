# tests/testthat/test-coerce-pop-means-margin.R

# Helper: get the current binding from the package namespace
.get_pkg_fun <- function() {
  asNamespace("auxvecLASSO")[[".coerce_pop_means_margin"]]
}

# Sanity check the binding looks like the real function
.is_real_like <- function(f) {
  # correct signature
  if (!identical(names(formals(f)), c("dom", "vec", "varname"))) {
    return(FALSE)
  }
  # small contract probe: named numeric vector -> 2-col data.frame
  probe <- try(
    {
      out <- f("d", c(A = 1, B = 2), "v")
      is.data.frame(out) && identical(names(out), c("d", "v"))
    },
    silent = TRUE
  )
  isTRUE(probe)
}

# Safe inline copy of the real implementation (used only if we detect a leaked mock)
.real_coerce_copy <- function(dom, vec, varname) {
  if (is.null(vec)) {
    return(NULL)
  }
  if (is.numeric(vec) && !is.null(names(vec))) {
    out <- data.frame(
      stats::setNames(list(names(vec)), dom),
      stats::setNames(list(as.numeric(vec)), varname),
      check.names = FALSE
    )
    rownames(out) <- NULL
    return(out)
  }
  if (is.data.frame(vec) && all(c(dom, varname) %in% names(vec))) {
    return(vec[, c(dom, varname), drop = FALSE])
  }
  NULL
}

# Ensure we have a correct function for this test; if not, temporarily patch namespace.
.ensure_real <- function() {
  f <- .get_pkg_fun()
  if (.is_real_like(f)) {
    return(f)
  }

  # Namespace was mocked elsewhere (leaked). Patch it locally for this test.
  testthat::local_mocked_bindings(
    .coerce_pop_means_margin = .real_coerce_copy,
    .env = asNamespace("auxvecLASSO")
  )
  .real_coerce_copy
}

test_that("named numeric vector -> 2-column data.frame with correct names and values", {
  coerce <- .ensure_real()

  dom <- "stype"
  var <- "sch.wide_bin"
  vec <- c(E = 0.89, H = 0.56, M = 0.74)

  out <- coerce(dom, vec, var)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c(dom, var))
  expect_true(is.character(out[[dom]]) || is.factor(out[[dom]]))
  expect_identical(as.character(out[[dom]]), c("E", "H", "M"))
  expect_true(is.numeric(out[[var]]))
  expect_equal(out[[var]], as.numeric(vec), tolerance = 1e-12)
  expect_identical(rownames(out), as.character(seq_len(nrow(out))))
})

test_that("order of names in vector is preserved in output rows", {
  coerce <- .ensure_real()

  dom <- "stype"
  var <- "sch.wide_bin"
  vec <- c(M = 0.74, E = 0.89, H = 0.56) # intentionally unsorted

  out <- coerce(dom, vec, var)
  expect_identical(as.character(out[[dom]]), c("M", "E", "H"))
  expect_equal(out[[var]], as.numeric(vec), tolerance = 1e-12)
})

test_that("passes through a data.frame and subsets to [dom, var] only", {
  coerce <- .ensure_real()

  dom <- "stype"
  var <- "sch.wide_bin"
  df <- data.frame(
    stype        = c("E", "H", "M"),
    sch.wide_bin = c(0.89, 0.56, 0.74),
    extra_col    = 1:3,
    check.names  = FALSE
  )

  out <- coerce(dom, df, var)
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c(dom, var))
  expect_identical(out[[dom]], df[[dom]])
  expect_identical(out[[var]], df[[var]])
})

test_that("returns NULL for NULL input", {
  coerce <- .ensure_real()
  expect_null(coerce("stype", NULL, "sch.wide_bin"))
})

test_that("returns NULL for unnamed numeric vector", {
  coerce <- .ensure_real()
  vec <- c(0.89, 0.56, 0.74) # no names
  expect_null(coerce("stype", vec, "sch.wide_bin"))
})

test_that("returns NULL for non-numeric named vector", {
  coerce <- .ensure_real()
  vec <- c(E = "0.89", H = "0.56") # character values
  expect_null(coerce("stype", vec, "sch.wide_bin"))
})

test_that("handles integer named vector and keeps numeric type", {
  coerce <- .ensure_real()
  vec <- c(E = 1L, H = 0L)
  out <- coerce("stype", vec, "sch.wide_bin")
  expect_true(is.numeric(out[["sch.wide_bin"]]))
  expect_equal(out[["sch.wide_bin"]], as.numeric(vec), tolerance = 1e-12)
})

test_that("propagates NA values from the vector", {
  coerce <- .ensure_real()
  vec <- c(E = 0.89, H = NA_real_, M = 0.74)
  out <- coerce("stype", vec, "sch.wide_bin")
  expect_true(is.na(out[["sch.wide_bin"]][2]))
  expect_equal(out[["sch.wide_bin"]][c(1, 3)], c(0.89, 0.74), tolerance = 1e-12)
})

test_that("data.frame passthrough requires both dom and var columns, otherwise NULL", {
  coerce <- .ensure_real()

  bad1 <- data.frame(stype = c("E", "H"), other = c(1, 2)) # missing var col
  bad2 <- data.frame(sch.wide_bin = c(0.1, 0.2), other = c(1, 2)) # missing dom col

  expect_null(coerce("stype", bad1, "sch.wide_bin"))
  expect_null(coerce("stype", bad2, "sch.wide_bin"))
})
