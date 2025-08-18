# tests/testthat/helper-setup.R

# Deterministic console coloring for print tests
options(crayon.enabled = FALSE)

# Lightweight utilities used across tests (do NOT shadow package internals)
.make_design <- function(df) survey::svydesign(ids = ~1, data = df, weights = ~weight)
.make_simple_design <- function(df) list(variables = df)
.msg_stub <- function(verbose, ...) if (isTRUE(verbose)) message(...)

# Lazily load {survey} api datasets when available
if (requireNamespace("survey", quietly = TRUE)) {
  data("api", package = "survey", envir = environment())
}
