library(testthat)



test_that("message_verbose prints message when verbose=TRUE", {
  expect_message(message_verbose(TRUE, "This is a test message"), "This is a test message")
})

test_that("message_verbose does NOT print message when verbose=FALSE", {
  expect_silent(message_verbose(FALSE, "This message should not appear"))
})
