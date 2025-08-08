#' Conditional verbose messaging
#'
#' Prints a message only if `verbose` is `TRUE`.
#'
#' @param verbose Logical; whether to print messages.
#' @param msg Character; the message to print.
#'
#' @keywords internal
#' @noRd
message_verbose <- function(verbose, msg) {
  if (isTRUE(verbose)) message(msg)
}
