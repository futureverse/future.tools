#' The exported version of \pkg{future}'s private `journal()`
#'
#' @param \ldots Arguments passed to `future:::journal()`.
#'
#' @return
#' Returns a data.frame of class `FutureJournal`.
#'
#' @details
#' The `journal()` function of the \pkg{future} package is not exported
#' at the time being because it undergoes lots of development.  In the
#' meanwhile, one can use the version provided here, but please note that
#' the arguments and the format of the returned value is likely to change.
#' Please do not depend on this in other packages or production code.
#' 
#' @export
journal <- function(...) {
  .journal <- import_future("journal")
  .journal(...)
}
