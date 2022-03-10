stopf <- function(fmt, ..., call. = TRUE, domain = NULL) {  #nolint
  stop(sprintf(fmt, ...), call. = call., domain = domain)
}

warnf <- function(fmt, ..., call. = TRUE, immediate. = FALSE, domain = NULL) {  #nolint
  warning(sprintf(fmt, ...), call. = call., immediate. = immediate., domain = domain)
}

msgf <- function(fmt, ..., appendLF = FALSE, domain = NULL) {  #nolint
  message(sprintf(fmt, ...), appendLF = appendLF, domain = domain)
}

stop_if_not <- function(...) {
  res <- list(...)
  for (ii in 1L:length(res)) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "....")
        stopf("%s is not TRUE", sQuote(call), call. = FALSE, domain = NA)
    }
  }
  
  NULL
}

now <- function(x = Sys.time(), format = "[%H:%M:%OS3] ") {
  ## format(x, format = format) ## slower
  format(as.POSIXlt(x, tz = ""), format = format)
}

mdebug <- function(..., debug = NA) {
  if (is.na(debug)) debug <- getOption("future.tools.debug", FALSE)
  if (!debug) return()
  message(now(), ...)
}

mdebugf <- function(..., appendLF = TRUE, debug = NA) {
  if (is.na(debug)) debug <- getOption("future.tools.debug", FALSE)
  if (!debug) return()
  message(now(), sprintf(...), appendLF = appendLF)
}

#' @importFrom utils capture.output
mprint <- function(..., appendLF = TRUE, debug = NA) {
  if (is.na(debug)) debug <- getOption("future.tools.debug", FALSE)
  if (!debug) return()
  message(paste(now(), capture.output(print(...)), sep = "", collapse = "\n"), appendLF = appendLF)
}

#' @importFrom utils capture.output str
mstr <- function(..., appendLF = TRUE, debug = NA) {
  if (is.na(debug)) debug <- getOption("future.tools.debug", FALSE)
  if (!debug) return()
  message(paste(now(), capture.output(str(...)), sep = "", collapse = "\n"), appendLF = appendLF)
}
