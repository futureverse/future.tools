#' Evaluate an R expression while collecting journals from completed futures
#' 
#' @param expr The R expression to evaluate
#'
#' @param substitute If TRUE, then `expr` is subtituted, otherwise not.
#'
#' @param envir The environment where `expr` should be evaluated
#'
#' @return
#' A list of FutureJournal:s. 
#'
#' @example incl/gather_journals.R
#'
#' @export
gather_journals <- function(expr, substitute = TRUE, envir = parent.frame()) {
  oopts <- options(future.journal = TRUE)
  on.exit(options(oopts))
  
  journals <- NULL
  withCallingHandlers({
    eval(expr, envir = envir)
  }, FutureJournalCondition = function(cond) {
    journals <<- c(journals, list(cond$journal))
  })
  
  journals
}
