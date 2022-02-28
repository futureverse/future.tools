#' Create a Future Journal Plot
#'
#' @param futures A list of [future::Future] objects.
#'
#' @param baseline (optional) A POSIXct timestamp to server as time zero
#' for the relative timestamps.
#'
#' @param \ldots Currently not used.
#'
#' @return
#' A [ggplot2::ggplot] object.
#'
#' @example incl/ggjournal.R
#'
#' @import ggplot2
#' @export
ggjournal <- function(futures, baseline = NULL, ...) {
  journal <- import_future("journal")

  ## To please R CMD check
  at <- duration <- index <- step <- NULL

  js <- journal(futures, baseline = baseline)

  gg <- ggplot(js, aes(
    x = as.numeric(at + duration / 2),
    y = index,
    width = as.numeric(duration),
    height = 0.8
  ))
  gg <- gg + geom_tile(aes(fill = step))
  gg <- gg + scale_y_reverse() + xlab("Time (seconds)") + ylab("future")
  gg
}
