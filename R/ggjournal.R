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
  jj <- at <- duration <- index <- step <- NULL

  js <- lapply(futures, FUN = journal, baseline = baseline)

  ## Prepend index
  js <- lapply(seq_along(js), FUN = function(ii) cbind(index = ii, js[[ii]]))

  ## Stack
  js <- Reduce(rbind, js)

  gg <- ggplot(js, aes(x = at + duration / 2, y = index, width = duration, height = 0.8))
  gg <- gg + geom_tile(aes(fill = step))
  gg <- gg + scale_y_reverse() + xlab("Time (seconds)") + ylab("future")
  gg
}
