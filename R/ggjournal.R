#' Create a Future Journal Plot
#'
#' @param x A list of [future::Future] or [future::FutureJournal] objects.
#'
#' @param baseline (POSIXct; optional) A timestamp to server as time zero
#' for the relative timestamps. If `TRUE` (default), then the earliest
#' timepoint observed is used as the baseline.
#'
#' @param \ldots Currently not used.
#'
#' @return
#' A [ggplot2::ggplot] object.
#'
#' @example incl/ggjournal.R
#'
#' @import dplyr
#' @import ggplot2
#' @export
ggjournal <- function(x, baseline = TRUE, ...) {
  journal <- import_future("journal")

  ## To please R CMD check
  at <- duration <- end <- index <- step <- NULL

  if (inherits(x, "FutureJournal") || inherits(x, "data.frame")) {
    js <- x
  } else {
    x <- journal(x, baseline = baseline)
  }
  js <- mutate(js, start = as.numeric(at), end = as.numeric(at + duration))

  gg <- ggplot()

  ## Lifespans
  js <- group_by(js, index)
  start <- filter(js, step == "create")[, c("index", "start")]
  stop  <- filter(js, step %in% c("launch", "gather"))[, c("index", "end")]
  stop  <- top_n(stop, n = 1L, wt = "end")
  lifespan <- full_join(start, stop, by = "index")
  gg <- gg + geom_rect(data = lifespan, aes(
    xmin = start, xmax = end,
    ymin = index + 0.4, ymax = index + 0.5,
    fill = "lifespan"
  ))

  ## Events
  gg <- gg + geom_rect(data = js, aes(
    xmin = start, xmax = end,
    ymin = index - 0.4, ymax = index + 0.4,
    fill = step
  ))
  
  gg <- gg + scale_y_reverse()
  gg <- gg + xlab("Time (seconds)") + ylab("future")
  gg <- gg + labs(fill = "Event")
  
  gg
}
