#' Create a Future Journal Plot
#'
#' @param x A list of [future::Future] or FutureJournal objects.
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
#' @example incl/ggjournal,future.apply.R
#'
#' @import dplyr
#' @import ggplot2
#' @export
ggjournal <- function(x, baseline = TRUE, ...) {
  ## To please R CMD check
  at <- duration <- end <- index <- event <- NULL

  if (inherits(x, "FutureJournal") || inherits(x, "data.frame")) {
    js <- x
  } else {
    js <- journal(x, baseline = baseline)
  }
  js <- mutate(js, start = as.numeric(at), end = as.numeric(at + duration))

  gg <- ggplot()

  ## Lifespans
  js <- group_by(js, index)
  start <- filter(js, event == "create")[, c("index", "start")]
  stop  <- filter(js, event %in% c("launch", "gather"))[, c("index", "end")]
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
    fill = event
  ))

  gg <- gg + scale_y_reverse()
  gg <- gg + xlab("Time (seconds)") + ylab("future")
  gg <- gg + labs(fill = "Event")

  ## Fix the colors
  known_events <- c("lifespan", "create", "launch", "resolved", "gather", "evaluate")
  extra_events <- setdiff(levels(js$event), known_events)
#  if (length(extra_events) <= 6L) {
#    extra_events <- c(extra_events, rep(NA_character_, times = 6L - length(extra_events)))
#  } else if (length(extra_events) > 6L) {
#    stopf("Only supports at most six extra 'events': %s",
#                 paste(sQuote(extra_events), collapse = ", "))
#  }
  events <- c(known_events, extra_events)
  cols <- journal_palette(along = events)
  names(cols) <- events

  gg <- gg + scale_fill_manual(values = cols)

  gg
}


## FIXME: palette.colors requires R (>= 4.0.0)
#' @importFrom grDevices palette.colors
journal_palette <- function(n = NULL, along = NULL) {
  if (!is.null(n)) {
    stopifnot(
      is.null(along),
      is.numeric(n), length(n) == 1L, is.finite(n), n >= 0L
    )
  } else if (!is.null(along)) {
    stopifnot(is.null(n))
    n <- length(along)
  }
  
  cols <- suppressWarnings(palette.colors(n, palette = "Paired"))
  if (length(cols) < n) cols <- rep(cols, length.out = n)
  cols
}
