% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggjournal.R
\name{ggjournal}
\alias{ggjournal}
\title{Create a Future Journal Plot}
\usage{
ggjournal(
  x,
  style = c("future", "future-worker", "worker"),
  flatten = FALSE,
  time_range = getOption("future.tools.ggjournal.time_range", NULL),
  item_range = getOption("future.tools.ggjournal.item_range", NULL),
  events = NULL,
  baseline = TRUE,
  label_fmt = "#\%s",
  annotate = c("future_label"),
  arrows = c("launch", "gather"),
  layer_height = c(1/4, 1/4, 1/4, 1/8),
  ...
)
}
\arguments{
\item{x}{A list of \link[future:Future-class]{future::Future} or FutureJournal objects.}

\item{style}{(character string) One of \code{"future"}, \code{"future-worker"},
and \code{"worker"}.}

\item{flatten}{(logical) If TRUE, futures are not separated vertically.}

\item{time_range}{(optional vector of length two) The range of time
to displayed.}

\item{item_range}{(optional vector of length two) The range of future
or worker indices to displayed.}

\item{events}{(character vector; optional) Events to be displayed.
If NULL, then all events are displayed.}

\item{baseline}{(POSIXct; optional) A timestamp to server as time zero
for the relative timestamps. If \code{TRUE} (default), then the earliest
timepoint observed is used as the baseline.}

\item{label_fmt}{(format string; optional) Used to create labels if
\code{future_label} is missing. If NULL, no labels are created.}

\item{annotate}{(character vector) Additional annotations to add.}

\item{arrows}{(character vector) Type of arrows to draw.}

\item{layer_height}{(integer vector of length four) Height of each of
the four possible layers of stacked events. Their total height, the sum,
should be less than one in order for futures to not overlap.}

\item{\ldots}{Currently not used.}
}
\value{
A \link[ggplot2:ggplot]{ggplot2::ggplot} object.
}
\description{
Create a Future Journal Plot
}
\examples{
library(future.apply)
library(future)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

## Plot with fixed x and y limits
ggjournal_x <- function(js) {
  for (style in c("future", "worker")) {
    item_range <- if (style == "future") c(1, 5) else c(0, 1.8)
    print(ggjournal(js, style = style,
                    time_range = c(0, 2.0), item_range = item_range))
  }
}


plan(sequential)

js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
ggjournal_x(js)

js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
ggjournal_x(js)


plan(multisession, workers = 2)

js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
ggjournal_x(js)

js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
ggjournal_x(js)


## Shut down parallel workers
plan(sequential)
}
