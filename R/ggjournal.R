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
#' @importFrom future journal
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

  ## ------------------------------------------------------------------
  ## Create (event, legend) map
  ## ------------------------------------------------------------------
  general <- c("lifespan", "resolved")
  chrono <- c("create", "launch", "evaluate", "gather")
  events <- c(general, chrono)
  others <- setdiff(as.character(levels(js[["event"]])), events)
  events <- c(events, others)

  index <- as.character(match(events, table = chrono))
  index[is.na(index)] <- ""
  map <- data.frame(event = events, index = index)

  parents <- unique(js[["parent"]])
  parents <- parents[!is.na(parents)]
  for (parent in parents) {
    children <- subset(js, parent == parent)[["event"]]
    children <- as.character(unique(children))
    index_parent <- subset(map, event == parent)[["index"]]
    index_children <- paste(index_parent, seq_along(children), sep = ".")
    idxs <- match(map[["event"]], children)
    keep <- is.finite(idxs)
    map[["index"]][keep] <- index_children[idxs][keep]
  }

  ## Event labels
  labels <- tolower(gsub("([[:upper:]])", " \\1", events))
  labels[labels == "resolved"] <- "resolved?"
  map[["label"]] <- labels

  ## Labels with index prefix
  labels <- paste(map[["index"]], map[["label"]], sep = ". ")
  labels <- gsub("^[.] ", "", labels)
  map[["indexed_label"]] <- labels

  ## Order by index
  index <- map[["index"]]
  index[index == ""] <- 0
  o <- order(numeric_version(index))
  map <- map[o, ]


  ## ------------------------------------------------------------------
  ## Generate plot
  ## ------------------------------------------------------------------
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

  yoffset <- double(length = nrow(js))
  yoffset[js[["parent"]] == "launch"] <- 0.4
  yoffset[js[["event"]] == "resolved"] <- 0.4
  
  ## Events
  gg <- gg + geom_rect(data = js, aes(
    xmin = start, xmax = end,
    ymin = index + 0.0 - yoffset, ymax = index + 0.4 - yoffset,
    fill = event
  ))

  gg <- gg + scale_y_reverse()
  gg <- gg + xlab("Time (seconds)") + ylab("future")
  gg <- gg + labs(fill = "Event")

  ## Fixate colors
  
  cols <- journal_palette(along = map[["event"]])
  names(cols) <- map[["event"]]

  gg <- gg + scale_fill_manual(values = cols, labels = map[["indexed_label"]])

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
