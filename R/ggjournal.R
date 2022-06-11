#' Create a Future Journal Plot
#'
#' @param x A list of [future::Future] or FutureJournal objects.
#'
#' @param baseline (POSIXct; optional) A timestamp to server as time zero
#' for the relative timestamps. If `TRUE` (default), then the earliest
#' timepoint observed is used as the baseline.
#'
#' @param time_range (optional vector of length two) The range of time
#' to displayed.
#'
#' @param future_range (optional vector of length two) The range of future
#' indices to displayed.
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
ggjournal <- function(x, baseline = TRUE, time_range = NULL, future_range = NULL, ...) {
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

  height <- c(2, 1, 2, 1)
  height <- 0.8 * height / sum(height)
  yoffset <- c(0.0, cumsum(height)[-length(height)])
  yoffset <- yoffset - height[1]

  layer <- rep(3L, times = nrow(js))
  layer[js[["parent"]] == "launch"  ] <- 4L

  ## Was 'evaluate' performed in another R process?  If so, draw
  ## 'evaluate' underneath 'lifespan' instead of as above.
  for (idx in js[["index"]]) {
    idx_c <- which((js[["event"]] == "create"  ) & (js[["index"]] == idx))
    idx_e <- which((js[["event"]] == "evaluate") & (js[["index"]] == idx))
    if (js[["session_uuid"]][idx_e] != js[["session_uuid"]][idx_c]) {
      layer[idx_e] <- 1L
    }
  }

  ## Lifespans
  js <- group_by(js, index)
  start <- filter(js, event == "create")[, c("index", "start")]
  stop  <- filter(js, event %in% c("launch", "gather"))[, c("index", "end")]
  stop  <- top_n(stop, n = 1L, wt = "end")
  lifespan <- full_join(start, stop, by = "index")
  gg <- gg + geom_rect(data = lifespan, aes(
    xmin = start, xmax = end,
    ymin = index + yoffset[2], ymax = index + yoffset[2] + height[2],
    fill = "lifespan"
  ))

  ## Events
  gg <- gg + geom_rect(data = js, aes(
    xmin = start, xmax = end,
    ymin = index + yoffset[layer], ymax = index + yoffset[layer] + height[layer],
    fill = event
  ))

  nbr_of_futures <- length(unique(js[["future_uuid"]]))

  gg <- gg + scale_y_continuous(breaks = seq_len(max(nbr_of_futures, 100L)))
  gg <- gg + xlab("Time (seconds)") + ylab("future")
  gg <- gg + labs(fill = "Event")

  ## Generate event colors
  cols <- journal_palette(along = events)
  names(cols) <- events
  
  ## Map events to (color, label)
  cols <- cols[match(map[["event"]], events)]
  labels <- map[["indexed_label"]]
  
  gg <- gg + scale_fill_manual(values = cols, labels = labels)

  ylim <- if (is.null(future_range)) NULL else future_range + c(-0.2, 0.8)
  gg <- gg + coord_cartesian(xlim = time_range, ylim = ylim)

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
