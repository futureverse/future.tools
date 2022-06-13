#' Create a Future Journal Plot
#'
#' @param x A list of [future::Future] or FutureJournal objects.
#'
#' @param style (character string) One of `"future"`, `"future-worker"`,
#' and `"worker"`.
#'
#' @param time_range (optional vector of length two) The range of time
#' to displayed.
#'
#' @param item_range (optional vector of length two) The range of future
#' or worker indices to displayed.
#'
#' @param events (character vector; optional) Events to be displayed.
#' If NULL, then all events are displayed.
#'
#' @param baseline (POSIXct; optional) A timestamp to server as time zero
#' for the relative timestamps. If `TRUE` (default), then the earliest
#' timepoint observed is used as the baseline.
#'
#' @param label_fmt (format string; optional) Used to create labels if
#' `future_label` is missing. If NULL, no labels are created.
#'
#' @param \ldots Currently not used.
#'
#' @return
#' A [ggplot2::ggplot] object.
#'
#' @example incl/ggjournal.R
#'
#' @importFrom future journal
#' @import dplyr
#' @import ggplot2
#' @export
ggjournal <- function(x, style = c("future", "future-worker", "worker"), time_range = getOption("future.tools.ggjournal.time_range", NULL), item_range = getOption("future.tools.ggjournal.item_range", NULL), events = NULL, baseline = TRUE, label_fmt = "#%s", ...) {
  style <- match.arg(style)
  ## To please R CMD check
  at <- duration <- end <- index <- event <- future_index <- future_uuid <- start <- NULL

  ## ------------------------------------------------------------------
  ## Merge multiple journals and index the futures
  ## ------------------------------------------------------------------
  if (inherits(x, "FutureJournal") || inherits(x, "data.frame")) x <- list(x)
  js <- journal(x, baseline = baseline)
  js <- mutate(js, start = as.numeric(at), end = as.numeric(at + duration))


  ## ------------------------------------------------------------------
  ## Add 'lifespan' event
  ## ------------------------------------------------------------------
  js <- group_by(js, future_uuid)
  lifespan <- filter(js, event == "create")
  lifespan <- mutate(lifespan, event = as.factor("lifespan"), type = NA_character_)
  stop <- filter(js, event %in% c("launch", "gather"))[, c("future_uuid", "end")]
  stop <- arrange(stop, future_uuid, end)
  stop <- slice_tail(stop, n = 1L)
  lifespan$end <- stop$end
  lifespan <- mutate(lifespan, duration = end - at)
  js <- rbind(js, lifespan)
  js <- arrange(js, end)

  stopifnot(is.factor(js[["event"]]))


  ## ------------------------------------------------------------------
  ## Additional annotations
  ## ------------------------------------------------------------------
  ## Mid time point
  js <- mutate(js, mid = (start + end) / 2)
  
  ## Evaluated in external R process?
  js$external <- rep(FALSE, times = nrow(js))
  for (uuid in js[["future_uuid"]]) {
    keep <- (js[["future_uuid"]] == uuid)
    idx_c <- which(keep & (js[["event"]] == "create"  ))
    idx_e <- which(keep & (js[["event"]] == "evaluate"))
    if (js[["session_uuid"]][idx_e] != js[["session_uuid"]][idx_c]) {
      js$external[keep] <- TRUE
    }
  }
  stopifnot(!anyNA(js$external))

  ## Add 'future' index
  ids <- unique(js[["future_uuid"]])
  js[["future_index"]] <- match(js[["future_uuid"]], ids)
  stopifnot(all(is.finite(js$future_index)))

  ## Add 'session' index
  ids <- unique(js[["session_uuid"]])
  js[["session_index"]] <- match(js[["session_uuid"]], ids) - 1L
  stopifnot(all(is.finite(js$session_index)))

  ## Set future labels, if missing?
  if (is.character(label_fmt)) {
    js <- mutate(js, future_label = case_when(
      is.na(future_label) ~ sprintf(label_fmt, future_index)
    ))
  }


  ## ------------------------------------------------------------------
  ## Vertically separate by future or workers?
  ## ------------------------------------------------------------------
  js[["index"]] <- js[["future_index"]]
  if (style == "worker") js[["index"]] <- js[["session_index"]]
  stopifnot(all(is.finite(js$index)))

  nbr_of_items <- length(unique(ids))


  ## ------------------------------------------------------------------
  ## Events
  ## ------------------------------------------------------------------
  general <- c("lifespan", "resolved")
  chrono <- c("create", "launch", "evaluate", "gather")
  all_events <- c(general, chrono)
  others <- setdiff(as.character(levels(js[["event"]])), all_events)
  all_events <- c(all_events, others)

  if (is.null(events)) {
    events <- c("lifespan", all_events)
  }


  ## ------------------------------------------------------------------
  ## Create (event, legend) map
  ## ------------------------------------------------------------------
  index <- as.character(match(all_events, table = chrono))
  index[is.na(index)] <- ""
  map <- data.frame(event = all_events, index = index)

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
  labels <- tolower(gsub("([[:upper:]])", " \\1", all_events))
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
  rm(list = c("index", "labels", "o"))

  ## ------------------------------------------------------------------
  ## Generate plot
  ## ------------------------------------------------------------------
  layer_height <- c(2, 2, 2, 1)
  layer_height <- 0.8 * layer_height / sum(layer_height)
  layer_voffset <- c(0.0, cumsum(layer_height)[-length(layer_height)])
  layer_voffset <- layer_voffset - layer_height[1]
  stopifnot(all(is.finite(layer_height)), all(is.finite(layer_voffset)))
  
  layer <- rep(3L, times = nrow(js))
  layer[js[["parent"]] == "launch"] <- 4L
  layer[js[["event"]] == "lifespan"] <- 2L
  stopifnot(length(layer) == nrow(js), all(is.finite(layer)))

  voffset <- layer_voffset[layer]
  stopifnot(length(voffset) == nrow(js), all(is.finite(voffset)))
  
  height <- layer_height[layer]
  stopifnot(length(height) == nrow(js), all(is.finite(height)))

  if (style %in% c("future", "future-worker")) {
    keep <- (js$external & (js$event == "evaluate"))
    layer[keep] <- 1L

    if (style == "future-worker") {
      for (uuid in js[["future_uuid"]]) {
        keep <- (js[["future_uuid"]] == uuid)
        idx_e <- which(keep & (js[["event"]] == "evaluate"))
        if (js$external[idx_e]) {
          js$index[idx_e] <- -js$session_index[idx_e]
        }
      }
    }
  } else if (style == "worker") {
    for (uuid in js[["future_uuid"]]) {
      keep <- (js[["future_uuid"]] == uuid)
      idx_e <- which(keep & (js[["event"]] == "evaluate"))
      if (js$external[idx_e]) {
        idx_ls <- which(keep & (js[["event"]] == "lifespan"))
        voffset[idx_ls] <- -layer_height[2]*js$future_index[idx_ls]
      }
    }
  }

  stopifnot(all(is.finite(js$index)))
  stopifnot(length(layer) == nrow(js), all(is.finite(layer)))
  stopifnot(length(voffset) == nrow(js), all(is.finite(voffset)))
  stopifnot(length(height) == nrow(js), all(is.finite(height)))

  js$layer <- layer
  js$voffset <- voffset
  js$height <- height
  rm(list = c("layer", "voffset", "height", "layer_voffset", "layer_height"))

  gg <- ggplot()

  ## Events
  gg <- gg + geom_rect(data = js, aes(
     xmin = start,
     xmax = end,
     ymin = index + voffset,
     ymax = index + voffset + height,
     fill = event
  ))

  js_lifespan <- filter(js, event == "lifespan")
  gg <- gg + geom_rect(data = js_lifespan, aes(
     xmin = start,
     xmax = end,
     ymin = index + voffset,
     ymax = index + voffset + height,
     fill = event
  ), color = "black")

  ## Generate event colors
  cols <- journal_palette(along = all_events)
  names(cols) <- all_events

  ## Map events to (color, label)
  cols <- cols[match(map[["event"]], all_events)]
  labels <- map[["indexed_label"]]

  ## Hide events?
  drop <- (!names(cols) %in% events)
  if (any(drop)) {
    cols[drop] <- "transparent"
    labels[drop] <- ""
  }

  gg <- gg + scale_fill_manual(values = cols, labels = labels)

  ylim <- if (is.null(item_range)) NULL else item_range + c(-0.2, 0.8)
  gg <- gg + coord_cartesian(xlim = time_range, ylim = ylim)

  gg <- gg + scale_y_continuous(breaks = seq_len(max(nbr_of_items, 100L)))
  gg <- gg + xlab("Time (seconds)") + ylab("")
  gg <- gg + labs(fill = "Event")

  attr(gg, "js_expanded") <- js

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
