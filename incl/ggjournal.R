library(future.apply)
library(future)
library(ggplot2)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

## Plot with fixed x and y limits
ggjournal_x <- function(js) {
  for (by in c("future", "worker")) {
    item_range <- if (by == "future") c(1, 5) else c(0, 1.5)
    print(ggjournal(js, by = by,
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
