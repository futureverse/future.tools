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
