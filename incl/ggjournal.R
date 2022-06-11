library(future.apply)
library(future)
library(ggplot2)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

## Fixate x and y lims
time_range <- c(0, 2.0)
item_range <- c(1, 5)

plan(sequential)

js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js, time_range = time_range, item_range = item_range)

js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
print(ggjournal(js, time_range = time_range, item_range = item_range)


plan(multisession, workers = 2)

js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js, time_range = time_range, item_range = item_range)

js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
print(ggjournal(js, time_range = time_range, item_range = item_range)

## Shut down parallel workers
plan(sequential)
