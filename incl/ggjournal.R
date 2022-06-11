library(future)
library(ggplot2)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

plan(sequential)
js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js, time_range = c(0, 2.0), future_range = c(1, 5)))

plan(multisession, workers = 2)
js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js, time_range = c(0, 2.0), future_range = c(1, 5)))

## Increase polling time to 0.1 seconds (default is 0.01 seconds)
options(future.wait.interval = 0.1)
js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js, time_range = c(0, 2.0), future_range = c(1, 5)))

## Shut down parallel workers
plan(sequential)
