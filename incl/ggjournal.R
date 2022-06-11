library(future.apply)
library(future)
library(ggplot2)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

## Fixate x and y lims
options(future.tools.ggjournal.time_range = c(0, 2.0))
options(future.tools.ggjournal.item_range = c(1, 5))


plan(sequential)

js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js))

js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
print(ggjournal(js))


plan(multisession, workers = 2)

js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js))

js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
print(ggjournal(js))


## Shut down parallel workers
plan(sequential)
