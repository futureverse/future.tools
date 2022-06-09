library(future)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

plan(sequential)
js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js))

plan(multisession, workers = 2)
js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js))

## Increase polling time to 0.1 seconds (default is 0.01 seconds)
options(future.wait.interval = 0.1)
js <- capture_journals({
  fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
  vs <- value(fs)
})
print(ggjournal(js))

## Shut down parallel workers
plan(sequential)
