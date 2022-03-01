library(future)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

plan(sequential)
fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
vs <- value(fs)
print(ggjournal(fs))

plan(multisession, workers = 2)
fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
vs <- value(fs)
print(ggjournal(fs))

## Shut down parallel workers
plan(sequential)
