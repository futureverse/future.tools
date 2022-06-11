library(future.apply)
library(ggplot2)

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

plan(sequential)
js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
print(ggjournal(js) + xlim(0.0, 2.0))

plan(multisession, workers = 2)
js <- capture_journals({
  vs <- future_lapply(5:1, FUN = slow_fcn)
})
print(ggjournal(js) + xlim(0.0, 2.0))

## Shut down parallel workers
plan(sequential)
