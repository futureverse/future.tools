# Evaluate an R expression while collecting journals from completed futures

Evaluate an R expression while collecting journals from completed
futures

## Usage

``` r
capture_journals(expr, substitute = TRUE, envir = parent.frame())
```

## Arguments

- expr:

  The R expression to evaluate

- substitute:

  If TRUE, then `expr` is substituted, otherwise not.

- envir:

  The environment where `expr` should be evaluated

## Value

A list of FutureJournal:s.

## Details

This function evaluates an R expression and capture the journals
signaled by futures as they are completed. A future
[journal](https://future.tools.futureverse.org/reference/journal.md)
comprise a log of events appearing during the life-span of a future,
e.g. the timestamps when the future was created, launched, queried,
resolved, and its results are collected.

## Examples

``` r
slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

plan(multisession, workers = 2)
js <- capture_journals({
  fs <- lapply(3:1, FUN = function(x) future(slow_fcn(x)))
  value(fs)
})

## Summarize all journals
js_all <- Reduce(rbind, js)
print(summary(js_all), digits = 2L)
#> Number of futures:     3
#> Workers used (peak):   2 [concurrent]
#> Wall-clock:            0.6507196 secs
#> Cumulative evaluation: 0.8061283 secs (serial estimate)
#> Cumulative overhead:   0.5333669 secs
#> Speedup:               1.24x
#> Parallel efficiency:   61.9% (of 2 workers)
#> Critical path:         0.6147299 secs (longest single future)
#> 
#> Per-future statistics:
#>   summary  evaluate evaluate_ratio   overhead overhead_ratio  duration
#> 1     min 0.10 secs           0.21 0.016 secs          0.031 0.50 secs
#> 2    mean 0.27 secs           0.47 0.178 secs          0.308 0.58 secs
#> 3  median 0.30 secs           0.49 0.019 secs          0.031 0.61 secs
#> 4     max 0.40 secs           0.65 0.499 secs          0.811 0.61 secs
#> 5   total 0.81 secs           0.47 0.533 secs          0.308 1.73 secs
#>    walltime
#> 1 0.50 secs
#> 2 0.58 secs
#> 3 0.61 secs
#> 4 0.61 secs
#> 5 1.73 secs

## Shut down parallel workers
plan(sequential)
```
