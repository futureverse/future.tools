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
#> Wall-clock:            1.024338 secs
#> Cumulative evaluation: 0.6921384 secs (serial estimate)
#> Cumulative overhead:   0.9117968 secs
#> Speedup:               0.68x
#> Parallel efficiency:   33.8% (of 2 workers)
#> Critical path:         0.9021888 secs (longest single future)
#> 
#> Per-future statistics:
#>   summary  evaluate evaluate_ratio   overhead overhead_ratio  duration
#> 1     min 0.10 secs           0.16 0.073 secs           0.11 0.63 secs
#> 2    mean 0.23 secs           0.32 0.304 secs           0.42 0.73 secs
#> 3  median 0.24 secs           0.37 0.289 secs           0.44 0.66 secs
#> 4     max 0.34 secs           0.38 0.551 secs           0.61 0.90 secs
#> 5   total 0.69 secs           0.32 0.912 secs           0.42 2.20 secs
#>    walltime
#> 1 0.63 secs
#> 2 0.73 secs
#> 3 0.66 secs
#> 4 0.90 secs
#> 5 2.20 secs

## Shut down parallel workers
plan(sequential)
```
