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
#> Number of futures: 3
#>   summary  evaluate evaluate_ratio   overhead overhead_ratio  duration
#> 1     min 0.10 secs           0.18 0.018 secs          0.030 0.60 secs
#> 2    mean 0.30 secs           0.45 0.211 secs          0.319 0.66 secs
#> 3  median 0.35 secs           0.51 0.023 secs          0.033 0.68 secs
#> 4     max 0.44 secs           0.63 0.591 secs          0.842 0.70 secs
#> 5   total 0.90 secs           0.45 0.632 secs          0.319 1.98 secs
#>    walltime
#> 1 0.60 secs
#> 2 0.66 secs
#> 3 0.68 secs
#> 4 0.70 secs
#> 5 1.98 secs

## Shut down parallel workers
plan(sequential)
```
