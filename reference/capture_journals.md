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
#>   summary  evaluate evaluate_ratio  overhead overhead_ratio  duration  walltime
#> 1     min 0.11 secs           0.17 0.02 secs          0.033 0.61 secs 0.61 secs
#> 2    mean 0.30 secs           0.43 0.22 secs          0.310 0.70 secs 0.70 secs
#> 3  median 0.35 secs           0.47 0.03 secs          0.041 0.73 secs 0.73 secs
#> 4     max 0.45 secs           0.58 0.60 secs          0.788 0.76 secs 0.76 secs
#> 5   total 0.90 secs           0.43 0.65 secs          0.310 2.11 secs 2.11 secs

## Shut down parallel workers
plan(sequential)
```
