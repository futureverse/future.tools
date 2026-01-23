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
#> 1     min 0.10 secs           0.23 0.011 secs          0.025 0.46 secs
#> 2    mean 0.27 secs           0.49 0.165 secs          0.298 0.55 secs
#> 3  median 0.31 secs           0.53 0.017 secs          0.029 0.57 secs
#> 4     max 0.41 secs           0.66 0.466 secs          0.747 0.62 secs
#> 5   total 0.82 secs           0.49 0.494 secs          0.298 1.66 secs
#>    walltime
#> 1 0.46 secs
#> 2 0.55 secs
#> 3 0.57 secs
#> 4 0.62 secs
#> 5 1.66 secs

## Shut down parallel workers
plan(sequential)
```
