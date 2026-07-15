# Gets the logged journal of events for a future

*WARNING: This function is under development. It can change at any time.
For now, please, do not depend on this function in a published R
package.*

## Usage

``` r
journal(x, ...)
```

## Arguments

- x:

  A
  [future::Future](https://future.futureverse.org/reference/Future-class.html)
  object.

- ...:

  Not used.

## Value

A data frame of class `FutureJournal` with columns:

1.  `event` (character string) - type of event that took place

2.  `category` (character string) - the category of the event

3.  `parent` (character string) - (to be describe)

4.  `start` (POSIXct) - the timestamp when the event started

5.  `at` (difftime) - the time when the event started relative to first
    event

6.  `duration` (difftime) - the duration of the event

7.  `future_label` (character string) - the label of the future

8.  `future_uuid` (character string) - the UUID of the future

9.  `session_uuid` (character string) - the UUID of the R session where
    the event took place

The common events are:

- `create` - the future was created (an `overhead`)

- `launch` - the future was launched (an `overhead`)

- `evaluate` - the future was evaluated (an `evaluation`)

- `resolved` - the future was queried (may be occur multiple times) (an
  `overhead`)

- `gather` - the results was retrieved (an `overhead`)

but others may be added by other Future classes.

Common event categories are:

- `evaluation` - processing time is spent on evaluation

- `overhead` - processing time is spent on orchestrating the future

- `waiting` - processing time is spent on waiting to set up or querying
  the future

but others may be added by other Future classes.

The data frame is sorted by the `at` time. Note that the timestamps for
the `evaluate` event are based on the local time on the worker. The
system clocks on the worker and the calling R system may not be in
perfect sync.

## Enabling and disabling event logging

To enable logging of events, set option `future.journal` is TRUE. To
disable, set it to FALSE (default).

## See also

Use
[`capture_journals()`](https://future.tools.futureverse.org/reference/capture_journals.md)
to capture journals from all futures.

## Examples

``` r
## Enable journaling of futures
oopts <- options(future.journal = TRUE)

plan(multisession, workers = 2L)

t_start <- Sys.time()
fs <- lapply(1:3, FUN = function(x) future({ Sys.sleep(x); sqrt(x) }))
vs <- value(fs)
js <- lapply(fs, FUN = journal, baseline = t_start)
print(js)
#> [[1]]
#>            event   category parent               start               at
#> 1         create   overhead   <NA> 2026-07-15 15:24:46 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-07-15 15:24:46 0.004933596 secs
#> 2      getWorker   overhead launch 2026-07-15 15:24:46 0.005052090 secs
#> 3    eraseWorker   overhead launch 2026-07-15 15:24:46 0.005919218 secs
#> 5       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.045169115 secs
#> 6       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.079361439 secs
#> 7       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.113365650 secs
#> 8       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.147440434 secs
#> 40      evaluate evaluation   <NA> 2026-07-15 15:24:46 0.168880224 secs
#> 9       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.181589127 secs
#> 10      resolved       <NA>   <NA> 2026-07-15 15:24:46 0.216688156 secs
#> 11      resolved       <NA>   <NA> 2026-07-15 15:24:46 0.252628565 secs
#> 12      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.286573887 secs
#> 13      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.322326899 secs
#> 14      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.360293865 secs
#> 15      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.397513151 secs
#> 16      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.434774160 secs
#> 17      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.472925186 secs
#> 18      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.510628462 secs
#> 19      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.548541784 secs
#> 20      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.586598396 secs
#> 21      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.624863863 secs
#> 22      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.664083719 secs
#> 23      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.701187372 secs
#> 24      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.739531279 secs
#> 25      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.778375626 secs
#> 26      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.823377609 secs
#> 27      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.862631559 secs
#> 28      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.901816130 secs
#> 29      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.941214800 secs
#> 30      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.980881453 secs
#> 31      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.020823240 secs
#> 32      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.060252190 secs
#> 33      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.099907398 secs
#> 34      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.139415503 secs
#> 35      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.179497004 secs
#> 36      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.219181538 secs
#> 37      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.258678198 secs
#> 41      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.298735857 secs
#> 38 receiveResult   overhead gather 2026-07-15 15:24:48 1.299933434 secs
#> 39        gather   overhead   <NA> 2026-07-15 15:24:48 1.301763773 secs
#> 42      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.314651966 secs
#> 43       resolve   overhead   <NA> 2026-07-15 15:24:48 1.315476179 secs
#>             duration future_label                         future_uuid
#> 1  4.708290e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 4  4.850149e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 2  4.324913e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 3  3.232956e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 5  1.106834e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 6  1.093268e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 7  1.085377e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 8  1.081562e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 40 1.105723e+00 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 9  1.088428e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 10 1.100302e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 11 1.103711e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 12 1.072526e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 13 1.183486e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 14 1.144767e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 15 1.137233e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 16 1.185107e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 17 1.128912e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 18 1.145458e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 19 1.139545e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 20 1.150322e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 21 1.183224e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 22 1.073098e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 23 1.152253e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 24 1.153874e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 25 1.782727e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 26 1.152039e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 27 1.153612e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 28 1.179099e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 29 1.167345e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 30 1.156950e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 31 1.139140e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 32 1.141787e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 33 1.147270e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 34 1.136756e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 35 1.135468e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 36 1.170325e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 37 1.144409e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 41 7.432938e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 38 2.832413e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 39 1.327991e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 42 4.076958e-05 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#> 43 1.072884e-05 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-17
#>                        session_uuid
#> 1  6cde5c2a5c168832eb65c22662984b63
#> 4  6cde5c2a5c168832eb65c22662984b63
#> 2  6cde5c2a5c168832eb65c22662984b63
#> 3  6cde5c2a5c168832eb65c22662984b63
#> 5  6cde5c2a5c168832eb65c22662984b63
#> 6  6cde5c2a5c168832eb65c22662984b63
#> 7  6cde5c2a5c168832eb65c22662984b63
#> 8  6cde5c2a5c168832eb65c22662984b63
#> 40 6cde5c2a5c168832eb65c22662984b63
#> 9  6cde5c2a5c168832eb65c22662984b63
#> 10 6cde5c2a5c168832eb65c22662984b63
#> 11 6cde5c2a5c168832eb65c22662984b63
#> 12 6cde5c2a5c168832eb65c22662984b63
#> 13 6cde5c2a5c168832eb65c22662984b63
#> 14 6cde5c2a5c168832eb65c22662984b63
#> 15 6cde5c2a5c168832eb65c22662984b63
#> 16 6cde5c2a5c168832eb65c22662984b63
#> 17 6cde5c2a5c168832eb65c22662984b63
#> 18 6cde5c2a5c168832eb65c22662984b63
#> 19 6cde5c2a5c168832eb65c22662984b63
#> 20 6cde5c2a5c168832eb65c22662984b63
#> 21 6cde5c2a5c168832eb65c22662984b63
#> 22 6cde5c2a5c168832eb65c22662984b63
#> 23 6cde5c2a5c168832eb65c22662984b63
#> 24 6cde5c2a5c168832eb65c22662984b63
#> 25 6cde5c2a5c168832eb65c22662984b63
#> 26 6cde5c2a5c168832eb65c22662984b63
#> 27 6cde5c2a5c168832eb65c22662984b63
#> 28 6cde5c2a5c168832eb65c22662984b63
#> 29 6cde5c2a5c168832eb65c22662984b63
#> 30 6cde5c2a5c168832eb65c22662984b63
#> 31 6cde5c2a5c168832eb65c22662984b63
#> 32 6cde5c2a5c168832eb65c22662984b63
#> 33 6cde5c2a5c168832eb65c22662984b63
#> 34 6cde5c2a5c168832eb65c22662984b63
#> 35 6cde5c2a5c168832eb65c22662984b63
#> 36 6cde5c2a5c168832eb65c22662984b63
#> 37 6cde5c2a5c168832eb65c22662984b63
#> 41 6cde5c2a5c168832eb65c22662984b63
#> 38 6cde5c2a5c168832eb65c22662984b63
#> 39 6cde5c2a5c168832eb65c22662984b63
#> 42 6cde5c2a5c168832eb65c22662984b63
#> 43 6cde5c2a5c168832eb65c22662984b63
#> 
#> [[2]]
#>            event   category parent               start               at
#> 1         create   overhead   <NA> 2026-07-15 15:24:46 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-07-15 15:24:46 0.008767128 secs
#> 2      getWorker   overhead launch 2026-07-15 15:24:46 0.008917809 secs
#> 3    eraseWorker   overhead launch 2026-07-15 15:24:46 0.010170937 secs
#> 5       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.046609402 secs
#> 6       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.080729485 secs
#> 7       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.114659309 secs
#> 8       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.148670435 secs
#> 67      evaluate evaluation   <NA> 2026-07-15 15:24:46 0.157828808 secs
#> 9       resolved       <NA>   <NA> 2026-07-15 15:24:46 0.183317661 secs
#> 10      resolved       <NA>   <NA> 2026-07-15 15:24:46 0.218659878 secs
#> 11      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.253880262 secs
#> 12      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.287912846 secs
#> 13      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.325613260 secs
#> 14      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.362846851 secs
#> 15      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.400093079 secs
#> 16      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.437896013 secs
#> 17      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.475144148 secs
#> 18      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.513108969 secs
#> 19      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.551062822 secs
#> 20      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.589224339 secs
#> 21      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.627738953 secs
#> 22      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.665683508 secs
#> 23      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.703897715 secs
#> 24      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.742206097 secs
#> 25      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.787361145 secs
#> 26      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.826399326 secs
#> 27      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.865338087 secs
#> 28      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.904641867 secs
#> 29      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.944027901 secs
#> 30      resolved       <NA>   <NA> 2026-07-15 15:24:47 0.983745575 secs
#> 31      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.023402929 secs
#> 32      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.062863588 secs
#> 33      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.102380753 secs
#> 34      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.141843796 secs
#> 35      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.181889057 secs
#> 36      resolved       <NA>   <NA> 2026-07-15 15:24:47 1.221516132 secs
#> 37      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.261132717 secs
#> 38      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.305424929 secs
#> 39      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.339725256 secs
#> 40      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.376146317 secs
#> 41      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.412412882 secs
#> 42      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.451666594 secs
#> 43      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.487126350 secs
#> 44      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.523255825 secs
#> 45      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.559343576 secs
#> 46      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.595535517 secs
#> 47      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.632216454 secs
#> 48      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.668390274 secs
#> 49      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.704411745 secs
#> 50      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.740670919 secs
#> 51      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.776708126 secs
#> 52      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.812495708 secs
#> 53      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.848628283 secs
#> 54      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.884955406 secs
#> 55      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.921216249 secs
#> 56      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.957233429 secs
#> 57      resolved       <NA>   <NA> 2026-07-15 15:24:48 1.993393660 secs
#> 58      resolved       <NA>   <NA> 2026-07-15 15:24:48 2.029226303 secs
#> 59      resolved       <NA>   <NA> 2026-07-15 15:24:48 2.065247536 secs
#> 60      resolved       <NA>   <NA> 2026-07-15 15:24:48 2.101156473 secs
#> 61      resolved       <NA>   <NA> 2026-07-15 15:24:48 2.137163877 secs
#> 62      resolved       <NA>   <NA> 2026-07-15 15:24:48 2.174320459 secs
#> 63      resolved       <NA>   <NA> 2026-07-15 15:24:48 2.210649252 secs
#> 64      resolved       <NA>   <NA> 2026-07-15 15:24:49 2.247060061 secs
#> 68      resolved       <NA>   <NA> 2026-07-15 15:24:49 2.284966946 secs
#> 65 receiveResult   overhead gather 2026-07-15 15:24:49 2.285500765 secs
#> 66        gather   overhead   <NA> 2026-07-15 15:24:49 2.286386728 secs
#> 69       resolve   overhead   <NA> 2026-07-15 15:24:49 2.289327860 secs
#>             duration future_label                         future_uuid
#> 1  8.391142e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 4  5.848169e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 2  5.867481e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 3  3.645420e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 5  1.094127e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 6  1.079273e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 7  1.084590e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 8  1.083279e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 67 2.108882e+00 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 9  1.104236e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 10 1.114678e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 11 1.059866e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 12 1.103759e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 13 1.132321e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 14 1.121402e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 15 1.120567e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 16 1.132226e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 17 1.156807e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 18 1.145434e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 19 1.142573e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 20 1.136017e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 21 1.127434e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 22 1.107073e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 23 1.128602e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 24 1.149082e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 25 1.132822e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 26 1.124597e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 27 1.129699e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 28 1.136541e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 29 1.138353e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 30 1.150560e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 31 1.126146e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 32 1.128125e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 33 1.130891e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 34 1.173925e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 35 1.126885e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 36 1.103282e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 37 1.122594e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 38 1.057601e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 39 1.144385e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 40 1.157403e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 41 1.162577e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 42 1.203895e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 43 1.141453e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 44 1.150393e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 45 1.139355e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 46 1.159859e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 47 1.134682e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 48 1.146460e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 49 1.146150e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 50 1.151633e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 51 1.145744e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 52 1.159120e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 53 1.157641e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 54 1.159120e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 55 1.144004e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 56 1.161051e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 57 1.150584e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 58 1.145935e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 59 1.139951e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 60 1.147151e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 61 1.153731e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 62 1.152110e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 63 1.142073e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 64 1.148582e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 68 3.514528e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 65 1.382828e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 66 7.057190e-05 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#> 69 1.263618e-05 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-18
#>                        session_uuid
#> 1  6cde5c2a5c168832eb65c22662984b63
#> 4  6cde5c2a5c168832eb65c22662984b63
#> 2  6cde5c2a5c168832eb65c22662984b63
#> 3  6cde5c2a5c168832eb65c22662984b63
#> 5  6cde5c2a5c168832eb65c22662984b63
#> 6  6cde5c2a5c168832eb65c22662984b63
#> 7  6cde5c2a5c168832eb65c22662984b63
#> 8  6cde5c2a5c168832eb65c22662984b63
#> 67 6cde5c2a5c168832eb65c22662984b63
#> 9  6cde5c2a5c168832eb65c22662984b63
#> 10 6cde5c2a5c168832eb65c22662984b63
#> 11 6cde5c2a5c168832eb65c22662984b63
#> 12 6cde5c2a5c168832eb65c22662984b63
#> 13 6cde5c2a5c168832eb65c22662984b63
#> 14 6cde5c2a5c168832eb65c22662984b63
#> 15 6cde5c2a5c168832eb65c22662984b63
#> 16 6cde5c2a5c168832eb65c22662984b63
#> 17 6cde5c2a5c168832eb65c22662984b63
#> 18 6cde5c2a5c168832eb65c22662984b63
#> 19 6cde5c2a5c168832eb65c22662984b63
#> 20 6cde5c2a5c168832eb65c22662984b63
#> 21 6cde5c2a5c168832eb65c22662984b63
#> 22 6cde5c2a5c168832eb65c22662984b63
#> 23 6cde5c2a5c168832eb65c22662984b63
#> 24 6cde5c2a5c168832eb65c22662984b63
#> 25 6cde5c2a5c168832eb65c22662984b63
#> 26 6cde5c2a5c168832eb65c22662984b63
#> 27 6cde5c2a5c168832eb65c22662984b63
#> 28 6cde5c2a5c168832eb65c22662984b63
#> 29 6cde5c2a5c168832eb65c22662984b63
#> 30 6cde5c2a5c168832eb65c22662984b63
#> 31 6cde5c2a5c168832eb65c22662984b63
#> 32 6cde5c2a5c168832eb65c22662984b63
#> 33 6cde5c2a5c168832eb65c22662984b63
#> 34 6cde5c2a5c168832eb65c22662984b63
#> 35 6cde5c2a5c168832eb65c22662984b63
#> 36 6cde5c2a5c168832eb65c22662984b63
#> 37 6cde5c2a5c168832eb65c22662984b63
#> 38 6cde5c2a5c168832eb65c22662984b63
#> 39 6cde5c2a5c168832eb65c22662984b63
#> 40 6cde5c2a5c168832eb65c22662984b63
#> 41 6cde5c2a5c168832eb65c22662984b63
#> 42 6cde5c2a5c168832eb65c22662984b63
#> 43 6cde5c2a5c168832eb65c22662984b63
#> 44 6cde5c2a5c168832eb65c22662984b63
#> 45 6cde5c2a5c168832eb65c22662984b63
#> 46 6cde5c2a5c168832eb65c22662984b63
#> 47 6cde5c2a5c168832eb65c22662984b63
#> 48 6cde5c2a5c168832eb65c22662984b63
#> 49 6cde5c2a5c168832eb65c22662984b63
#> 50 6cde5c2a5c168832eb65c22662984b63
#> 51 6cde5c2a5c168832eb65c22662984b63
#> 52 6cde5c2a5c168832eb65c22662984b63
#> 53 6cde5c2a5c168832eb65c22662984b63
#> 54 6cde5c2a5c168832eb65c22662984b63
#> 55 6cde5c2a5c168832eb65c22662984b63
#> 56 6cde5c2a5c168832eb65c22662984b63
#> 57 6cde5c2a5c168832eb65c22662984b63
#> 58 6cde5c2a5c168832eb65c22662984b63
#> 59 6cde5c2a5c168832eb65c22662984b63
#> 60 6cde5c2a5c168832eb65c22662984b63
#> 61 6cde5c2a5c168832eb65c22662984b63
#> 62 6cde5c2a5c168832eb65c22662984b63
#> 63 6cde5c2a5c168832eb65c22662984b63
#> 64 6cde5c2a5c168832eb65c22662984b63
#> 68 6cde5c2a5c168832eb65c22662984b63
#> 65 6cde5c2a5c168832eb65c22662984b63
#> 66 6cde5c2a5c168832eb65c22662984b63
#> 69 6cde5c2a5c168832eb65c22662984b63
#> 
#> [[3]]
#>             event   category parent               start               at
#> 1          create   overhead   <NA> 2026-07-15 15:24:46 0.000000000 secs
#> 4          launch   overhead   <NA> 2026-07-15 15:24:46 0.009009361 secs
#> 2       getWorker   overhead launch 2026-07-15 15:24:46 0.009173393 secs
#> 3     eraseWorker   overhead launch 2026-07-15 15:24:48 1.283761024 secs
#> 123      evaluate evaluation   <NA> 2026-07-15 15:24:48 1.289731741 secs
#> 5        resolved       <NA>   <NA> 2026-07-15 15:24:48 1.301622629 secs
#> 6        resolved       <NA>   <NA> 2026-07-15 15:24:48 1.337493181 secs
#> 7        resolved       <NA>   <NA> 2026-07-15 15:24:48 1.374014378 secs
#> 8        resolved       <NA>   <NA> 2026-07-15 15:24:48 1.410526276 secs
#> 9        resolved       <NA>   <NA> 2026-07-15 15:24:48 1.449789286 secs
#> 10       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.484808445 secs
#> 11       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.520967007 secs
#> 12       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.556957722 secs
#> 13       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.593870163 secs
#> 14       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.629763842 secs
#> 15       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.666423082 secs
#> 16       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.702233553 secs
#> 17       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.738467693 secs
#> 18       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.774388313 secs
#> 19       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.810314894 secs
#> 20       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.846548557 secs
#> 21       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.882766247 secs
#> 22       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.918933392 secs
#> 23       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.955154657 secs
#> 24       resolved       <NA>   <NA> 2026-07-15 15:24:48 1.991097927 secs
#> 25       resolved       <NA>   <NA> 2026-07-15 15:24:48 2.026958227 secs
#> 26       resolved       <NA>   <NA> 2026-07-15 15:24:48 2.062823534 secs
#> 27       resolved       <NA>   <NA> 2026-07-15 15:24:48 2.098915815 secs
#> 28       resolved       <NA>   <NA> 2026-07-15 15:24:48 2.134900331 secs
#> 29       resolved       <NA>   <NA> 2026-07-15 15:24:48 2.172155380 secs
#> 30       resolved       <NA>   <NA> 2026-07-15 15:24:48 2.208484888 secs
#> 31       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.245904446 secs
#> 32       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.274575949 secs
#> 33       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.296188593 secs
#> 34       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.319526672 secs
#> 35       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.342863560 secs
#> 36       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.366370440 secs
#> 37       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.388723612 secs
#> 38       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.410508633 secs
#> 39       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.432348728 secs
#> 40       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.455654383 secs
#> 41       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.479164362 secs
#> 42       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.502843618 secs
#> 43       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.524660826 secs
#> 44       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.548358440 secs
#> 45       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.571957588 secs
#> 46       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.595325232 secs
#> 47       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.618668795 secs
#> 48       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.641741753 secs
#> 49       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.664872169 secs
#> 50       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.687941074 secs
#> 51       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.711102724 secs
#> 52       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.734261036 secs
#> 53       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.757573128 secs
#> 54       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.779629946 secs
#> 55       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.801531076 secs
#> 56       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.822892904 secs
#> 57       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.844778061 secs
#> 58       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.866544247 secs
#> 59       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.888037920 secs
#> 60       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.909438372 secs
#> 61       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.930733442 secs
#> 62       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.952270508 secs
#> 63       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.973590851 secs
#> 64       resolved       <NA>   <NA> 2026-07-15 15:24:49 2.995026588 secs
#> 65       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.020295620 secs
#> 66       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.041895151 secs
#> 67       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.063835382 secs
#> 68       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.085632563 secs
#> 69       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.108594418 secs
#> 70       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.131966829 secs
#> 71       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.155305862 secs
#> 72       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.178369999 secs
#> 73       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.201736450 secs
#> 74       resolved       <NA>   <NA> 2026-07-15 15:24:49 3.225624561 secs
#> 75       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.247440577 secs
#> 76       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.268995762 secs
#> 77       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.290468693 secs
#> 78       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.311893225 secs
#> 79       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.334775686 secs
#> 80       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.357939720 secs
#> 81       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.381032705 secs
#> 82       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.402470827 secs
#> 83       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.424344301 secs
#> 84       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.447871208 secs
#> 85       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.471172094 secs
#> 86       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.494114161 secs
#> 87       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.517569304 secs
#> 88       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.540730238 secs
#> 89       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.563943624 secs
#> 90       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.587242603 secs
#> 91       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.610415220 secs
#> 92       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.633644819 secs
#> 93       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.656613827 secs
#> 94       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.679879427 secs
#> 95       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.703259468 secs
#> 96       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.726355553 secs
#> 97       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.749490976 secs
#> 98       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.774426222 secs
#> 99       resolved       <NA>   <NA> 2026-07-15 15:24:50 3.797250986 secs
#> 100      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.820433140 secs
#> 101      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.843503714 secs
#> 102      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.866507292 secs
#> 103      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.889590263 secs
#> 104      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.913523912 secs
#> 105      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.936769485 secs
#> 106      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.959859848 secs
#> 107      resolved       <NA>   <NA> 2026-07-15 15:24:50 3.981987476 secs
#> 108      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.003690958 secs
#> 109      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.026834011 secs
#> 110      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.050003529 secs
#> 111      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.073095560 secs
#> 112      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.096307039 secs
#> 113      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.119510889 secs
#> 114      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.143009901 secs
#> 115      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.166690826 secs
#> 116      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.189971685 secs
#> 117      resolved       <NA>   <NA> 2026-07-15 15:24:50 4.213340282 secs
#> 118      resolved       <NA>   <NA> 2026-07-15 15:24:51 4.236693621 secs
#> 119      resolved       <NA>   <NA> 2026-07-15 15:24:51 4.259680271 secs
#> 120      resolved       <NA>   <NA> 2026-07-15 15:24:51 4.282953501 secs
#> 124      resolved       <NA>   <NA> 2026-07-15 15:24:51 4.304615974 secs
#> 121 receiveResult   overhead gather 2026-07-15 15:24:51 4.305205345 secs
#> 122        gather   overhead   <NA> 2026-07-15 15:24:51 4.306066036 secs
#> 125       resolve   overhead   <NA> 2026-07-15 15:24:51 4.310422421 secs
#>              duration future_label                         future_uuid
#> 1   8.630276e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 4   1.278912e+00 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 2   1.273725e+00 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 3   6.470680e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 123 3.004546e+00 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 5   1.116085e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 6   1.138115e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 7   1.131248e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 8   1.139784e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 9   1.112461e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 10  1.131892e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 11  1.136065e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 12  1.139998e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 13  1.136279e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 14  1.140428e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 15  1.114964e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 16  1.129556e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 17  1.124334e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 18  1.123619e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 19  1.129580e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 20  1.131701e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 21  1.133323e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 22  1.124096e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 23  1.124716e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 24  1.123571e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 25  1.130891e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 26  1.120687e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 27  1.133633e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 28  1.122069e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 29  1.140785e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 30  1.146960e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 31  1.198363e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 32  1.048779e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 33  1.131368e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 34  1.156735e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 35  1.173401e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 36  1.140261e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 37  1.072669e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 38  1.060987e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 39  1.144290e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 40  1.165771e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 41  1.219130e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 42  1.071453e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 43  1.194406e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 44  1.214933e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 45  1.154637e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 46  1.135778e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 47  1.141405e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 48  1.139307e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 49  1.146865e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 50  1.144290e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 51  1.141453e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 52  1.213241e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 53  1.098061e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 54  1.092958e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 55  1.062179e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 56  1.079273e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 57  1.093006e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 58  1.071811e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 59  1.066422e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 60  1.059890e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 61  1.066422e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 62  1.063037e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 63  1.065946e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 64  1.447511e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 65  1.066613e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 66  1.061249e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 67  1.080728e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 68  1.128793e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 69  1.135421e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 70  1.171446e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 71  1.125336e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 72  1.142335e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 73  1.138830e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 74  1.106477e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 75  1.082087e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 76  1.071501e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 77  1.064849e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 78  1.107502e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 79  1.150393e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 80  1.156354e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 81  1.064539e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 82  1.063395e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 83  1.183605e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 84  1.142097e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 85  1.132774e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 86  1.135755e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 87  1.136327e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 88  1.146650e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 89  1.145887e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 90  1.147771e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 91  1.144934e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 92  1.129699e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 93  1.145983e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 94  1.149297e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 95  1.138282e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 96  1.139975e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 97  1.142597e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 98  1.205063e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 99  1.090074e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 100 1.133800e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 101 1.134896e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 102 1.137042e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 103 1.222610e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 104 1.150417e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 105 1.144075e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 106 1.129842e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 107 1.063561e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 108 1.140356e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 109 1.149917e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 110 1.144671e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 111 1.141453e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 112 1.141715e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 113 1.161790e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 114 1.166272e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 115 1.149893e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 116 1.122785e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 117 1.147532e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 118 1.136661e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 119 1.164365e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 120 1.097035e-02 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 124 4.331350e-03 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 121 1.404285e-04 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 122 5.769730e-05 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#> 125 2.074242e-05 secs         <NA> 6cde5c2a5c168832eb65c22662984b63-19
#>                         session_uuid
#> 1   6cde5c2a5c168832eb65c22662984b63
#> 4   6cde5c2a5c168832eb65c22662984b63
#> 2   6cde5c2a5c168832eb65c22662984b63
#> 3   6cde5c2a5c168832eb65c22662984b63
#> 123 6cde5c2a5c168832eb65c22662984b63
#> 5   6cde5c2a5c168832eb65c22662984b63
#> 6   6cde5c2a5c168832eb65c22662984b63
#> 7   6cde5c2a5c168832eb65c22662984b63
#> 8   6cde5c2a5c168832eb65c22662984b63
#> 9   6cde5c2a5c168832eb65c22662984b63
#> 10  6cde5c2a5c168832eb65c22662984b63
#> 11  6cde5c2a5c168832eb65c22662984b63
#> 12  6cde5c2a5c168832eb65c22662984b63
#> 13  6cde5c2a5c168832eb65c22662984b63
#> 14  6cde5c2a5c168832eb65c22662984b63
#> 15  6cde5c2a5c168832eb65c22662984b63
#> 16  6cde5c2a5c168832eb65c22662984b63
#> 17  6cde5c2a5c168832eb65c22662984b63
#> 18  6cde5c2a5c168832eb65c22662984b63
#> 19  6cde5c2a5c168832eb65c22662984b63
#> 20  6cde5c2a5c168832eb65c22662984b63
#> 21  6cde5c2a5c168832eb65c22662984b63
#> 22  6cde5c2a5c168832eb65c22662984b63
#> 23  6cde5c2a5c168832eb65c22662984b63
#> 24  6cde5c2a5c168832eb65c22662984b63
#> 25  6cde5c2a5c168832eb65c22662984b63
#> 26  6cde5c2a5c168832eb65c22662984b63
#> 27  6cde5c2a5c168832eb65c22662984b63
#> 28  6cde5c2a5c168832eb65c22662984b63
#> 29  6cde5c2a5c168832eb65c22662984b63
#> 30  6cde5c2a5c168832eb65c22662984b63
#> 31  6cde5c2a5c168832eb65c22662984b63
#> 32  6cde5c2a5c168832eb65c22662984b63
#> 33  6cde5c2a5c168832eb65c22662984b63
#> 34  6cde5c2a5c168832eb65c22662984b63
#> 35  6cde5c2a5c168832eb65c22662984b63
#> 36  6cde5c2a5c168832eb65c22662984b63
#> 37  6cde5c2a5c168832eb65c22662984b63
#> 38  6cde5c2a5c168832eb65c22662984b63
#> 39  6cde5c2a5c168832eb65c22662984b63
#> 40  6cde5c2a5c168832eb65c22662984b63
#> 41  6cde5c2a5c168832eb65c22662984b63
#> 42  6cde5c2a5c168832eb65c22662984b63
#> 43  6cde5c2a5c168832eb65c22662984b63
#> 44  6cde5c2a5c168832eb65c22662984b63
#> 45  6cde5c2a5c168832eb65c22662984b63
#> 46  6cde5c2a5c168832eb65c22662984b63
#> 47  6cde5c2a5c168832eb65c22662984b63
#> 48  6cde5c2a5c168832eb65c22662984b63
#> 49  6cde5c2a5c168832eb65c22662984b63
#> 50  6cde5c2a5c168832eb65c22662984b63
#> 51  6cde5c2a5c168832eb65c22662984b63
#> 52  6cde5c2a5c168832eb65c22662984b63
#> 53  6cde5c2a5c168832eb65c22662984b63
#> 54  6cde5c2a5c168832eb65c22662984b63
#> 55  6cde5c2a5c168832eb65c22662984b63
#> 56  6cde5c2a5c168832eb65c22662984b63
#> 57  6cde5c2a5c168832eb65c22662984b63
#> 58  6cde5c2a5c168832eb65c22662984b63
#> 59  6cde5c2a5c168832eb65c22662984b63
#> 60  6cde5c2a5c168832eb65c22662984b63
#> 61  6cde5c2a5c168832eb65c22662984b63
#> 62  6cde5c2a5c168832eb65c22662984b63
#> 63  6cde5c2a5c168832eb65c22662984b63
#> 64  6cde5c2a5c168832eb65c22662984b63
#> 65  6cde5c2a5c168832eb65c22662984b63
#> 66  6cde5c2a5c168832eb65c22662984b63
#> 67  6cde5c2a5c168832eb65c22662984b63
#> 68  6cde5c2a5c168832eb65c22662984b63
#> 69  6cde5c2a5c168832eb65c22662984b63
#> 70  6cde5c2a5c168832eb65c22662984b63
#> 71  6cde5c2a5c168832eb65c22662984b63
#> 72  6cde5c2a5c168832eb65c22662984b63
#> 73  6cde5c2a5c168832eb65c22662984b63
#> 74  6cde5c2a5c168832eb65c22662984b63
#> 75  6cde5c2a5c168832eb65c22662984b63
#> 76  6cde5c2a5c168832eb65c22662984b63
#> 77  6cde5c2a5c168832eb65c22662984b63
#> 78  6cde5c2a5c168832eb65c22662984b63
#> 79  6cde5c2a5c168832eb65c22662984b63
#> 80  6cde5c2a5c168832eb65c22662984b63
#> 81  6cde5c2a5c168832eb65c22662984b63
#> 82  6cde5c2a5c168832eb65c22662984b63
#> 83  6cde5c2a5c168832eb65c22662984b63
#> 84  6cde5c2a5c168832eb65c22662984b63
#> 85  6cde5c2a5c168832eb65c22662984b63
#> 86  6cde5c2a5c168832eb65c22662984b63
#> 87  6cde5c2a5c168832eb65c22662984b63
#> 88  6cde5c2a5c168832eb65c22662984b63
#> 89  6cde5c2a5c168832eb65c22662984b63
#> 90  6cde5c2a5c168832eb65c22662984b63
#> 91  6cde5c2a5c168832eb65c22662984b63
#> 92  6cde5c2a5c168832eb65c22662984b63
#> 93  6cde5c2a5c168832eb65c22662984b63
#> 94  6cde5c2a5c168832eb65c22662984b63
#> 95  6cde5c2a5c168832eb65c22662984b63
#> 96  6cde5c2a5c168832eb65c22662984b63
#> 97  6cde5c2a5c168832eb65c22662984b63
#> 98  6cde5c2a5c168832eb65c22662984b63
#> 99  6cde5c2a5c168832eb65c22662984b63
#> 100 6cde5c2a5c168832eb65c22662984b63
#> 101 6cde5c2a5c168832eb65c22662984b63
#> 102 6cde5c2a5c168832eb65c22662984b63
#> 103 6cde5c2a5c168832eb65c22662984b63
#> 104 6cde5c2a5c168832eb65c22662984b63
#> 105 6cde5c2a5c168832eb65c22662984b63
#> 106 6cde5c2a5c168832eb65c22662984b63
#> 107 6cde5c2a5c168832eb65c22662984b63
#> 108 6cde5c2a5c168832eb65c22662984b63
#> 109 6cde5c2a5c168832eb65c22662984b63
#> 110 6cde5c2a5c168832eb65c22662984b63
#> 111 6cde5c2a5c168832eb65c22662984b63
#> 112 6cde5c2a5c168832eb65c22662984b63
#> 113 6cde5c2a5c168832eb65c22662984b63
#> 114 6cde5c2a5c168832eb65c22662984b63
#> 115 6cde5c2a5c168832eb65c22662984b63
#> 116 6cde5c2a5c168832eb65c22662984b63
#> 117 6cde5c2a5c168832eb65c22662984b63
#> 118 6cde5c2a5c168832eb65c22662984b63
#> 119 6cde5c2a5c168832eb65c22662984b63
#> 120 6cde5c2a5c168832eb65c22662984b63
#> 124 6cde5c2a5c168832eb65c22662984b63
#> 121 6cde5c2a5c168832eb65c22662984b63
#> 122 6cde5c2a5c168832eb65c22662984b63
#> 125 6cde5c2a5c168832eb65c22662984b63
#> 

## Stop parallel workers and disable journal logging and signaling
plan(sequential)
options(oopts)
```
