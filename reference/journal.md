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
#> 1         create   overhead   <NA> 2026-01-22 14:04:53 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-01-22 14:04:53 0.007701397 secs
#> 2      getWorker   overhead launch 2026-01-22 14:04:53 0.007827997 secs
#> 3    eraseWorker   overhead launch 2026-01-22 14:04:53 0.008989096 secs
#> 5       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.046030998 secs
#> 6       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.079614878 secs
#> 7       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.112993717 secs
#> 8       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.146475554 secs
#> 9       resolved       <NA>   <NA> 2026-01-22 14:04:54 0.180242777 secs
#> 42      evaluate evaluation   <NA> 2026-01-22 14:04:54 0.200117826 secs
#> 10      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.213913679 secs
#> 11      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.247920275 secs
#> 12      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.282315731 secs
#> 13      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.321346998 secs
#> 14      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.355668783 secs
#> 15      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.390400171 secs
#> 16      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.426722288 secs
#> 17      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.463300467 secs
#> 18      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.499892950 secs
#> 19      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.536648750 secs
#> 20      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.573940992 secs
#> 21      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.611943722 secs
#> 22      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.649174452 secs
#> 23      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.686213017 secs
#> 24      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.723458052 secs
#> 25      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.761112690 secs
#> 26      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.800209761 secs
#> 27      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.837920427 secs
#> 28      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.875991821 secs
#> 29      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.914366722 secs
#> 30      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.952555180 secs
#> 31      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.991349459 secs
#> 32      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.030778646 secs
#> 33      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.070014477 secs
#> 34      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.109707594 secs
#> 35      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.149197102 secs
#> 36      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.188924551 secs
#> 37      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.228784561 secs
#> 38      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.268786669 secs
#> 39      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.309331417 secs
#> 43      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.349638939 secs
#> 40 receiveResult   overhead gather 2026-01-22 14:04:55 1.350919247 secs
#> 41        gather   overhead   <NA> 2026-01-22 14:04:55 1.352777719 secs
#> 44      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.369877577 secs
#> 45       resolve   overhead   <NA> 2026-01-22 14:04:55 1.371018410 secs
#>             duration future_label                         future_uuid
#> 1  7.359266e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 4  5.131245e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 2  5.631447e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 3  3.364086e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 5  1.090407e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 6  1.072598e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 7  1.072812e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 8  1.083040e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 9  1.073885e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 42 1.129323e+00 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 10 1.073980e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 11 1.085448e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 12 1.495576e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 13 1.077247e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 14 1.071095e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 15 1.186943e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 16 1.123714e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 17 1.125836e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 18 1.115084e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 19 1.136613e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 20 1.125765e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 21 1.120114e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 22 1.125932e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 23 1.109767e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 24 1.157856e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 25 1.161098e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 26 1.121473e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 27 1.128054e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 28 1.113987e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 29 1.131439e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 30 1.136208e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 31 1.139379e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 32 1.143503e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 33 1.186109e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 34 1.143789e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 35 1.142764e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 36 1.129508e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 37 1.140308e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 38 1.146555e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 39 1.155400e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 43 7.817984e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 40 2.801418e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 41 1.246929e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 44 5.102158e-05 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#> 45 1.668930e-05 secs         <NA> e3fec43117aeffafcf59f8376e551302-17
#>                        session_uuid
#> 1  e3fec43117aeffafcf59f8376e551302
#> 4  e3fec43117aeffafcf59f8376e551302
#> 2  e3fec43117aeffafcf59f8376e551302
#> 3  e3fec43117aeffafcf59f8376e551302
#> 5  e3fec43117aeffafcf59f8376e551302
#> 6  e3fec43117aeffafcf59f8376e551302
#> 7  e3fec43117aeffafcf59f8376e551302
#> 8  e3fec43117aeffafcf59f8376e551302
#> 9  e3fec43117aeffafcf59f8376e551302
#> 42 e3fec43117aeffafcf59f8376e551302
#> 10 e3fec43117aeffafcf59f8376e551302
#> 11 e3fec43117aeffafcf59f8376e551302
#> 12 e3fec43117aeffafcf59f8376e551302
#> 13 e3fec43117aeffafcf59f8376e551302
#> 14 e3fec43117aeffafcf59f8376e551302
#> 15 e3fec43117aeffafcf59f8376e551302
#> 16 e3fec43117aeffafcf59f8376e551302
#> 17 e3fec43117aeffafcf59f8376e551302
#> 18 e3fec43117aeffafcf59f8376e551302
#> 19 e3fec43117aeffafcf59f8376e551302
#> 20 e3fec43117aeffafcf59f8376e551302
#> 21 e3fec43117aeffafcf59f8376e551302
#> 22 e3fec43117aeffafcf59f8376e551302
#> 23 e3fec43117aeffafcf59f8376e551302
#> 24 e3fec43117aeffafcf59f8376e551302
#> 25 e3fec43117aeffafcf59f8376e551302
#> 26 e3fec43117aeffafcf59f8376e551302
#> 27 e3fec43117aeffafcf59f8376e551302
#> 28 e3fec43117aeffafcf59f8376e551302
#> 29 e3fec43117aeffafcf59f8376e551302
#> 30 e3fec43117aeffafcf59f8376e551302
#> 31 e3fec43117aeffafcf59f8376e551302
#> 32 e3fec43117aeffafcf59f8376e551302
#> 33 e3fec43117aeffafcf59f8376e551302
#> 34 e3fec43117aeffafcf59f8376e551302
#> 35 e3fec43117aeffafcf59f8376e551302
#> 36 e3fec43117aeffafcf59f8376e551302
#> 37 e3fec43117aeffafcf59f8376e551302
#> 38 e3fec43117aeffafcf59f8376e551302
#> 39 e3fec43117aeffafcf59f8376e551302
#> 43 e3fec43117aeffafcf59f8376e551302
#> 40 e3fec43117aeffafcf59f8376e551302
#> 41 e3fec43117aeffafcf59f8376e551302
#> 44 e3fec43117aeffafcf59f8376e551302
#> 45 e3fec43117aeffafcf59f8376e551302
#> 
#> [[2]]
#>            event   category parent               start               at
#> 1         create   overhead   <NA> 2026-01-22 14:04:53 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-01-22 14:04:53 0.008237839 secs
#> 2      getWorker   overhead launch 2026-01-22 14:04:53 0.008369684 secs
#> 3    eraseWorker   overhead launch 2026-01-22 14:04:53 0.009540081 secs
#> 5       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.044297695 secs
#> 6       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.077705145 secs
#> 7       resolved       <NA>   <NA> 2026-01-22 14:04:53 0.111085415 secs
#> 8       resolved       <NA>   <NA> 2026-01-22 14:04:54 0.144713879 secs
#> 9       resolved       <NA>   <NA> 2026-01-22 14:04:54 0.178364277 secs
#> 71      evaluate evaluation   <NA> 2026-01-22 14:04:54 0.202781439 secs
#> 10      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.212060452 secs
#> 11      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.246261120 secs
#> 12      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.285027742 secs
#> 13      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.319603682 secs
#> 14      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.353763103 secs
#> 15      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.389721155 secs
#> 16      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.425877810 secs
#> 17      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.462540388 secs
#> 18      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.498984814 secs
#> 19      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.535998344 secs
#> 20      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.573347330 secs
#> 21      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.611033201 secs
#> 22      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.648298025 secs
#> 23      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.685141325 secs
#> 24      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.722684145 secs
#> 25      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.760740519 secs
#> 26      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.799404144 secs
#> 27      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.837246418 secs
#> 28      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.875306845 secs
#> 29      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.913605690 secs
#> 30      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.951893091 secs
#> 31      resolved       <NA>   <NA> 2026-01-22 14:04:54 0.990903854 secs
#> 32      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.030397177 secs
#> 33      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.070040464 secs
#> 34      resolved       <NA>   <NA> 2026-01-22 14:04:54 1.109224081 secs
#> 35      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.148689985 secs
#> 36      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.188385010 secs
#> 37      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.228440285 secs
#> 38      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.268444300 secs
#> 39      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.309067965 secs
#> 40      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.358128309 secs
#> 41      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.393018484 secs
#> 42      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.428468227 secs
#> 43      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.464390039 secs
#> 44      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.499889612 secs
#> 45      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.535615683 secs
#> 46      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.571448565 secs
#> 47      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.604844093 secs
#> 48      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.640401602 secs
#> 49      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.676140547 secs
#> 50      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.712179422 secs
#> 51      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.748151064 secs
#> 52      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.783723354 secs
#> 53      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.816982746 secs
#> 54      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.852220297 secs
#> 55      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.888459444 secs
#> 56      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.924061537 secs
#> 57      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.959937096 secs
#> 58      resolved       <NA>   <NA> 2026-01-22 14:04:55 1.995342016 secs
#> 59      resolved       <NA>   <NA> 2026-01-22 14:04:55 2.030724764 secs
#> 60      resolved       <NA>   <NA> 2026-01-22 14:04:55 2.065623760 secs
#> 61      resolved       <NA>   <NA> 2026-01-22 14:04:55 2.099312544 secs
#> 62      resolved       <NA>   <NA> 2026-01-22 14:04:55 2.131995916 secs
#> 63      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.164853811 secs
#> 64      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.197667360 secs
#> 65      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.230225086 secs
#> 66      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.262883425 secs
#> 67      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.297336817 secs
#> 68      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.330097198 secs
#> 72      resolved       <NA>   <NA> 2026-01-22 14:04:56 2.362774849 secs
#> 69 receiveResult   overhead gather 2026-01-22 14:04:56 2.363474131 secs
#> 70        gather   overhead   <NA> 2026-01-22 14:04:56 2.364778519 secs
#> 73       resolve   overhead   <NA> 2026-01-22 14:04:56 2.374812603 secs
#>             duration future_label                         future_uuid
#> 1  7.914305e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 4  5.202770e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 2  5.848408e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 3  3.910065e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 5  1.082373e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 6  1.070499e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 7  1.071715e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 8  1.067734e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 9  1.065683e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 71 2.143430e+00 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 10 1.076555e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 11 1.076317e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 12 1.101971e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 13 1.070905e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 14 1.070070e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 15 1.074910e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 16 1.102805e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 17 1.097393e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 18 1.107025e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 19 1.114631e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 20 1.132894e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 21 1.109290e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 22 1.100206e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 23 1.095152e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 24 1.079226e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 25 1.114416e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 26 1.098490e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 27 1.116276e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 28 1.126242e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 29 1.106930e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 30 1.125693e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 31 1.140261e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 32 1.123929e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 33 1.111174e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 34 1.113725e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 35 1.126432e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 36 1.122928e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 37 1.128316e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 38 1.146817e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 39 1.117897e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 40 1.074457e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 41 1.128125e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 42 1.135564e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 43 1.137853e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 44 1.137543e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 45 1.131344e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 46 1.095772e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 47 1.136065e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 48 1.138258e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 49 1.136708e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 50 1.147437e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 51 1.146078e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 52 1.099634e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 53 1.120019e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 54 1.178694e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 55 1.101637e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 56 1.143861e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 57 1.126504e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 58 1.130128e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 59 1.151609e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 60 1.066828e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 61 1.062584e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 62 1.065707e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 63 1.075935e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 64 1.064610e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 65 1.067162e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 66 1.073122e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 67 1.068020e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 68 1.066637e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 72 1.074290e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 69 1.974106e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 70 8.630753e-05 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#> 73 1.859665e-05 secs         <NA> e3fec43117aeffafcf59f8376e551302-18
#>                        session_uuid
#> 1  e3fec43117aeffafcf59f8376e551302
#> 4  e3fec43117aeffafcf59f8376e551302
#> 2  e3fec43117aeffafcf59f8376e551302
#> 3  e3fec43117aeffafcf59f8376e551302
#> 5  e3fec43117aeffafcf59f8376e551302
#> 6  e3fec43117aeffafcf59f8376e551302
#> 7  e3fec43117aeffafcf59f8376e551302
#> 8  e3fec43117aeffafcf59f8376e551302
#> 9  e3fec43117aeffafcf59f8376e551302
#> 71 e3fec43117aeffafcf59f8376e551302
#> 10 e3fec43117aeffafcf59f8376e551302
#> 11 e3fec43117aeffafcf59f8376e551302
#> 12 e3fec43117aeffafcf59f8376e551302
#> 13 e3fec43117aeffafcf59f8376e551302
#> 14 e3fec43117aeffafcf59f8376e551302
#> 15 e3fec43117aeffafcf59f8376e551302
#> 16 e3fec43117aeffafcf59f8376e551302
#> 17 e3fec43117aeffafcf59f8376e551302
#> 18 e3fec43117aeffafcf59f8376e551302
#> 19 e3fec43117aeffafcf59f8376e551302
#> 20 e3fec43117aeffafcf59f8376e551302
#> 21 e3fec43117aeffafcf59f8376e551302
#> 22 e3fec43117aeffafcf59f8376e551302
#> 23 e3fec43117aeffafcf59f8376e551302
#> 24 e3fec43117aeffafcf59f8376e551302
#> 25 e3fec43117aeffafcf59f8376e551302
#> 26 e3fec43117aeffafcf59f8376e551302
#> 27 e3fec43117aeffafcf59f8376e551302
#> 28 e3fec43117aeffafcf59f8376e551302
#> 29 e3fec43117aeffafcf59f8376e551302
#> 30 e3fec43117aeffafcf59f8376e551302
#> 31 e3fec43117aeffafcf59f8376e551302
#> 32 e3fec43117aeffafcf59f8376e551302
#> 33 e3fec43117aeffafcf59f8376e551302
#> 34 e3fec43117aeffafcf59f8376e551302
#> 35 e3fec43117aeffafcf59f8376e551302
#> 36 e3fec43117aeffafcf59f8376e551302
#> 37 e3fec43117aeffafcf59f8376e551302
#> 38 e3fec43117aeffafcf59f8376e551302
#> 39 e3fec43117aeffafcf59f8376e551302
#> 40 e3fec43117aeffafcf59f8376e551302
#> 41 e3fec43117aeffafcf59f8376e551302
#> 42 e3fec43117aeffafcf59f8376e551302
#> 43 e3fec43117aeffafcf59f8376e551302
#> 44 e3fec43117aeffafcf59f8376e551302
#> 45 e3fec43117aeffafcf59f8376e551302
#> 46 e3fec43117aeffafcf59f8376e551302
#> 47 e3fec43117aeffafcf59f8376e551302
#> 48 e3fec43117aeffafcf59f8376e551302
#> 49 e3fec43117aeffafcf59f8376e551302
#> 50 e3fec43117aeffafcf59f8376e551302
#> 51 e3fec43117aeffafcf59f8376e551302
#> 52 e3fec43117aeffafcf59f8376e551302
#> 53 e3fec43117aeffafcf59f8376e551302
#> 54 e3fec43117aeffafcf59f8376e551302
#> 55 e3fec43117aeffafcf59f8376e551302
#> 56 e3fec43117aeffafcf59f8376e551302
#> 57 e3fec43117aeffafcf59f8376e551302
#> 58 e3fec43117aeffafcf59f8376e551302
#> 59 e3fec43117aeffafcf59f8376e551302
#> 60 e3fec43117aeffafcf59f8376e551302
#> 61 e3fec43117aeffafcf59f8376e551302
#> 62 e3fec43117aeffafcf59f8376e551302
#> 63 e3fec43117aeffafcf59f8376e551302
#> 64 e3fec43117aeffafcf59f8376e551302
#> 65 e3fec43117aeffafcf59f8376e551302
#> 66 e3fec43117aeffafcf59f8376e551302
#> 67 e3fec43117aeffafcf59f8376e551302
#> 68 e3fec43117aeffafcf59f8376e551302
#> 72 e3fec43117aeffafcf59f8376e551302
#> 69 e3fec43117aeffafcf59f8376e551302
#> 70 e3fec43117aeffafcf59f8376e551302
#> 73 e3fec43117aeffafcf59f8376e551302
#> 
#> [[3]]
#>             event   category parent               start               at
#> 1          create   overhead   <NA> 2026-01-22 14:04:53 0.000000000 secs
#> 4          launch   overhead   <NA> 2026-01-22 14:04:53 0.008287430 secs
#> 2       getWorker   overhead launch 2026-01-22 14:04:53 0.008419275 secs
#> 3     eraseWorker   overhead launch 2026-01-22 14:04:55 1.334636688 secs
#> 5        resolved       <NA>   <NA> 2026-01-22 14:04:55 1.356327057 secs
#> 125      evaluate evaluation   <NA> 2026-01-22 14:04:55 1.382415295 secs
#> 6        resolved       <NA>   <NA> 2026-01-22 14:04:55 1.391815901 secs
#> 7        resolved       <NA>   <NA> 2026-01-22 14:04:55 1.427343607 secs
#> 8        resolved       <NA>   <NA> 2026-01-22 14:04:55 1.463204861 secs
#> 9        resolved       <NA>   <NA> 2026-01-22 14:04:55 1.498686552 secs
#> 10       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.534339666 secs
#> 11       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.569034576 secs
#> 12       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.603685856 secs
#> 13       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.639314890 secs
#> 14       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.675053358 secs
#> 15       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.711178780 secs
#> 16       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.747271776 secs
#> 17       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.781458855 secs
#> 18       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.815534353 secs
#> 19       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.851594210 secs
#> 20       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.887111425 secs
#> 21       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.923114061 secs
#> 22       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.958595991 secs
#> 23       resolved       <NA>   <NA> 2026-01-22 14:04:55 1.993714094 secs
#> 24       resolved       <NA>   <NA> 2026-01-22 14:04:55 2.029545069 secs
#> 25       resolved       <NA>   <NA> 2026-01-22 14:04:55 2.063385487 secs
#> 26       resolved       <NA>   <NA> 2026-01-22 14:04:55 2.096554041 secs
#> 27       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.129354477 secs
#> 28       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.162247419 secs
#> 29       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.194934845 secs
#> 30       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.227601290 secs
#> 31       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.260958672 secs
#> 32       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.294692039 secs
#> 33       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.327399492 secs
#> 34       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.361554146 secs
#> 35       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.383461475 secs
#> 36       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.406688213 secs
#> 37       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.429786444 secs
#> 38       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.452671051 secs
#> 39       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.475813150 secs
#> 40       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.499012709 secs
#> 41       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.521984339 secs
#> 42       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.545203209 secs
#> 43       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.568325996 secs
#> 44       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.591378212 secs
#> 45       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.614492416 secs
#> 46       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.637639523 secs
#> 47       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.660766363 secs
#> 48       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.683514833 secs
#> 49       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.706294060 secs
#> 50       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.729300737 secs
#> 51       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.752246380 secs
#> 52       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.775256634 secs
#> 53       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.798480272 secs
#> 54       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.821693659 secs
#> 55       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.844794035 secs
#> 56       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.867756605 secs
#> 57       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.890779495 secs
#> 58       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.913757563 secs
#> 59       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.935828924 secs
#> 60       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.958139896 secs
#> 61       resolved       <NA>   <NA> 2026-01-22 14:04:56 2.981189013 secs
#> 62       resolved       <NA>   <NA> 2026-01-22 14:04:56 3.004529953 secs
#> 63       resolved       <NA>   <NA> 2026-01-22 14:04:56 3.027751207 secs
#> 64       resolved       <NA>   <NA> 2026-01-22 14:04:56 3.050837278 secs
#> 65       resolved       <NA>   <NA> 2026-01-22 14:04:56 3.073858023 secs
#> 66       resolved       <NA>   <NA> 2026-01-22 14:04:56 3.097183704 secs
#> 67       resolved       <NA>   <NA> 2026-01-22 14:04:56 3.120723724 secs
#> 68       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.143636703 secs
#> 69       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.165019512 secs
#> 70       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.187737703 secs
#> 71       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.211131573 secs
#> 72       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.234809637 secs
#> 73       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.258823156 secs
#> 74       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.282779932 secs
#> 75       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.304861784 secs
#> 76       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.326513290 secs
#> 77       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.348044157 secs
#> 78       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.370413542 secs
#> 79       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.393518209 secs
#> 80       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.416815996 secs
#> 81       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.439916849 secs
#> 82       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.462302923 secs
#> 83       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.484236240 secs
#> 84       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.507179737 secs
#> 85       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.530160189 secs
#> 86       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.553181887 secs
#> 87       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.576224089 secs
#> 88       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.599116325 secs
#> 89       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.622169495 secs
#> 90       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.645411015 secs
#> 91       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.667766571 secs
#> 92       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.689507246 secs
#> 93       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.712443352 secs
#> 94       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.735338449 secs
#> 95       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.758325577 secs
#> 96       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.781106472 secs
#> 97       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.804136992 secs
#> 98       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.827477217 secs
#> 99       resolved       <NA>   <NA> 2026-01-22 14:04:57 3.850584745 secs
#> 100      resolved       <NA>   <NA> 2026-01-22 14:04:57 3.873863220 secs
#> 101      resolved       <NA>   <NA> 2026-01-22 14:04:57 3.897046566 secs
#> 102      resolved       <NA>   <NA> 2026-01-22 14:04:57 3.920303345 secs
#> 103      resolved       <NA>   <NA> 2026-01-22 14:04:57 3.943518400 secs
#> 104      resolved       <NA>   <NA> 2026-01-22 14:04:57 3.966540813 secs
#> 105      resolved       <NA>   <NA> 2026-01-22 14:04:57 3.990035534 secs
#> 106      resolved       <NA>   <NA> 2026-01-22 14:04:57 4.011812687 secs
#> 107      resolved       <NA>   <NA> 2026-01-22 14:04:57 4.033301592 secs
#> 108      resolved       <NA>   <NA> 2026-01-22 14:04:57 4.055878401 secs
#> 109      resolved       <NA>   <NA> 2026-01-22 14:04:57 4.079496622 secs
#> 110      resolved       <NA>   <NA> 2026-01-22 14:04:57 4.102637768 secs
#> 111      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.125814676 secs
#> 112      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.148958921 secs
#> 113      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.171824932 secs
#> 114      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.194971085 secs
#> 115      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.217884302 secs
#> 116      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.240878105 secs
#> 117      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.263936758 secs
#> 118      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.286872149 secs
#> 119      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.310258389 secs
#> 120      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.333389521 secs
#> 121      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.356736660 secs
#> 122      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.379624367 secs
#> 126      resolved       <NA>   <NA> 2026-01-22 14:04:58 4.402734280 secs
#> 123 receiveResult   overhead gather 2026-01-22 14:04:58 4.403984547 secs
#> 124        gather   overhead   <NA> 2026-01-22 14:04:58 4.405903816 secs
#> 127       resolve   overhead   <NA> 2026-01-22 14:04:58 4.413202763 secs
#>              duration future_label                         future_uuid
#> 1   7.955551e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 4   1.332919e+00 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 2   1.324960e+00 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 3   7.598400e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 5   1.121521e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 125 3.008833e+00 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 6   1.120710e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 7   1.129079e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 8   1.109815e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 9   1.125264e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 10  1.120186e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 11  1.061893e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 12  1.120472e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 13  1.115346e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 14  1.130414e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 15  1.126504e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 16  1.122212e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 17  1.064730e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 18  1.138568e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 19  1.133704e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 20  1.110220e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 21  1.117945e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 22  1.099682e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 23  1.111364e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 24  1.106477e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 25  1.113176e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 26  1.063108e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 27  1.059318e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 28  1.058722e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 29  1.057005e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 30  1.058817e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 31  1.112199e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 32  1.064539e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 33  1.058674e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 34  1.061034e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 35  1.147509e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 36  1.145148e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 37  1.128864e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 38  1.136684e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 39  1.143909e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 40  1.135230e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 41  1.142788e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 42  1.137686e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 43  1.130700e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 44  1.145339e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 45  1.148510e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 46  1.147056e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 47  1.128268e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 48  1.114917e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 49  1.132083e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 50  1.126099e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 51  1.136088e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 52  1.147008e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 53  1.145840e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 54  1.129961e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 55  1.137257e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 56  1.132631e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 57  1.140666e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 58  1.108718e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 59  1.100302e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 60  1.127887e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 61  1.153135e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 62  1.152825e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 63  1.135850e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 64  1.138043e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 65  1.151013e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 66  1.140594e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 67  1.137757e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 68  1.062846e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 69  1.099229e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 70  1.155734e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 71  1.202822e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 72  1.123714e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 73  1.160765e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 74  1.136589e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 75  1.070452e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 76  1.065612e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 77  1.100111e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 78  1.122808e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 79  1.151085e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 80  1.141143e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 81  1.108193e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 82  1.062846e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 83  1.140833e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 84  1.125264e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 85  1.127315e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 86  1.143885e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 87  1.119614e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 88  1.135325e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 89  1.195359e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 90  1.114249e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 91  1.091766e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 92  1.095438e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 93  1.135182e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 94  1.129580e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 95  1.096940e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 96  1.136494e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 97  1.131344e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 98  1.139545e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 99  1.163554e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 100 1.140213e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 101 1.159382e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 102 1.137543e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 103 1.140404e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 104 1.143861e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 105 1.081514e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 106 1.075411e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 107 1.104951e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 108 1.134396e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 109 1.139545e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 110 1.138902e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 111 1.140094e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 112 1.131082e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 113 1.121569e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 114 1.127791e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 115 1.136541e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 116 1.142836e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 117 1.136374e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 118 1.133585e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 119 1.132417e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 120 1.179647e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 121 1.119065e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 122 1.139021e-02 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 126 8.269787e-03 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 123 2.858639e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 124 1.337528e-04 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#> 127 3.218651e-05 secs         <NA> e3fec43117aeffafcf59f8376e551302-19
#>                         session_uuid
#> 1   e3fec43117aeffafcf59f8376e551302
#> 4   e3fec43117aeffafcf59f8376e551302
#> 2   e3fec43117aeffafcf59f8376e551302
#> 3   e3fec43117aeffafcf59f8376e551302
#> 5   e3fec43117aeffafcf59f8376e551302
#> 125 e3fec43117aeffafcf59f8376e551302
#> 6   e3fec43117aeffafcf59f8376e551302
#> 7   e3fec43117aeffafcf59f8376e551302
#> 8   e3fec43117aeffafcf59f8376e551302
#> 9   e3fec43117aeffafcf59f8376e551302
#> 10  e3fec43117aeffafcf59f8376e551302
#> 11  e3fec43117aeffafcf59f8376e551302
#> 12  e3fec43117aeffafcf59f8376e551302
#> 13  e3fec43117aeffafcf59f8376e551302
#> 14  e3fec43117aeffafcf59f8376e551302
#> 15  e3fec43117aeffafcf59f8376e551302
#> 16  e3fec43117aeffafcf59f8376e551302
#> 17  e3fec43117aeffafcf59f8376e551302
#> 18  e3fec43117aeffafcf59f8376e551302
#> 19  e3fec43117aeffafcf59f8376e551302
#> 20  e3fec43117aeffafcf59f8376e551302
#> 21  e3fec43117aeffafcf59f8376e551302
#> 22  e3fec43117aeffafcf59f8376e551302
#> 23  e3fec43117aeffafcf59f8376e551302
#> 24  e3fec43117aeffafcf59f8376e551302
#> 25  e3fec43117aeffafcf59f8376e551302
#> 26  e3fec43117aeffafcf59f8376e551302
#> 27  e3fec43117aeffafcf59f8376e551302
#> 28  e3fec43117aeffafcf59f8376e551302
#> 29  e3fec43117aeffafcf59f8376e551302
#> 30  e3fec43117aeffafcf59f8376e551302
#> 31  e3fec43117aeffafcf59f8376e551302
#> 32  e3fec43117aeffafcf59f8376e551302
#> 33  e3fec43117aeffafcf59f8376e551302
#> 34  e3fec43117aeffafcf59f8376e551302
#> 35  e3fec43117aeffafcf59f8376e551302
#> 36  e3fec43117aeffafcf59f8376e551302
#> 37  e3fec43117aeffafcf59f8376e551302
#> 38  e3fec43117aeffafcf59f8376e551302
#> 39  e3fec43117aeffafcf59f8376e551302
#> 40  e3fec43117aeffafcf59f8376e551302
#> 41  e3fec43117aeffafcf59f8376e551302
#> 42  e3fec43117aeffafcf59f8376e551302
#> 43  e3fec43117aeffafcf59f8376e551302
#> 44  e3fec43117aeffafcf59f8376e551302
#> 45  e3fec43117aeffafcf59f8376e551302
#> 46  e3fec43117aeffafcf59f8376e551302
#> 47  e3fec43117aeffafcf59f8376e551302
#> 48  e3fec43117aeffafcf59f8376e551302
#> 49  e3fec43117aeffafcf59f8376e551302
#> 50  e3fec43117aeffafcf59f8376e551302
#> 51  e3fec43117aeffafcf59f8376e551302
#> 52  e3fec43117aeffafcf59f8376e551302
#> 53  e3fec43117aeffafcf59f8376e551302
#> 54  e3fec43117aeffafcf59f8376e551302
#> 55  e3fec43117aeffafcf59f8376e551302
#> 56  e3fec43117aeffafcf59f8376e551302
#> 57  e3fec43117aeffafcf59f8376e551302
#> 58  e3fec43117aeffafcf59f8376e551302
#> 59  e3fec43117aeffafcf59f8376e551302
#> 60  e3fec43117aeffafcf59f8376e551302
#> 61  e3fec43117aeffafcf59f8376e551302
#> 62  e3fec43117aeffafcf59f8376e551302
#> 63  e3fec43117aeffafcf59f8376e551302
#> 64  e3fec43117aeffafcf59f8376e551302
#> 65  e3fec43117aeffafcf59f8376e551302
#> 66  e3fec43117aeffafcf59f8376e551302
#> 67  e3fec43117aeffafcf59f8376e551302
#> 68  e3fec43117aeffafcf59f8376e551302
#> 69  e3fec43117aeffafcf59f8376e551302
#> 70  e3fec43117aeffafcf59f8376e551302
#> 71  e3fec43117aeffafcf59f8376e551302
#> 72  e3fec43117aeffafcf59f8376e551302
#> 73  e3fec43117aeffafcf59f8376e551302
#> 74  e3fec43117aeffafcf59f8376e551302
#> 75  e3fec43117aeffafcf59f8376e551302
#> 76  e3fec43117aeffafcf59f8376e551302
#> 77  e3fec43117aeffafcf59f8376e551302
#> 78  e3fec43117aeffafcf59f8376e551302
#> 79  e3fec43117aeffafcf59f8376e551302
#> 80  e3fec43117aeffafcf59f8376e551302
#> 81  e3fec43117aeffafcf59f8376e551302
#> 82  e3fec43117aeffafcf59f8376e551302
#> 83  e3fec43117aeffafcf59f8376e551302
#> 84  e3fec43117aeffafcf59f8376e551302
#> 85  e3fec43117aeffafcf59f8376e551302
#> 86  e3fec43117aeffafcf59f8376e551302
#> 87  e3fec43117aeffafcf59f8376e551302
#> 88  e3fec43117aeffafcf59f8376e551302
#> 89  e3fec43117aeffafcf59f8376e551302
#> 90  e3fec43117aeffafcf59f8376e551302
#> 91  e3fec43117aeffafcf59f8376e551302
#> 92  e3fec43117aeffafcf59f8376e551302
#> 93  e3fec43117aeffafcf59f8376e551302
#> 94  e3fec43117aeffafcf59f8376e551302
#> 95  e3fec43117aeffafcf59f8376e551302
#> 96  e3fec43117aeffafcf59f8376e551302
#> 97  e3fec43117aeffafcf59f8376e551302
#> 98  e3fec43117aeffafcf59f8376e551302
#> 99  e3fec43117aeffafcf59f8376e551302
#> 100 e3fec43117aeffafcf59f8376e551302
#> 101 e3fec43117aeffafcf59f8376e551302
#> 102 e3fec43117aeffafcf59f8376e551302
#> 103 e3fec43117aeffafcf59f8376e551302
#> 104 e3fec43117aeffafcf59f8376e551302
#> 105 e3fec43117aeffafcf59f8376e551302
#> 106 e3fec43117aeffafcf59f8376e551302
#> 107 e3fec43117aeffafcf59f8376e551302
#> 108 e3fec43117aeffafcf59f8376e551302
#> 109 e3fec43117aeffafcf59f8376e551302
#> 110 e3fec43117aeffafcf59f8376e551302
#> 111 e3fec43117aeffafcf59f8376e551302
#> 112 e3fec43117aeffafcf59f8376e551302
#> 113 e3fec43117aeffafcf59f8376e551302
#> 114 e3fec43117aeffafcf59f8376e551302
#> 115 e3fec43117aeffafcf59f8376e551302
#> 116 e3fec43117aeffafcf59f8376e551302
#> 117 e3fec43117aeffafcf59f8376e551302
#> 118 e3fec43117aeffafcf59f8376e551302
#> 119 e3fec43117aeffafcf59f8376e551302
#> 120 e3fec43117aeffafcf59f8376e551302
#> 121 e3fec43117aeffafcf59f8376e551302
#> 122 e3fec43117aeffafcf59f8376e551302
#> 126 e3fec43117aeffafcf59f8376e551302
#> 123 e3fec43117aeffafcf59f8376e551302
#> 124 e3fec43117aeffafcf59f8376e551302
#> 127 e3fec43117aeffafcf59f8376e551302
#> 

## Stop parallel workers and disable journal logging and signaling
plan(sequential)
options(oopts)
```
