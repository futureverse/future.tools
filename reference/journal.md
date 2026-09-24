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
#> 1         create   overhead   <NA> 2026-09-23 19:29:12 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-09-23 19:29:12 0.005189657 secs
#> 2      getWorker   overhead launch 2026-09-23 19:29:12 0.005282879 secs
#> 3    eraseWorker   overhead launch 2026-09-23 19:29:12 0.006197691 secs
#> 5       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.034527302 secs
#> 6       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.067332029 secs
#> 7       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.100176096 secs
#> 8       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.133128643 secs
#> 9       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.166498423 secs
#> 10      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.199799776 secs
#> 39      evaluate evaluation   <NA> 2026-09-23 19:29:12 0.215856791 secs
#> 11      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.233194113 secs
#> 12      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.266817093 secs
#> 13      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.302908182 secs
#> 14      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.341014385 secs
#> 15      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.378512383 secs
#> 16      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.416159868 secs
#> 17      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.454181671 secs
#> 18      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.492519855 secs
#> 19      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.530823708 secs
#> 20      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.569323301 secs
#> 21      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.608314991 secs
#> 22      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.646578074 secs
#> 23      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.685166597 secs
#> 24      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.731318712 secs
#> 25      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.770634174 secs
#> 26      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.810048103 secs
#> 27      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.849606037 secs
#> 28      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.889215469 secs
#> 29      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.929041862 secs
#> 30      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.968853712 secs
#> 31      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.008763075 secs
#> 32      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.046653509 secs
#> 33      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.086634159 secs
#> 34      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.127423286 secs
#> 35      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.167901754 secs
#> 36      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.208556414 secs
#> 40      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.249511719 secs
#> 37 receiveResult   overhead gather 2026-09-23 19:29:13 1.250564814 secs
#> 38        gather   overhead   <NA> 2026-09-23 19:29:13 1.252093792 secs
#> 41      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.260554552 secs
#> 42       resolve   overhead   <NA> 2026-09-23 19:29:13 1.261265993 secs
#>             duration future_label                         future_uuid
#> 1  4.925966e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 4  3.935575e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 2  4.611015e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 3  2.739429e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 5  1.071882e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 6  1.066756e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 7  1.069570e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 8  1.068306e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 9  1.079154e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 10 1.073027e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 39 1.031532e+00 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 11 1.067805e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 12 1.074100e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 13 1.162171e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 14 1.153922e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 15 1.145244e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 16 1.144552e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 17 1.156616e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 18 1.150751e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 19 1.159358e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 20 1.173186e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 21 1.161313e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 22 1.142836e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 23 1.159286e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 24 1.169395e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 25 1.162577e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 26 1.165771e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 27 1.169682e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 28 1.163268e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 29 1.164603e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 30 1.162219e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 31 1.149392e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 32 1.160121e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 33 1.177835e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 34 1.164770e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 35 1.167202e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 36 1.168537e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 40 5.124331e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 37 2.367496e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 38 9.894371e-05 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 41 3.480911e-05 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#> 42 9.536743e-06 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-17
#>                        session_uuid
#> 1  2429a1a68beabacff0e26fae8952a02a
#> 4  2429a1a68beabacff0e26fae8952a02a
#> 2  2429a1a68beabacff0e26fae8952a02a
#> 3  2429a1a68beabacff0e26fae8952a02a
#> 5  2429a1a68beabacff0e26fae8952a02a
#> 6  2429a1a68beabacff0e26fae8952a02a
#> 7  2429a1a68beabacff0e26fae8952a02a
#> 8  2429a1a68beabacff0e26fae8952a02a
#> 9  2429a1a68beabacff0e26fae8952a02a
#> 10 2429a1a68beabacff0e26fae8952a02a
#> 39 2429a1a68beabacff0e26fae8952a02a
#> 11 2429a1a68beabacff0e26fae8952a02a
#> 12 2429a1a68beabacff0e26fae8952a02a
#> 13 2429a1a68beabacff0e26fae8952a02a
#> 14 2429a1a68beabacff0e26fae8952a02a
#> 15 2429a1a68beabacff0e26fae8952a02a
#> 16 2429a1a68beabacff0e26fae8952a02a
#> 17 2429a1a68beabacff0e26fae8952a02a
#> 18 2429a1a68beabacff0e26fae8952a02a
#> 19 2429a1a68beabacff0e26fae8952a02a
#> 20 2429a1a68beabacff0e26fae8952a02a
#> 21 2429a1a68beabacff0e26fae8952a02a
#> 22 2429a1a68beabacff0e26fae8952a02a
#> 23 2429a1a68beabacff0e26fae8952a02a
#> 24 2429a1a68beabacff0e26fae8952a02a
#> 25 2429a1a68beabacff0e26fae8952a02a
#> 26 2429a1a68beabacff0e26fae8952a02a
#> 27 2429a1a68beabacff0e26fae8952a02a
#> 28 2429a1a68beabacff0e26fae8952a02a
#> 29 2429a1a68beabacff0e26fae8952a02a
#> 30 2429a1a68beabacff0e26fae8952a02a
#> 31 2429a1a68beabacff0e26fae8952a02a
#> 32 2429a1a68beabacff0e26fae8952a02a
#> 33 2429a1a68beabacff0e26fae8952a02a
#> 34 2429a1a68beabacff0e26fae8952a02a
#> 35 2429a1a68beabacff0e26fae8952a02a
#> 36 2429a1a68beabacff0e26fae8952a02a
#> 40 2429a1a68beabacff0e26fae8952a02a
#> 37 2429a1a68beabacff0e26fae8952a02a
#> 38 2429a1a68beabacff0e26fae8952a02a
#> 41 2429a1a68beabacff0e26fae8952a02a
#> 42 2429a1a68beabacff0e26fae8952a02a
#> 
#> [[2]]
#>            event   category parent               start               at
#> 1         create   overhead   <NA> 2026-09-23 19:29:12 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-09-23 19:29:12 0.005121231 secs
#> 2      getWorker   overhead launch 2026-09-23 19:29:12 0.005222797 secs
#> 3    eraseWorker   overhead launch 2026-09-23 19:29:12 0.006132364 secs
#> 5       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.036230803 secs
#> 6       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.069025517 secs
#> 7       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.101892471 secs
#> 8       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.134983540 secs
#> 9       resolved       <NA>   <NA> 2026-09-23 19:29:12 0.168306112 secs
#> 10      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.201632500 secs
#> 67      evaluate evaluation   <NA> 2026-09-23 19:29:12 0.216477156 secs
#> 11      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.234869719 secs
#> 12      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.269200563 secs
#> 13      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.306705713 secs
#> 14      resolved       <NA>   <NA> 2026-09-23 19:29:12 0.344535351 secs
#> 15      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.381982565 secs
#> 16      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.419941187 secs
#> 17      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.457933903 secs
#> 18      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.496155739 secs
#> 19      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.534450531 secs
#> 20      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.573184490 secs
#> 21      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.611962318 secs
#> 22      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.650066376 secs
#> 23      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.695960045 secs
#> 24      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.735110760 secs
#> 25      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.774371147 secs
#> 26      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.813837051 secs
#> 27      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.853428125 secs
#> 28      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.892967224 secs
#> 29      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.932805538 secs
#> 30      resolved       <NA>   <NA> 2026-09-23 19:29:13 0.972664595 secs
#> 31      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.011313438 secs
#> 32      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.050236464 secs
#> 33      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.091078520 secs
#> 34      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.131181717 secs
#> 35      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.171765327 secs
#> 36      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.212367296 secs
#> 37      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.252093077 secs
#> 38      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.285732031 secs
#> 39      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.322024107 secs
#> 40      resolved       <NA>   <NA> 2026-09-23 19:29:13 1.359432697 secs
#> 41      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.396811247 secs
#> 42      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.433037758 secs
#> 43      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.469310522 secs
#> 44      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.505763054 secs
#> 45      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.542641163 secs
#> 46      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.580088854 secs
#> 47      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.616865873 secs
#> 48      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.652889729 secs
#> 49      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.689242363 secs
#> 50      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.725898027 secs
#> 51      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.761088371 secs
#> 52      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.794816971 secs
#> 53      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.831578970 secs
#> 54      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.868570328 secs
#> 55      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.905454159 secs
#> 56      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.942080259 secs
#> 57      resolved       <NA>   <NA> 2026-09-23 19:29:14 1.978668213 secs
#> 58      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.015696526 secs
#> 59      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.052435875 secs
#> 60      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.087055445 secs
#> 61      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.122782230 secs
#> 62      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.155596972 secs
#> 63      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.188045979 secs
#> 64      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.220576286 secs
#> 68      resolved       <NA>   <NA> 2026-09-23 19:29:14 2.253209114 secs
#> 65 receiveResult   overhead gather 2026-09-23 19:29:14 2.253745079 secs
#> 66        gather   overhead   <NA> 2026-09-23 19:29:14 2.254496336 secs
#> 69       resolve   overhead   <NA> 2026-09-23 19:29:14 2.257151842 secs
#>             duration future_label                         future_uuid
#> 1  4.880667e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 4  3.949881e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 2  4.982948e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 3  2.341270e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 5  1.068044e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 6  1.056242e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 7  1.054454e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 8  1.063108e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 9  1.060057e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 10 1.059842e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 67 2.027854e+00 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 11 1.067519e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 12 1.117468e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 13 1.149464e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 14 1.118565e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 15 1.145673e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 16 1.155210e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 17 1.143074e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 18 1.141334e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 19 1.138735e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 20 1.144576e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 21 1.128411e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 22 1.141500e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 23 1.160026e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 24 1.147580e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 25 1.148033e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 26 1.146913e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 27 1.137018e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 28 1.151323e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 29 1.151156e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 30 1.146007e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 31 1.066351e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 32 1.142907e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 33 1.136374e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 34 1.150990e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 35 1.148677e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 36 1.144934e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 37 1.058412e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 38 1.160693e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 39 1.191998e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 40 1.160502e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 41 1.171088e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 42 1.145887e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 43 1.158094e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 44 1.168323e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 45 1.170397e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 46 1.161098e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 47 1.151872e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 48 1.157475e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 49 1.170111e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 50 1.164484e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 51 1.073885e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 52 1.161766e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 53 1.163530e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 54 1.181245e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 55 1.165366e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 56 1.158619e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 57 1.164079e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 58 1.167345e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 59 1.186752e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 60 1.136780e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 61 1.067591e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 62 1.064515e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 63 1.061654e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 64 1.066828e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 68 3.205538e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 65 1.177788e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 66 5.865097e-05 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#> 69 1.263618e-05 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-18
#>                        session_uuid
#> 1  2429a1a68beabacff0e26fae8952a02a
#> 4  2429a1a68beabacff0e26fae8952a02a
#> 2  2429a1a68beabacff0e26fae8952a02a
#> 3  2429a1a68beabacff0e26fae8952a02a
#> 5  2429a1a68beabacff0e26fae8952a02a
#> 6  2429a1a68beabacff0e26fae8952a02a
#> 7  2429a1a68beabacff0e26fae8952a02a
#> 8  2429a1a68beabacff0e26fae8952a02a
#> 9  2429a1a68beabacff0e26fae8952a02a
#> 10 2429a1a68beabacff0e26fae8952a02a
#> 67 2429a1a68beabacff0e26fae8952a02a
#> 11 2429a1a68beabacff0e26fae8952a02a
#> 12 2429a1a68beabacff0e26fae8952a02a
#> 13 2429a1a68beabacff0e26fae8952a02a
#> 14 2429a1a68beabacff0e26fae8952a02a
#> 15 2429a1a68beabacff0e26fae8952a02a
#> 16 2429a1a68beabacff0e26fae8952a02a
#> 17 2429a1a68beabacff0e26fae8952a02a
#> 18 2429a1a68beabacff0e26fae8952a02a
#> 19 2429a1a68beabacff0e26fae8952a02a
#> 20 2429a1a68beabacff0e26fae8952a02a
#> 21 2429a1a68beabacff0e26fae8952a02a
#> 22 2429a1a68beabacff0e26fae8952a02a
#> 23 2429a1a68beabacff0e26fae8952a02a
#> 24 2429a1a68beabacff0e26fae8952a02a
#> 25 2429a1a68beabacff0e26fae8952a02a
#> 26 2429a1a68beabacff0e26fae8952a02a
#> 27 2429a1a68beabacff0e26fae8952a02a
#> 28 2429a1a68beabacff0e26fae8952a02a
#> 29 2429a1a68beabacff0e26fae8952a02a
#> 30 2429a1a68beabacff0e26fae8952a02a
#> 31 2429a1a68beabacff0e26fae8952a02a
#> 32 2429a1a68beabacff0e26fae8952a02a
#> 33 2429a1a68beabacff0e26fae8952a02a
#> 34 2429a1a68beabacff0e26fae8952a02a
#> 35 2429a1a68beabacff0e26fae8952a02a
#> 36 2429a1a68beabacff0e26fae8952a02a
#> 37 2429a1a68beabacff0e26fae8952a02a
#> 38 2429a1a68beabacff0e26fae8952a02a
#> 39 2429a1a68beabacff0e26fae8952a02a
#> 40 2429a1a68beabacff0e26fae8952a02a
#> 41 2429a1a68beabacff0e26fae8952a02a
#> 42 2429a1a68beabacff0e26fae8952a02a
#> 43 2429a1a68beabacff0e26fae8952a02a
#> 44 2429a1a68beabacff0e26fae8952a02a
#> 45 2429a1a68beabacff0e26fae8952a02a
#> 46 2429a1a68beabacff0e26fae8952a02a
#> 47 2429a1a68beabacff0e26fae8952a02a
#> 48 2429a1a68beabacff0e26fae8952a02a
#> 49 2429a1a68beabacff0e26fae8952a02a
#> 50 2429a1a68beabacff0e26fae8952a02a
#> 51 2429a1a68beabacff0e26fae8952a02a
#> 52 2429a1a68beabacff0e26fae8952a02a
#> 53 2429a1a68beabacff0e26fae8952a02a
#> 54 2429a1a68beabacff0e26fae8952a02a
#> 55 2429a1a68beabacff0e26fae8952a02a
#> 56 2429a1a68beabacff0e26fae8952a02a
#> 57 2429a1a68beabacff0e26fae8952a02a
#> 58 2429a1a68beabacff0e26fae8952a02a
#> 59 2429a1a68beabacff0e26fae8952a02a
#> 60 2429a1a68beabacff0e26fae8952a02a
#> 61 2429a1a68beabacff0e26fae8952a02a
#> 62 2429a1a68beabacff0e26fae8952a02a
#> 63 2429a1a68beabacff0e26fae8952a02a
#> 64 2429a1a68beabacff0e26fae8952a02a
#> 68 2429a1a68beabacff0e26fae8952a02a
#> 65 2429a1a68beabacff0e26fae8952a02a
#> 66 2429a1a68beabacff0e26fae8952a02a
#> 69 2429a1a68beabacff0e26fae8952a02a
#> 
#> [[3]]
#>             event   category parent               start               at
#> 1          create   overhead   <NA> 2026-09-23 19:29:12 0.000000000 secs
#> 4          launch   overhead   <NA> 2026-09-23 19:29:12 0.005146265 secs
#> 2       getWorker   overhead launch 2026-09-23 19:29:12 0.005247116 secs
#> 3     eraseWorker   overhead launch 2026-09-23 19:29:13 1.237490416 secs
#> 121      evaluate evaluation   <NA> 2026-09-23 19:29:13 1.242403984 secs
#> 5        resolved       <NA>   <NA> 2026-09-23 19:29:13 1.253822327 secs
#> 6        resolved       <NA>   <NA> 2026-09-23 19:29:13 1.289381504 secs
#> 7        resolved       <NA>   <NA> 2026-09-23 19:29:13 1.326094389 secs
#> 8        resolved       <NA>   <NA> 2026-09-23 19:29:14 1.363650084 secs
#> 9        resolved       <NA>   <NA> 2026-09-23 19:29:14 1.400504351 secs
#> 10       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.436503887 secs
#> 11       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.473046064 secs
#> 12       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.509720325 secs
#> 13       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.547404289 secs
#> 14       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.583800316 secs
#> 15       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.620555639 secs
#> 16       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.656475306 secs
#> 17       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.692978621 secs
#> 18       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.729513168 secs
#> 19       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.762916327 secs
#> 20       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.798575401 secs
#> 21       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.835521936 secs
#> 22       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.872537613 secs
#> 23       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.909246206 secs
#> 24       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.945852518 secs
#> 25       resolved       <NA>   <NA> 2026-09-23 19:29:14 1.982830048 secs
#> 26       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.019519567 secs
#> 27       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.055718184 secs
#> 28       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.090674639 secs
#> 29       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.124559879 secs
#> 30       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.157305241 secs
#> 31       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.189729452 secs
#> 32       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.222335339 secs
#> 33       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.248092890 secs
#> 34       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.269432068 secs
#> 35       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.291077852 secs
#> 36       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.312700510 secs
#> 37       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.334218502 secs
#> 38       resolved       <NA>   <NA> 2026-09-23 19:29:14 2.355554581 secs
#> 39       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.376972198 secs
#> 40       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.398667097 secs
#> 41       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.420098305 secs
#> 42       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.442564726 secs
#> 43       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.466024399 secs
#> 44       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.489405155 secs
#> 45       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.512678146 secs
#> 46       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.536112547 secs
#> 47       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.559681892 secs
#> 48       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.583140612 secs
#> 49       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.606344938 secs
#> 50       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.629699230 secs
#> 51       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.652950048 secs
#> 52       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.676249743 secs
#> 53       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.699824333 secs
#> 54       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.723397970 secs
#> 55       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.746771097 secs
#> 56       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.769882441 secs
#> 57       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.793164968 secs
#> 58       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.816729546 secs
#> 59       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.847532511 secs
#> 60       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.870945215 secs
#> 61       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.894587040 secs
#> 62       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.918210745 secs
#> 63       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.941748142 secs
#> 64       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.965245008 secs
#> 65       resolved       <NA>   <NA> 2026-09-23 19:29:15 2.988853931 secs
#> 66       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.012265205 secs
#> 67       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.034497738 secs
#> 68       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.056749582 secs
#> 69       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.080217600 secs
#> 70       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.103899717 secs
#> 71       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.127311468 secs
#> 72       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.150744200 secs
#> 73       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.174093962 secs
#> 74       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.197394133 secs
#> 75       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.221032858 secs
#> 76       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.244481564 secs
#> 77       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.268059969 secs
#> 78       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.291676998 secs
#> 79       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.315183878 secs
#> 80       resolved       <NA>   <NA> 2026-09-23 19:29:15 3.338808298 secs
#> 81       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.362449408 secs
#> 82       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.385735512 secs
#> 83       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.409157753 secs
#> 84       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.433034658 secs
#> 85       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.456583500 secs
#> 86       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.480074167 secs
#> 87       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.503645897 secs
#> 88       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.527215958 secs
#> 89       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.550761461 secs
#> 90       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.574632645 secs
#> 91       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.598316193 secs
#> 92       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.621902943 secs
#> 93       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.645329714 secs
#> 94       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.668486357 secs
#> 95       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.691808939 secs
#> 96       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.715121746 secs
#> 97       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.738852739 secs
#> 98       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.762370110 secs
#> 99       resolved       <NA>   <NA> 2026-09-23 19:29:16 3.785724401 secs
#> 100      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.809421778 secs
#> 101      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.833161116 secs
#> 102      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.856906891 secs
#> 103      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.880515337 secs
#> 104      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.904160500 secs
#> 105      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.927765846 secs
#> 106      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.951760292 secs
#> 107      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.975367785 secs
#> 108      resolved       <NA>   <NA> 2026-09-23 19:29:16 3.999147654 secs
#> 109      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.022915840 secs
#> 110      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.046679735 secs
#> 111      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.070210218 secs
#> 112      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.093923092 secs
#> 113      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.117639780 secs
#> 114      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.141255140 secs
#> 115      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.164969206 secs
#> 116      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.188775539 secs
#> 117      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.212347984 secs
#> 118      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.235852003 secs
#> 122      resolved       <NA>   <NA> 2026-09-23 19:29:16 4.259580851 secs
#> 119 receiveResult   overhead gather 2026-09-23 19:29:16 4.260990381 secs
#> 120        gather   overhead   <NA> 2026-09-23 19:29:16 4.263136148 secs
#> 123       resolve   overhead   <NA> 2026-09-23 19:29:16 4.270416975 secs
#>              duration future_label                         future_uuid
#> 1   4.913568e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 4   1.235490e+00 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 2   1.231814e+00 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 3   3.006458e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 121 3.007935e+00 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 5   1.095223e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 6   1.137590e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 7   1.133490e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 8   1.190329e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 9   1.131082e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 10  1.141715e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 11  1.133776e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 12  1.146555e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 13  1.135278e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 14  1.148081e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 15  1.124477e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 16  1.137781e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 17  1.144981e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 18  1.122069e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 19  1.059175e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 20  1.149297e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 21  1.161766e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 22  1.145911e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 23  1.137042e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 24  1.135468e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 25  1.144195e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 26  1.151204e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 27  1.087141e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 28  1.129746e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 29  1.064205e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 30  1.053524e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 31  1.061392e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 32  1.057339e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 33  1.049066e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 34  1.075435e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 35  1.081228e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 36  1.080799e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 37  1.057410e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 38  1.056886e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 39  1.097083e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 40  1.065087e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 41  1.096892e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 42  1.159143e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 43  1.157451e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 44  1.152563e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 45  1.154828e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 46  1.169825e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 47  1.157141e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 48  1.141882e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 49  1.158571e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 50  1.147580e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 51  1.145220e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 52  1.167822e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 53  1.155043e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 54  1.165509e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 55  1.138043e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 56  1.143098e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 57  1.163316e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 58  1.168489e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 59  1.161718e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 60  1.164913e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 61  1.166320e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 62  1.155972e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 63  1.162434e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 64  1.164246e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 65  1.154494e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 66  1.147032e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 67  1.067138e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 68  1.157117e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 69  1.168466e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 70  1.147246e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 71  1.145935e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 72  1.148820e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 73  1.155376e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 74  1.164961e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 75  1.159620e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 76  1.164770e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 77  1.164055e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 78  1.163507e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 79  1.166558e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 80  1.166606e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 81  1.154065e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 82  1.153612e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 83  1.156378e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 84  1.153374e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 85  1.149702e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 86  1.159787e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 87  1.160669e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 88  1.153803e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 89  1.164246e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 90  1.164699e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 91  1.156902e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 92  1.159143e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 93  1.138449e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 94  1.155853e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 95  1.143432e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 96  1.167488e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 97  1.155949e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 98  1.160479e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 99  1.168489e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 100 1.174831e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 101 1.168346e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 102 1.163530e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 103 1.165700e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 104 1.160431e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 105 1.169372e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 106 1.160192e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 107 1.172400e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 108 1.162839e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 109 1.170564e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 110 1.158071e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 111 1.171708e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 112 1.166844e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 113 1.168275e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 114 1.170635e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 115 1.169133e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 116 1.169109e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 117 1.164103e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 118 1.154137e-02 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 122 8.600235e-03 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 119 3.046989e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 120 1.399517e-04 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#> 123 3.290176e-05 secs         <NA> 2429a1a68beabacff0e26fae8952a02a-19
#>                         session_uuid
#> 1   2429a1a68beabacff0e26fae8952a02a
#> 4   2429a1a68beabacff0e26fae8952a02a
#> 2   2429a1a68beabacff0e26fae8952a02a
#> 3   2429a1a68beabacff0e26fae8952a02a
#> 121 2429a1a68beabacff0e26fae8952a02a
#> 5   2429a1a68beabacff0e26fae8952a02a
#> 6   2429a1a68beabacff0e26fae8952a02a
#> 7   2429a1a68beabacff0e26fae8952a02a
#> 8   2429a1a68beabacff0e26fae8952a02a
#> 9   2429a1a68beabacff0e26fae8952a02a
#> 10  2429a1a68beabacff0e26fae8952a02a
#> 11  2429a1a68beabacff0e26fae8952a02a
#> 12  2429a1a68beabacff0e26fae8952a02a
#> 13  2429a1a68beabacff0e26fae8952a02a
#> 14  2429a1a68beabacff0e26fae8952a02a
#> 15  2429a1a68beabacff0e26fae8952a02a
#> 16  2429a1a68beabacff0e26fae8952a02a
#> 17  2429a1a68beabacff0e26fae8952a02a
#> 18  2429a1a68beabacff0e26fae8952a02a
#> 19  2429a1a68beabacff0e26fae8952a02a
#> 20  2429a1a68beabacff0e26fae8952a02a
#> 21  2429a1a68beabacff0e26fae8952a02a
#> 22  2429a1a68beabacff0e26fae8952a02a
#> 23  2429a1a68beabacff0e26fae8952a02a
#> 24  2429a1a68beabacff0e26fae8952a02a
#> 25  2429a1a68beabacff0e26fae8952a02a
#> 26  2429a1a68beabacff0e26fae8952a02a
#> 27  2429a1a68beabacff0e26fae8952a02a
#> 28  2429a1a68beabacff0e26fae8952a02a
#> 29  2429a1a68beabacff0e26fae8952a02a
#> 30  2429a1a68beabacff0e26fae8952a02a
#> 31  2429a1a68beabacff0e26fae8952a02a
#> 32  2429a1a68beabacff0e26fae8952a02a
#> 33  2429a1a68beabacff0e26fae8952a02a
#> 34  2429a1a68beabacff0e26fae8952a02a
#> 35  2429a1a68beabacff0e26fae8952a02a
#> 36  2429a1a68beabacff0e26fae8952a02a
#> 37  2429a1a68beabacff0e26fae8952a02a
#> 38  2429a1a68beabacff0e26fae8952a02a
#> 39  2429a1a68beabacff0e26fae8952a02a
#> 40  2429a1a68beabacff0e26fae8952a02a
#> 41  2429a1a68beabacff0e26fae8952a02a
#> 42  2429a1a68beabacff0e26fae8952a02a
#> 43  2429a1a68beabacff0e26fae8952a02a
#> 44  2429a1a68beabacff0e26fae8952a02a
#> 45  2429a1a68beabacff0e26fae8952a02a
#> 46  2429a1a68beabacff0e26fae8952a02a
#> 47  2429a1a68beabacff0e26fae8952a02a
#> 48  2429a1a68beabacff0e26fae8952a02a
#> 49  2429a1a68beabacff0e26fae8952a02a
#> 50  2429a1a68beabacff0e26fae8952a02a
#> 51  2429a1a68beabacff0e26fae8952a02a
#> 52  2429a1a68beabacff0e26fae8952a02a
#> 53  2429a1a68beabacff0e26fae8952a02a
#> 54  2429a1a68beabacff0e26fae8952a02a
#> 55  2429a1a68beabacff0e26fae8952a02a
#> 56  2429a1a68beabacff0e26fae8952a02a
#> 57  2429a1a68beabacff0e26fae8952a02a
#> 58  2429a1a68beabacff0e26fae8952a02a
#> 59  2429a1a68beabacff0e26fae8952a02a
#> 60  2429a1a68beabacff0e26fae8952a02a
#> 61  2429a1a68beabacff0e26fae8952a02a
#> 62  2429a1a68beabacff0e26fae8952a02a
#> 63  2429a1a68beabacff0e26fae8952a02a
#> 64  2429a1a68beabacff0e26fae8952a02a
#> 65  2429a1a68beabacff0e26fae8952a02a
#> 66  2429a1a68beabacff0e26fae8952a02a
#> 67  2429a1a68beabacff0e26fae8952a02a
#> 68  2429a1a68beabacff0e26fae8952a02a
#> 69  2429a1a68beabacff0e26fae8952a02a
#> 70  2429a1a68beabacff0e26fae8952a02a
#> 71  2429a1a68beabacff0e26fae8952a02a
#> 72  2429a1a68beabacff0e26fae8952a02a
#> 73  2429a1a68beabacff0e26fae8952a02a
#> 74  2429a1a68beabacff0e26fae8952a02a
#> 75  2429a1a68beabacff0e26fae8952a02a
#> 76  2429a1a68beabacff0e26fae8952a02a
#> 77  2429a1a68beabacff0e26fae8952a02a
#> 78  2429a1a68beabacff0e26fae8952a02a
#> 79  2429a1a68beabacff0e26fae8952a02a
#> 80  2429a1a68beabacff0e26fae8952a02a
#> 81  2429a1a68beabacff0e26fae8952a02a
#> 82  2429a1a68beabacff0e26fae8952a02a
#> 83  2429a1a68beabacff0e26fae8952a02a
#> 84  2429a1a68beabacff0e26fae8952a02a
#> 85  2429a1a68beabacff0e26fae8952a02a
#> 86  2429a1a68beabacff0e26fae8952a02a
#> 87  2429a1a68beabacff0e26fae8952a02a
#> 88  2429a1a68beabacff0e26fae8952a02a
#> 89  2429a1a68beabacff0e26fae8952a02a
#> 90  2429a1a68beabacff0e26fae8952a02a
#> 91  2429a1a68beabacff0e26fae8952a02a
#> 92  2429a1a68beabacff0e26fae8952a02a
#> 93  2429a1a68beabacff0e26fae8952a02a
#> 94  2429a1a68beabacff0e26fae8952a02a
#> 95  2429a1a68beabacff0e26fae8952a02a
#> 96  2429a1a68beabacff0e26fae8952a02a
#> 97  2429a1a68beabacff0e26fae8952a02a
#> 98  2429a1a68beabacff0e26fae8952a02a
#> 99  2429a1a68beabacff0e26fae8952a02a
#> 100 2429a1a68beabacff0e26fae8952a02a
#> 101 2429a1a68beabacff0e26fae8952a02a
#> 102 2429a1a68beabacff0e26fae8952a02a
#> 103 2429a1a68beabacff0e26fae8952a02a
#> 104 2429a1a68beabacff0e26fae8952a02a
#> 105 2429a1a68beabacff0e26fae8952a02a
#> 106 2429a1a68beabacff0e26fae8952a02a
#> 107 2429a1a68beabacff0e26fae8952a02a
#> 108 2429a1a68beabacff0e26fae8952a02a
#> 109 2429a1a68beabacff0e26fae8952a02a
#> 110 2429a1a68beabacff0e26fae8952a02a
#> 111 2429a1a68beabacff0e26fae8952a02a
#> 112 2429a1a68beabacff0e26fae8952a02a
#> 113 2429a1a68beabacff0e26fae8952a02a
#> 114 2429a1a68beabacff0e26fae8952a02a
#> 115 2429a1a68beabacff0e26fae8952a02a
#> 116 2429a1a68beabacff0e26fae8952a02a
#> 117 2429a1a68beabacff0e26fae8952a02a
#> 118 2429a1a68beabacff0e26fae8952a02a
#> 122 2429a1a68beabacff0e26fae8952a02a
#> 119 2429a1a68beabacff0e26fae8952a02a
#> 120 2429a1a68beabacff0e26fae8952a02a
#> 123 2429a1a68beabacff0e26fae8952a02a
#> 

## Stop parallel workers and disable journal logging and signaling
plan(sequential)
options(oopts)
```
