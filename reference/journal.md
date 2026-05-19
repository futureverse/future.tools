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
#> 1         create   overhead   <NA> 2026-05-18 21:43:00 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-05-18 21:43:00 0.007316113 secs
#> 2      getWorker   overhead launch 2026-05-18 21:43:00 0.007465601 secs
#> 3    eraseWorker   overhead launch 2026-05-18 21:43:00 0.008897543 secs
#> 5       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.052569866 secs
#> 6       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.086985588 secs
#> 7       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.121183157 secs
#> 8       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.155398130 secs
#> 9       resolved       <NA>   <NA> 2026-05-18 21:43:01 0.189940214 secs
#> 10      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.224567413 secs
#> 41      evaluate evaluation   <NA> 2026-05-18 21:43:01 0.225333691 secs
#> 11      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.259438038 secs
#> 12      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.294220209 secs
#> 13      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.328825474 secs
#> 14      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.363375902 secs
#> 15      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.399512053 secs
#> 16      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.437319994 secs
#> 17      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.475749016 secs
#> 18      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.513956785 secs
#> 19      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.552118540 secs
#> 20      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.589874744 secs
#> 21      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.628646374 secs
#> 22      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.675104856 secs
#> 23      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.714304686 secs
#> 24      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.753663063 secs
#> 25      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.793109417 secs
#> 26      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.832437754 secs
#> 27      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.872129679 secs
#> 28      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.911931515 secs
#> 29      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.952009439 secs
#> 30      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.992016315 secs
#> 31      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.032137632 secs
#> 32      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.072309971 secs
#> 33      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.112679720 secs
#> 34      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.153041840 secs
#> 35      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.192930460 secs
#> 36      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.233195305 secs
#> 37      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.273591518 secs
#> 38      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.314706087 secs
#> 42      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.355365992 secs
#> 39 receiveResult   overhead gather 2026-05-18 21:43:02 1.358559608 secs
#> 40        gather   overhead   <NA> 2026-05-18 21:43:02 1.360552073 secs
#> 43      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.382220268 secs
#> 44       resolve   overhead   <NA> 2026-05-18 21:43:02 1.384499073 secs
#>             duration future_label                         future_uuid
#> 1  6.922245e-03 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 4  6.009579e-03 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 2  7.495880e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 3  4.742146e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 5  1.118112e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 6  1.116276e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 7  1.091456e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 8  1.116323e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 9  1.105618e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 10 1.087260e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 41 1.130545e+00 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 11 1.089001e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 12 1.087523e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 13 1.088977e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 14 1.132894e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 15 1.150155e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 16 1.180601e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 17 1.161861e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 18 1.147461e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 19 1.144385e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 20 1.140571e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 21 1.180935e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 22 1.165056e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 23 1.153946e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 24 1.197577e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 25 1.147723e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 26 1.178265e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 27 1.165867e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 28 1.179433e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 29 1.176953e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 30 1.169443e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 31 1.174974e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 32 1.172233e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 33 1.171923e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 34 1.150560e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 35 1.164317e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 36 1.151729e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 37 1.163125e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 38 1.158190e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 42 9.869337e-03 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 39 3.304482e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 40 1.320839e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 43 9.846687e-05 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#> 44 3.242493e-05 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-17
#>                        session_uuid
#> 1  8a9eefebe0d4cc2e160302252a75da6d
#> 4  8a9eefebe0d4cc2e160302252a75da6d
#> 2  8a9eefebe0d4cc2e160302252a75da6d
#> 3  8a9eefebe0d4cc2e160302252a75da6d
#> 5  8a9eefebe0d4cc2e160302252a75da6d
#> 6  8a9eefebe0d4cc2e160302252a75da6d
#> 7  8a9eefebe0d4cc2e160302252a75da6d
#> 8  8a9eefebe0d4cc2e160302252a75da6d
#> 9  8a9eefebe0d4cc2e160302252a75da6d
#> 10 8a9eefebe0d4cc2e160302252a75da6d
#> 41 8a9eefebe0d4cc2e160302252a75da6d
#> 11 8a9eefebe0d4cc2e160302252a75da6d
#> 12 8a9eefebe0d4cc2e160302252a75da6d
#> 13 8a9eefebe0d4cc2e160302252a75da6d
#> 14 8a9eefebe0d4cc2e160302252a75da6d
#> 15 8a9eefebe0d4cc2e160302252a75da6d
#> 16 8a9eefebe0d4cc2e160302252a75da6d
#> 17 8a9eefebe0d4cc2e160302252a75da6d
#> 18 8a9eefebe0d4cc2e160302252a75da6d
#> 19 8a9eefebe0d4cc2e160302252a75da6d
#> 20 8a9eefebe0d4cc2e160302252a75da6d
#> 21 8a9eefebe0d4cc2e160302252a75da6d
#> 22 8a9eefebe0d4cc2e160302252a75da6d
#> 23 8a9eefebe0d4cc2e160302252a75da6d
#> 24 8a9eefebe0d4cc2e160302252a75da6d
#> 25 8a9eefebe0d4cc2e160302252a75da6d
#> 26 8a9eefebe0d4cc2e160302252a75da6d
#> 27 8a9eefebe0d4cc2e160302252a75da6d
#> 28 8a9eefebe0d4cc2e160302252a75da6d
#> 29 8a9eefebe0d4cc2e160302252a75da6d
#> 30 8a9eefebe0d4cc2e160302252a75da6d
#> 31 8a9eefebe0d4cc2e160302252a75da6d
#> 32 8a9eefebe0d4cc2e160302252a75da6d
#> 33 8a9eefebe0d4cc2e160302252a75da6d
#> 34 8a9eefebe0d4cc2e160302252a75da6d
#> 35 8a9eefebe0d4cc2e160302252a75da6d
#> 36 8a9eefebe0d4cc2e160302252a75da6d
#> 37 8a9eefebe0d4cc2e160302252a75da6d
#> 38 8a9eefebe0d4cc2e160302252a75da6d
#> 42 8a9eefebe0d4cc2e160302252a75da6d
#> 39 8a9eefebe0d4cc2e160302252a75da6d
#> 40 8a9eefebe0d4cc2e160302252a75da6d
#> 43 8a9eefebe0d4cc2e160302252a75da6d
#> 44 8a9eefebe0d4cc2e160302252a75da6d
#> 
#> [[2]]
#>            event   category parent               start               at
#> 1         create   overhead   <NA> 2026-05-18 21:43:00 0.000000000 secs
#> 4         launch   overhead   <NA> 2026-05-18 21:43:00 0.006854773 secs
#> 2      getWorker   overhead launch 2026-05-18 21:43:00 0.006991625 secs
#> 3    eraseWorker   overhead launch 2026-05-18 21:43:00 0.008297205 secs
#> 5       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.050669909 secs
#> 6       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.084989786 secs
#> 7       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.119065762 secs
#> 8       resolved       <NA>   <NA> 2026-05-18 21:43:00 0.153493881 secs
#> 9       resolved       <NA>   <NA> 2026-05-18 21:43:01 0.187834501 secs
#> 10      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.222550154 secs
#> 68      evaluate evaluation   <NA> 2026-05-18 21:43:01 0.228397846 secs
#> 11      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.257186174 secs
#> 12      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.291904688 secs
#> 13      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.326567650 secs
#> 14      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.361770391 secs
#> 15      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.398642540 secs
#> 16      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.436849356 secs
#> 17      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.475066423 secs
#> 18      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.513016462 secs
#> 19      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.551297426 secs
#> 20      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.589062691 secs
#> 21      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.628215075 secs
#> 22      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.674503326 secs
#> 23      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.713639975 secs
#> 24      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.753387451 secs
#> 25      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.792371750 secs
#> 26      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.831875563 secs
#> 27      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.871582031 secs
#> 28      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.911442280 secs
#> 29      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.951464415 secs
#> 30      resolved       <NA>   <NA> 2026-05-18 21:43:01 0.991422176 secs
#> 31      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.031557083 secs
#> 32      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.071744680 secs
#> 33      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.112025499 secs
#> 34      resolved       <NA>   <NA> 2026-05-18 21:43:01 1.152026892 secs
#> 35      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.192193985 secs
#> 36      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.232347727 secs
#> 37      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.272778749 secs
#> 38      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.313894987 secs
#> 39      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.371814966 secs
#> 40      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.408226728 secs
#> 41      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.444530249 secs
#> 42      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.480931520 secs
#> 43      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.517539263 secs
#> 44      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.553966999 secs
#> 45      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.589524031 secs
#> 46      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.626801968 secs
#> 47      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.663762093 secs
#> 48      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.701005936 secs
#> 49      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.739575863 secs
#> 50      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.776810884 secs
#> 51      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.813690186 secs
#> 52      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.850284100 secs
#> 53      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.886920452 secs
#> 54      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.923875332 secs
#> 55      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.960370541 secs
#> 56      resolved       <NA>   <NA> 2026-05-18 21:43:02 1.996902704 secs
#> 57      resolved       <NA>   <NA> 2026-05-18 21:43:02 2.033962011 secs
#> 58      resolved       <NA>   <NA> 2026-05-18 21:43:02 2.070790529 secs
#> 59      resolved       <NA>   <NA> 2026-05-18 21:43:02 2.108396769 secs
#> 60      resolved       <NA>   <NA> 2026-05-18 21:43:02 2.145118713 secs
#> 61      resolved       <NA>   <NA> 2026-05-18 21:43:03 2.181893587 secs
#> 62      resolved       <NA>   <NA> 2026-05-18 21:43:03 2.218632698 secs
#> 63      resolved       <NA>   <NA> 2026-05-18 21:43:03 2.255377293 secs
#> 64      resolved       <NA>   <NA> 2026-05-18 21:43:03 2.292185068 secs
#> 65      resolved       <NA>   <NA> 2026-05-18 21:43:03 2.328911066 secs
#> 69      resolved       <NA>   <NA> 2026-05-18 21:43:03 2.365825653 secs
#> 66 receiveResult   overhead gather 2026-05-18 21:43:03 2.367328882 secs
#> 67        gather   overhead   <NA> 2026-05-18 21:43:03 2.369220972 secs
#> 70       resolve   overhead   <NA> 2026-05-18 21:43:03 2.376339197 secs
#>             duration future_label                         future_uuid
#> 1  0.0065295696 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 4  0.0076293945 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 2  0.0006384850 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 3  0.0004024506 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 5  0.0110592842 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 6  0.0108509064 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 7  0.0108046532 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 8  0.0107638836 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 9  0.0108308792 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 10 0.0109219551 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 68 2.1325955391 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 11 0.0108547211 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 12 0.0107953548 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 13 0.0107910633 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 14 0.0110201836 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 15 0.0113468170 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 16 0.0114850998 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 17 0.0114276409 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 18 0.0115778446 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 19 0.0111651421 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 20 0.0115628242 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 21 0.0186572075 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 22 0.0114903450 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 23 0.0114150047 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 24 0.0113663673 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 25 0.0114648342 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 26 0.0115146637 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 27 0.0113756657 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 28 0.0115196705 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 29 0.0115046501 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 30 0.0114653111 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 31 0.0114817619 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 32 0.0113704205 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 33 0.0115973949 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 34 0.0113985538 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 35 0.0112590790 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 36 0.0113713741 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 37 0.0112864971 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 38 0.0113821030 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 39 0.0113008022 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 40 0.0115904808 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 41 0.0115463734 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 42 0.0115919113 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 43 0.0115368366 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 44 0.0115072727 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 45 0.0116839409 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 46 0.0119431019 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 47 0.0117373466 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 48 0.0115103722 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 49 0.0115461349 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 50 0.0119111538 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 51 0.0116510391 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 52 0.0115857124 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 53 0.0117492676 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 54 0.0116720200 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 55 0.0115394592 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 56 0.0117182732 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 57 0.0117163658 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 58 0.0117073059 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 59 0.0117769241 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 60 0.0116004944 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 61 0.0117235184 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 62 0.0116403103 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 63 0.0117361546 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 64 0.0116760731 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 65 0.0118045807 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 69 0.0082092285 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 66 0.0002882481 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 67 0.0001308918 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#> 70 0.0000333786 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-18
#>                        session_uuid
#> 1  8a9eefebe0d4cc2e160302252a75da6d
#> 4  8a9eefebe0d4cc2e160302252a75da6d
#> 2  8a9eefebe0d4cc2e160302252a75da6d
#> 3  8a9eefebe0d4cc2e160302252a75da6d
#> 5  8a9eefebe0d4cc2e160302252a75da6d
#> 6  8a9eefebe0d4cc2e160302252a75da6d
#> 7  8a9eefebe0d4cc2e160302252a75da6d
#> 8  8a9eefebe0d4cc2e160302252a75da6d
#> 9  8a9eefebe0d4cc2e160302252a75da6d
#> 10 8a9eefebe0d4cc2e160302252a75da6d
#> 68 8a9eefebe0d4cc2e160302252a75da6d
#> 11 8a9eefebe0d4cc2e160302252a75da6d
#> 12 8a9eefebe0d4cc2e160302252a75da6d
#> 13 8a9eefebe0d4cc2e160302252a75da6d
#> 14 8a9eefebe0d4cc2e160302252a75da6d
#> 15 8a9eefebe0d4cc2e160302252a75da6d
#> 16 8a9eefebe0d4cc2e160302252a75da6d
#> 17 8a9eefebe0d4cc2e160302252a75da6d
#> 18 8a9eefebe0d4cc2e160302252a75da6d
#> 19 8a9eefebe0d4cc2e160302252a75da6d
#> 20 8a9eefebe0d4cc2e160302252a75da6d
#> 21 8a9eefebe0d4cc2e160302252a75da6d
#> 22 8a9eefebe0d4cc2e160302252a75da6d
#> 23 8a9eefebe0d4cc2e160302252a75da6d
#> 24 8a9eefebe0d4cc2e160302252a75da6d
#> 25 8a9eefebe0d4cc2e160302252a75da6d
#> 26 8a9eefebe0d4cc2e160302252a75da6d
#> 27 8a9eefebe0d4cc2e160302252a75da6d
#> 28 8a9eefebe0d4cc2e160302252a75da6d
#> 29 8a9eefebe0d4cc2e160302252a75da6d
#> 30 8a9eefebe0d4cc2e160302252a75da6d
#> 31 8a9eefebe0d4cc2e160302252a75da6d
#> 32 8a9eefebe0d4cc2e160302252a75da6d
#> 33 8a9eefebe0d4cc2e160302252a75da6d
#> 34 8a9eefebe0d4cc2e160302252a75da6d
#> 35 8a9eefebe0d4cc2e160302252a75da6d
#> 36 8a9eefebe0d4cc2e160302252a75da6d
#> 37 8a9eefebe0d4cc2e160302252a75da6d
#> 38 8a9eefebe0d4cc2e160302252a75da6d
#> 39 8a9eefebe0d4cc2e160302252a75da6d
#> 40 8a9eefebe0d4cc2e160302252a75da6d
#> 41 8a9eefebe0d4cc2e160302252a75da6d
#> 42 8a9eefebe0d4cc2e160302252a75da6d
#> 43 8a9eefebe0d4cc2e160302252a75da6d
#> 44 8a9eefebe0d4cc2e160302252a75da6d
#> 45 8a9eefebe0d4cc2e160302252a75da6d
#> 46 8a9eefebe0d4cc2e160302252a75da6d
#> 47 8a9eefebe0d4cc2e160302252a75da6d
#> 48 8a9eefebe0d4cc2e160302252a75da6d
#> 49 8a9eefebe0d4cc2e160302252a75da6d
#> 50 8a9eefebe0d4cc2e160302252a75da6d
#> 51 8a9eefebe0d4cc2e160302252a75da6d
#> 52 8a9eefebe0d4cc2e160302252a75da6d
#> 53 8a9eefebe0d4cc2e160302252a75da6d
#> 54 8a9eefebe0d4cc2e160302252a75da6d
#> 55 8a9eefebe0d4cc2e160302252a75da6d
#> 56 8a9eefebe0d4cc2e160302252a75da6d
#> 57 8a9eefebe0d4cc2e160302252a75da6d
#> 58 8a9eefebe0d4cc2e160302252a75da6d
#> 59 8a9eefebe0d4cc2e160302252a75da6d
#> 60 8a9eefebe0d4cc2e160302252a75da6d
#> 61 8a9eefebe0d4cc2e160302252a75da6d
#> 62 8a9eefebe0d4cc2e160302252a75da6d
#> 63 8a9eefebe0d4cc2e160302252a75da6d
#> 64 8a9eefebe0d4cc2e160302252a75da6d
#> 65 8a9eefebe0d4cc2e160302252a75da6d
#> 69 8a9eefebe0d4cc2e160302252a75da6d
#> 66 8a9eefebe0d4cc2e160302252a75da6d
#> 67 8a9eefebe0d4cc2e160302252a75da6d
#> 70 8a9eefebe0d4cc2e160302252a75da6d
#> 
#> [[3]]
#>             event   category parent               start              at
#> 1          create   overhead   <NA> 2026-05-18 21:43:00 0.00000000 secs
#> 4          launch   overhead   <NA> 2026-05-18 21:43:00 0.01254702 secs
#> 2       getWorker   overhead launch 2026-05-18 21:43:00 0.01270390 secs
#> 3     eraseWorker   overhead launch 2026-05-18 21:43:02 1.34067035 secs
#> 118      evaluate evaluation   <NA> 2026-05-18 21:43:02 1.35475945 secs
#> 5        resolved       <NA>   <NA> 2026-05-18 21:43:02 1.36910582 secs
#> 6        resolved       <NA>   <NA> 2026-05-18 21:43:02 1.40585351 secs
#> 7        resolved       <NA>   <NA> 2026-05-18 21:43:02 1.44210553 secs
#> 8        resolved       <NA>   <NA> 2026-05-18 21:43:02 1.47854590 secs
#> 9        resolved       <NA>   <NA> 2026-05-18 21:43:02 1.51510119 secs
#> 10       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.55135298 secs
#> 11       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.58728290 secs
#> 12       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.62477589 secs
#> 13       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.66150904 secs
#> 14       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.69962072 secs
#> 15       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.73733830 secs
#> 16       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.77475190 secs
#> 17       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.81137490 secs
#> 18       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.84791279 secs
#> 19       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.88480759 secs
#> 20       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.92158961 secs
#> 21       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.95791030 secs
#> 22       resolved       <NA>   <NA> 2026-05-18 21:43:02 1.99476719 secs
#> 23       resolved       <NA>   <NA> 2026-05-18 21:43:02 2.03190947 secs
#> 24       resolved       <NA>   <NA> 2026-05-18 21:43:02 2.06864047 secs
#> 25       resolved       <NA>   <NA> 2026-05-18 21:43:02 2.10620117 secs
#> 26       resolved       <NA>   <NA> 2026-05-18 21:43:02 2.14283371 secs
#> 27       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.17965770 secs
#> 28       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.21635985 secs
#> 29       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.25325918 secs
#> 30       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.29000044 secs
#> 31       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.32674003 secs
#> 32       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.36204791 secs
#> 33       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.38528419 secs
#> 34       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.40880537 secs
#> 35       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.43232584 secs
#> 36       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.45595908 secs
#> 37       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.47954583 secs
#> 38       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.50279164 secs
#> 39       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.52608991 secs
#> 40       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.54934692 secs
#> 41       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.57259583 secs
#> 42       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.59592080 secs
#> 43       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.61940479 secs
#> 44       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.64309311 secs
#> 45       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.66678762 secs
#> 46       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.69021201 secs
#> 47       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.71446657 secs
#> 48       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.73796701 secs
#> 49       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.76182723 secs
#> 50       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.78545070 secs
#> 51       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.81561399 secs
#> 52       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.83919024 secs
#> 53       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.86271596 secs
#> 54       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.88671708 secs
#> 55       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.91049743 secs
#> 56       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.93427372 secs
#> 57       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.95800638 secs
#> 58       resolved       <NA>   <NA> 2026-05-18 21:43:03 2.98180294 secs
#> 59       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.00559807 secs
#> 60       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.02945876 secs
#> 61       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.05321455 secs
#> 62       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.07704258 secs
#> 63       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.10074997 secs
#> 64       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.12463689 secs
#> 65       resolved       <NA>   <NA> 2026-05-18 21:43:03 3.14816189 secs
#> 66       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.17160082 secs
#> 67       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.19517350 secs
#> 68       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.21871686 secs
#> 69       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.24241447 secs
#> 70       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.26610851 secs
#> 71       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.28976274 secs
#> 72       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.31342578 secs
#> 73       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.33720374 secs
#> 74       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.36101222 secs
#> 75       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.38478780 secs
#> 76       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.40860486 secs
#> 77       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.43246317 secs
#> 78       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.45633531 secs
#> 79       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.48023963 secs
#> 80       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.50394750 secs
#> 81       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.52784324 secs
#> 82       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.55155039 secs
#> 83       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.57483029 secs
#> 84       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.59811950 secs
#> 85       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.62179804 secs
#> 86       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.64620447 secs
#> 87       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.67015147 secs
#> 88       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.69407701 secs
#> 89       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.71893620 secs
#> 90       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.74351597 secs
#> 91       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.76677823 secs
#> 92       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.79016185 secs
#> 93       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.81372046 secs
#> 94       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.83718109 secs
#> 95       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.86098361 secs
#> 96       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.88471222 secs
#> 97       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.90850449 secs
#> 98       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.93244290 secs
#> 99       resolved       <NA>   <NA> 2026-05-18 21:43:04 3.95632505 secs
#> 100      resolved       <NA>   <NA> 2026-05-18 21:43:04 3.98022866 secs
#> 101      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.00394917 secs
#> 102      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.02762389 secs
#> 103      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.05128074 secs
#> 104      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.07471871 secs
#> 105      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.09840584 secs
#> 106      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.12214065 secs
#> 107      resolved       <NA>   <NA> 2026-05-18 21:43:04 4.14576554 secs
#> 108      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.16949034 secs
#> 109      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.19303417 secs
#> 110      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.21661091 secs
#> 111      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.24034286 secs
#> 112      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.26408100 secs
#> 113      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.28763461 secs
#> 114      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.31114244 secs
#> 115      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.33482838 secs
#> 119      resolved       <NA>   <NA> 2026-05-18 21:43:05 4.35872674 secs
#> 116 receiveResult   overhead gather 2026-05-18 21:43:05 4.36549091 secs
#> 117        gather   overhead   <NA> 2026-05-18 21:43:05 4.36768126 secs
#> 120       resolve   overhead   <NA> 2026-05-18 21:43:05 4.37468219 secs
#>              duration future_label                         future_uuid
#> 1   1.220465e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 4   1.337549e+00 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 2   1.326297e+00 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 3   9.908676e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 118 3.008945e+00 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 5   1.161838e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 6   1.137829e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 7   1.139665e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 8   1.136851e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 9   1.139784e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 10  1.107621e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 11  1.198030e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 12  1.134181e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 13  1.150131e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 14  1.228189e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 15  1.135063e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 16  1.146054e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 17  1.126480e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 18  1.141238e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 19  1.147389e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 20  1.133919e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 21  1.139879e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 22  1.142216e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 23  1.141834e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 24  1.138854e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 25  1.149058e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 26  1.160836e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 27  1.140165e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 28  1.145887e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 29  1.147032e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 30  1.145482e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 31  1.139522e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 32  1.131415e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 33  1.164079e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 34  1.162004e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 35  1.176858e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 36  1.175523e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 37  1.147819e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 38  1.156592e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 39  1.151156e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 40  1.142263e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 41  1.145172e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 42  1.150250e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 43  1.172066e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 44  1.172829e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 45  1.160049e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 46  1.226425e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 47  1.170635e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 48  1.179886e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 49  1.178432e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 50  1.152515e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 51  1.148462e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 52  1.159334e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 53  1.157713e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 54  1.172972e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 55  1.173592e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 56  1.171136e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 57  1.172972e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 58  1.170993e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 59  1.175547e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 60  1.167178e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 61  1.170468e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 62  1.168656e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 63  1.175952e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 64  1.161456e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 65  1.159573e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 66  1.165628e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 67  1.159620e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 68  1.155782e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 69  1.167870e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 70  1.172757e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 71  1.156950e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 72  1.167154e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 73  1.176286e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 74  1.172948e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 75  1.172209e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 76  1.170921e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 77  1.182771e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 78  1.177144e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 79  1.183534e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 80  1.181221e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 81  1.166463e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 82  1.156592e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 83  1.144481e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 84  1.166177e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 85  1.263881e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 86  1.181388e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 87  1.178169e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 88  1.181650e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 89  1.199865e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 90  1.150537e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 91  1.145339e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 92  1.165462e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 93  1.145077e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 94  1.178718e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 95  1.173878e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 96  1.171184e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 97  1.181865e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 98  1.177883e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 99  1.173782e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 100 1.178861e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 101 1.179814e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 102 1.170373e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 103 1.154423e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 104 1.177144e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 105 1.165295e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 106 1.162004e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 107 1.168489e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 108 1.160717e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 109 1.168036e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 110 1.168799e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 111 1.171660e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 112 1.166010e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 113 1.157093e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 114 1.171231e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 115 1.192045e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 119 1.367784e-02 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 116 4.019737e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 117 1.435280e-04 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#> 120 3.314018e-05 secs         <NA> 8a9eefebe0d4cc2e160302252a75da6d-19
#>                         session_uuid
#> 1   8a9eefebe0d4cc2e160302252a75da6d
#> 4   8a9eefebe0d4cc2e160302252a75da6d
#> 2   8a9eefebe0d4cc2e160302252a75da6d
#> 3   8a9eefebe0d4cc2e160302252a75da6d
#> 118 8a9eefebe0d4cc2e160302252a75da6d
#> 5   8a9eefebe0d4cc2e160302252a75da6d
#> 6   8a9eefebe0d4cc2e160302252a75da6d
#> 7   8a9eefebe0d4cc2e160302252a75da6d
#> 8   8a9eefebe0d4cc2e160302252a75da6d
#> 9   8a9eefebe0d4cc2e160302252a75da6d
#> 10  8a9eefebe0d4cc2e160302252a75da6d
#> 11  8a9eefebe0d4cc2e160302252a75da6d
#> 12  8a9eefebe0d4cc2e160302252a75da6d
#> 13  8a9eefebe0d4cc2e160302252a75da6d
#> 14  8a9eefebe0d4cc2e160302252a75da6d
#> 15  8a9eefebe0d4cc2e160302252a75da6d
#> 16  8a9eefebe0d4cc2e160302252a75da6d
#> 17  8a9eefebe0d4cc2e160302252a75da6d
#> 18  8a9eefebe0d4cc2e160302252a75da6d
#> 19  8a9eefebe0d4cc2e160302252a75da6d
#> 20  8a9eefebe0d4cc2e160302252a75da6d
#> 21  8a9eefebe0d4cc2e160302252a75da6d
#> 22  8a9eefebe0d4cc2e160302252a75da6d
#> 23  8a9eefebe0d4cc2e160302252a75da6d
#> 24  8a9eefebe0d4cc2e160302252a75da6d
#> 25  8a9eefebe0d4cc2e160302252a75da6d
#> 26  8a9eefebe0d4cc2e160302252a75da6d
#> 27  8a9eefebe0d4cc2e160302252a75da6d
#> 28  8a9eefebe0d4cc2e160302252a75da6d
#> 29  8a9eefebe0d4cc2e160302252a75da6d
#> 30  8a9eefebe0d4cc2e160302252a75da6d
#> 31  8a9eefebe0d4cc2e160302252a75da6d
#> 32  8a9eefebe0d4cc2e160302252a75da6d
#> 33  8a9eefebe0d4cc2e160302252a75da6d
#> 34  8a9eefebe0d4cc2e160302252a75da6d
#> 35  8a9eefebe0d4cc2e160302252a75da6d
#> 36  8a9eefebe0d4cc2e160302252a75da6d
#> 37  8a9eefebe0d4cc2e160302252a75da6d
#> 38  8a9eefebe0d4cc2e160302252a75da6d
#> 39  8a9eefebe0d4cc2e160302252a75da6d
#> 40  8a9eefebe0d4cc2e160302252a75da6d
#> 41  8a9eefebe0d4cc2e160302252a75da6d
#> 42  8a9eefebe0d4cc2e160302252a75da6d
#> 43  8a9eefebe0d4cc2e160302252a75da6d
#> 44  8a9eefebe0d4cc2e160302252a75da6d
#> 45  8a9eefebe0d4cc2e160302252a75da6d
#> 46  8a9eefebe0d4cc2e160302252a75da6d
#> 47  8a9eefebe0d4cc2e160302252a75da6d
#> 48  8a9eefebe0d4cc2e160302252a75da6d
#> 49  8a9eefebe0d4cc2e160302252a75da6d
#> 50  8a9eefebe0d4cc2e160302252a75da6d
#> 51  8a9eefebe0d4cc2e160302252a75da6d
#> 52  8a9eefebe0d4cc2e160302252a75da6d
#> 53  8a9eefebe0d4cc2e160302252a75da6d
#> 54  8a9eefebe0d4cc2e160302252a75da6d
#> 55  8a9eefebe0d4cc2e160302252a75da6d
#> 56  8a9eefebe0d4cc2e160302252a75da6d
#> 57  8a9eefebe0d4cc2e160302252a75da6d
#> 58  8a9eefebe0d4cc2e160302252a75da6d
#> 59  8a9eefebe0d4cc2e160302252a75da6d
#> 60  8a9eefebe0d4cc2e160302252a75da6d
#> 61  8a9eefebe0d4cc2e160302252a75da6d
#> 62  8a9eefebe0d4cc2e160302252a75da6d
#> 63  8a9eefebe0d4cc2e160302252a75da6d
#> 64  8a9eefebe0d4cc2e160302252a75da6d
#> 65  8a9eefebe0d4cc2e160302252a75da6d
#> 66  8a9eefebe0d4cc2e160302252a75da6d
#> 67  8a9eefebe0d4cc2e160302252a75da6d
#> 68  8a9eefebe0d4cc2e160302252a75da6d
#> 69  8a9eefebe0d4cc2e160302252a75da6d
#> 70  8a9eefebe0d4cc2e160302252a75da6d
#> 71  8a9eefebe0d4cc2e160302252a75da6d
#> 72  8a9eefebe0d4cc2e160302252a75da6d
#> 73  8a9eefebe0d4cc2e160302252a75da6d
#> 74  8a9eefebe0d4cc2e160302252a75da6d
#> 75  8a9eefebe0d4cc2e160302252a75da6d
#> 76  8a9eefebe0d4cc2e160302252a75da6d
#> 77  8a9eefebe0d4cc2e160302252a75da6d
#> 78  8a9eefebe0d4cc2e160302252a75da6d
#> 79  8a9eefebe0d4cc2e160302252a75da6d
#> 80  8a9eefebe0d4cc2e160302252a75da6d
#> 81  8a9eefebe0d4cc2e160302252a75da6d
#> 82  8a9eefebe0d4cc2e160302252a75da6d
#> 83  8a9eefebe0d4cc2e160302252a75da6d
#> 84  8a9eefebe0d4cc2e160302252a75da6d
#> 85  8a9eefebe0d4cc2e160302252a75da6d
#> 86  8a9eefebe0d4cc2e160302252a75da6d
#> 87  8a9eefebe0d4cc2e160302252a75da6d
#> 88  8a9eefebe0d4cc2e160302252a75da6d
#> 89  8a9eefebe0d4cc2e160302252a75da6d
#> 90  8a9eefebe0d4cc2e160302252a75da6d
#> 91  8a9eefebe0d4cc2e160302252a75da6d
#> 92  8a9eefebe0d4cc2e160302252a75da6d
#> 93  8a9eefebe0d4cc2e160302252a75da6d
#> 94  8a9eefebe0d4cc2e160302252a75da6d
#> 95  8a9eefebe0d4cc2e160302252a75da6d
#> 96  8a9eefebe0d4cc2e160302252a75da6d
#> 97  8a9eefebe0d4cc2e160302252a75da6d
#> 98  8a9eefebe0d4cc2e160302252a75da6d
#> 99  8a9eefebe0d4cc2e160302252a75da6d
#> 100 8a9eefebe0d4cc2e160302252a75da6d
#> 101 8a9eefebe0d4cc2e160302252a75da6d
#> 102 8a9eefebe0d4cc2e160302252a75da6d
#> 103 8a9eefebe0d4cc2e160302252a75da6d
#> 104 8a9eefebe0d4cc2e160302252a75da6d
#> 105 8a9eefebe0d4cc2e160302252a75da6d
#> 106 8a9eefebe0d4cc2e160302252a75da6d
#> 107 8a9eefebe0d4cc2e160302252a75da6d
#> 108 8a9eefebe0d4cc2e160302252a75da6d
#> 109 8a9eefebe0d4cc2e160302252a75da6d
#> 110 8a9eefebe0d4cc2e160302252a75da6d
#> 111 8a9eefebe0d4cc2e160302252a75da6d
#> 112 8a9eefebe0d4cc2e160302252a75da6d
#> 113 8a9eefebe0d4cc2e160302252a75da6d
#> 114 8a9eefebe0d4cc2e160302252a75da6d
#> 115 8a9eefebe0d4cc2e160302252a75da6d
#> 119 8a9eefebe0d4cc2e160302252a75da6d
#> 116 8a9eefebe0d4cc2e160302252a75da6d
#> 117 8a9eefebe0d4cc2e160302252a75da6d
#> 120 8a9eefebe0d4cc2e160302252a75da6d
#> 

## Stop parallel workers and disable journal logging and signaling
plan(sequential)
options(oopts)
```
