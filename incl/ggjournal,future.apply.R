library(future.apply)
oopts <- options(future.journal = TRUE)

gather_journals <- function(expr, substitute = TRUE, envir = parent.frame()) {
  journals <- NULL
  withCallingHandlers({
    eval(expr, envir = envir)
  }, FutureJournalCondition = function(cond) {
    message(sprintf("Caught a %s", class(cond)[1]))
    journals <<- c(journals, list(cond$journal))
  })
  journals
}

slow_fcn <- function(x) {
  Sys.sleep(x / 10)
  sqrt(x)
}

plan(sequential)
js <- gather_journals({
  vs <- future_lapply(5:1, FUN = function(x) slow_fcn(x))
})
print(ggjournal(js))

plan(multisession, workers = 2)
js <- gather_journals({
  vs <- future_lapply(5:1, FUN = function(x) slow_fcn(x))
})
print(ggjournal(js))

plan(multisession, workers = 5)
js <- gather_journals({
  vs <- future_lapply(5:1, FUN = function(x) slow_fcn(x))
})
print(ggjournal(js))

## Shut down parallel workers
plan(sequential)
options(oopts)
