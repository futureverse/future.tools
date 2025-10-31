library(future.tools)

slow_fcn <- function(x) {
  Sys.sleep(x / 100)
  sqrt(x)
}

if (require("future.apply")) {
  message("*** capture_journals() ...")

  plan(multisession, workers = 2)
  js <- capture_journals({
    vs <- future_lapply(3:1, FUN = slow_fcn)
  })
  print(js)
  stopifnot(
    is.list(js),
    all(vapply(js, FUN = is.data.frame, FUN.VALUE = NA))
  )
  
  ## Shut down parallel workers
  plan(sequential)

  message("*** capture_journals() ... done")
}
