library(future.tools)

slow_fcn <- function(x) {
  Sys.sleep(x / 100)
  sqrt(x)
}

if (require("future.apply")) {
  message("*** ggjournals() ...")

  plan(sequential)
  js <- capture_journals({
    vs <- future_lapply(5:1, FUN = slow_fcn)
  })
  print(ggjournal(js))
  
  plan(multisession, workers = 2)
  js <- capture_journals({
    vs <- future_lapply(5:1, FUN = slow_fcn)
  })
  print(ggjournal(js))
  
  ## Shut down parallel workers
  plan(sequential)

  message("*** ggjournals() ... done")
}
