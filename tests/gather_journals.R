if (require("future.apply")) {
  source("incl/start.R")
  
  message("*** gather_journals() ...")
  
  slow_fcn <- function(x) {
    Sys.sleep(x / 100)
    sqrt(x)
  }
  
  plan(multisession, workers = 2)
  js <- gather_journals({
    vs <- future_lapply(3:1, FUN = slow_fcn)
  })
  print(js)
  stopifnot(is.data.frame(js))
  
  ## Shut down parallel workers
  plan(sequential)

  message("*** gather_journals() ... done")
  source("incl/end.R")
}
