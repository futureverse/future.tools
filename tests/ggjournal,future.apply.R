if (require("future.apply")) {
  source("incl/start.R")
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
  source("incl/end.R")
}
