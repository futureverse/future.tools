if (require("future")) {
  source("incl/start.R")
  message("*** ggjournals() ...")

  plan(sequential)
  js <- capture_journals({
    fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
    vs <- value(fs)
  })
  print(ggjournal(js))
  
  plan(multisession, workers = 2)
  js <- capture_journals({
    fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
    vs <- value(fs)
  })
  print(ggjournal(js))
  
  ## Decrease polling time to 0.01 seconds (default is 0.1 seconds)
  options(future.wait.interval = 0.01)
  js <- capture_journals({
    fs <- lapply(5:1, FUN = function(x) future(slow_fcn(x)))
    vs <- value(fs)
  })
  print(ggjournal(js))
  
  ## Shut down parallel workers
  plan(sequential)

  message("*** ggjournals() ... done")
  source("incl/end.R")
}
  
