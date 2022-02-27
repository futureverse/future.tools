library(future)
plan(sequential)

t_start <- Sys.time()
fs <- lapply(5:1, FUN = function(x) future({
  Sys.sleep(x / 10)
  sqrt(x)
}))
vs <- value(fs)

gg <- ggjournal(fs, baseline = t_start)
print(gg)
