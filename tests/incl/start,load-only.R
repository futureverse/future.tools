## Load namespace already here; it'll set some options based on
## environment variables
loadNamespace("future.tools")

## Record original state
ovars <- ls()
oenvs <- oenvs0 <- Sys.getenv()
oopts0 <- options()

covr_testing <- ("covr" %in% loadedNamespaces())
on_solaris <- grepl("^solaris", R.version$os)
on_macos <- grepl("^darwin", R.version$os)
on_githubactions <- as.logical(Sys.getenv("GITHUB_ACTIONS", "FALSE"))

## Default options
oopts <- options(
  warn = 1L,
  mc.cores = 2L,
  future.debug = FALSE,
  ## Reset the following during testing in case
  ## they are set on the test system
  future.availableCores.system = NULL,
  future.availableCores.fallback = NULL
)

## Comment: The below should be set automatically whenever the future package
## is loaded and 'R CMD check' runs.  The below is added in case R is changed
## in the future and we fail to detect 'R CMD check'.
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_CONNECTTIMEOUT = 2 * 60)
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_TIMEOUT = 2 * 60)
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_SESSIONINFO_PKGS = TRUE)
Sys.setenv(R_FUTURE_WAIT_INTERVAL = 0.01) ## 0.01s (instead of default 0.2s)

## Label PSOCK cluster workers (to help troubleshooting)
test_script <- grep("[.]R$", commandArgs(), value = TRUE)[1]
if (is.na(test_script)) test_script <- "UNKNOWN"
worker_label <- sprintf("future/tests/%s:%s:%s:%s", test_script, Sys.info()[["nodename"]], Sys.info()[["user"]], Sys.getpid())
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_RSCRIPT_LABEL = worker_label)

## Reset the following during testing in case
## they are set on the test system
oenvs2 <- Sys.unsetenv(c(
  "R_PARALLELLY_AVAILABLECORES_SYSTEM",
  "R_PARALLELLY_AVAILABLECORES_FALLBACK",
  ## SGE
  "NSLOTS", "PE_HOSTFILE",
  ## Slurm
  "SLURM_CPUS_PER_TASK",
  ## TORQUE / PBS
  "NCPUS", "PBS_NUM_PPN", "PBS_NODEFILE", "PBS_NP", "PBS_NUM_NODES"
))

oplan <- future::plan()

## Use eager futures by default
future::plan("sequential")

fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")
isWin32 <- (.Platform$OS.type == "windows" && .Platform$r_arch == "i386")


## Local functions for test scripts
hpaste <- future:::hpaste
capture.output <- utils::capture.output 
printf <- function(...) cat(sprintf(...))
mstr <- function(...) message(paste(capture.output(str(...)), collapse = "\n"))
attachLocally <- function(x, envir = parent.frame()) {
  for (name in names(x)) {
    assign(name, value = x[[name]], envir = envir)
  }
}

supportedStrategies <- function(cores = NA_integer_, excl = "cluster", ...) {
  strategies <- future:::supportedStrategies(...)
  strategies <- setdiff(strategies, excl)
  
  if (!is.na(cores)) {
    if (cores == 1L) {
      strategies <- setdiff(strategies, c("multicore", "multisession"))
    } else if (cores > 1L) {
      strategies <- setdiff(strategies,
                            c("sequential", "uniprocess", "eager", "lazy"))
    }
  }
  
  strategies
}

availCores <- min(2L, future::availableCores())


slow_fcn <- function(x) {
  Sys.sleep(x / 100)
  sqrt(x)
}

