import_from <- function(name, default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    get(name, mode = "function", envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stopf("No such '%s' function: %s()", package, name)
  }
}

## ## dummy import to please 'R CMD check'
#' @importFrom future future
import_future <- function(name, default = NULL) {
  import_from(name, default = default, package = "future")
}
