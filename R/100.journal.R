#' Gets the logged journal of events for a future
#'
#' _WARNING: This function is under development. It can change at any time.
#' For now, please, do not depend on this function in a published R package._
#'
#' @param x A [Future] object.
#'
## @param baseline (POSIXct; optional) A timestamp to server as time zero
## for the relative start time (`at`). If `TRUE` (default), then the
## earliest timepoint observed is used as the baseline.
#'
#' @param \ldots Not used.
#'
#' @return
#' A data frame of class `FutureJournal` with columns:
#'
#'  1. `event` (character string) - type of event that took place
#'  2. `category` (character string) - the category of the event
#'  3. `parent` (character string) - (to be describe)
#'  4. `start` (POSIXct) - the timestamp when the event started
#'  5. `at` (difftime) - the time when the event started relative to
#'     first event
#'  6. `duration` (difftime) - the duration of the event
#'  7. `future_label` (character string) - the label of the future 
#'  8. `future_uuid` (character string) - the UUID of the future
#'  9. `session_uuid` (character string) - the UUID of the R session
#'     where the event took place
#'
#' The common events are:
#'
#'  * `create`   - the future was created (an `overhead`)
#'  * `launch`   - the future was launched (an `overhead`)
#'  * `evaluate` - the future was evaluated (an `evaluation`)
#'  * `resolved` - the future was queried (may be occur multiple times)
#'                 (an `overhead`)
#'  * `gather`   - the results was retrieved (an `overhead`)
#'
#' but others may be added by other Future classes.
#'
#' Common event categorys are:
#'
#'  * `evaluation` - processing time is spent on evaluation
#'  * `overhead`   - processing time is spent on orchestrating the future
#'  * `waiting`    - processing time is spent on waiting to set up or
#'                   querying the future
#'
#' but others may be added by other Future classes.
#'
#' The data frame is sorted by the `at` time.
#' Note that the timestamps for the `evaluate` event are based on the local
#' time on the worker. The system clocks on the worker and the calling R
#' system may not be in perfect sync.
#'
#' @section Enabling and disabling event logging:
#' To enable logging of events, set option `future.journal` is TRUE.
#' To disable, set it to FALSE (default).
#'
#' @example incl/journal.R
#'
#' @seealso
#' Use [capture_journals()] to capture journals from all futures.
#'
#' @keywords internal
#' @export
journal <- import_future("journal")
