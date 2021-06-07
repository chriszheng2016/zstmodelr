# Utility functions as the helpers for withr ----

#' @importFrom withr defer

# with/local helper for parallel ----

# Set function to enable parallel
set_parallel_state <- function(new_parallel_state = c("ON", "OFF")) {
  old_parallel_state <- ifelse(parallel_is_on(), "ON", "OFF")

  new_parallel_state <- match.arg(new_parallel_state)
  new_parallel_state <- stringr::str_to_upper(new_parallel_state)
  if (new_parallel_state == "ON") {
    if (old_parallel_state == "OFF") {
      suppressMessages(enable_parallel())
    }
  } else {
    if (old_parallel_state == "ON") {
      suppressMessages(disable_parallel())
    }
  }

  return(old_parallel_state)
}


#' Turn on/off Parallel process temporarily
#'
#' Temporarily turn on/off parallel process.
#'
#' @param new `[character(1)]` new parallel state: "ON" or "OFF".
#' @examples
#' \dontrun{
#'
#' with_parallel(new = "ON", {
#'   sprintf(
#'     "Parallel prcoess is %s.",
#'     ifelse(parallel_is_on(), "ON", "OFF")
#'   )
#' })
#'
#' with_parallel(new = "OFF", {
#'   sprintf(
#'     "Parallel prcoess is %s.",
#'     ifelse(parallel_is_on(), "ON", "OFF")
#'   )
#' })
#'
#' local_parallel("ON")
#' sprintf(
#'   "Parallel prcoess is %s.",
#'   ifelse(parallel_is_on(), "ON", "OFF")
#' )
#' local_parallel("OFF")
#' sprintf(
#'   "Parallel prcoess is %s.",
#'   ifelse(parallel_is_on(), "ON", "OFF")
#' )
#' }
#' @export
with_parallel <- withr::with_(set_parallel_state,
  reset = set_parallel_state,
  new = TRUE
)
#' @rdname with_parallel
#' @export
local_parallel <- withr::local_(set_parallel_state,
  reset = set_parallel_state,
  new = TRUE
)
