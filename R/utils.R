# Utility functions - general tools

#' Supress specific warnings
#'
#' Just like \link{supressWarnings} which suppresses all warnings,
#' it can supresses spefiec warnings and outputs remain warnings.
#'
#' @param expr A expression to evaluate.
#' @param warn_pattern A character pattern as a regular expression.
#'  Default NA means suppressing all warnings.
#'
#' @return The warning message as character string, invisibly.
#'
#' @examples
#' suppress_warnings(
#'   {
#'     sqrt(-1)
#'     warning("ooops", call. = FALSE)
#'   },
#'   warn_pattern = "NaN"
#' )
#' suppress_warnings(
#'   {
#'     sqrt(-1)
#'     warning("ooops", call. = FALSE)
#'   },
#'   warn_pattern = "o"
#' )
#' @export
suppress_warnings <- function(expr, warn_pattern = NA) {
  if (!is.na(warn_pattern)) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (grepl(warn_pattern, conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  } else {
    suppressWarnings(expr)
  }
}
