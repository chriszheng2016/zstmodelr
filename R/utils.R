# Utility functions - general tools

#' Suppress specific warnings
#'
#' Unlike [base::suppressWarnings()] which suppresses all warnings, it can
#' suppress some specific warnings and output remain warnings.
#'
#' @param expr A expression to evaluate.
#' @param warn_pattern A character pattern as a regular expression. Default NA
#'   means suppressing all warnings.
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

# Print environments of a function
print_fn_envs <- function(fn) {

  # Validate params
  assertive::assert_is_function(fn)

  fun_env <- rlang::fn_env(fn)
  fun_name <- as.character(substitute(fn))

  print_env_chain(fun_env, env_name = fun_name)

}

# Print chain of environments
print_env_chain <- function(env, env_name = NULL) {

  #Validate params
  assertive::assert_is_environment(env)
  if(!is.null(env_name)) assertive::assert_is_character(env_name)

  # Display information about current environment
  if(!is.null(env_name)){
    cli::cli_rule(center = " * current env of {env_name} * ")
  } else {
    cli::cli_rule(center = " * current env * ")
  }
  rlang::env_print(env)

  # Display information about parent environment of functions
  if(!is.null(env_name)){
    cli::cli_rule(center = " * parents of current env of {env_name} * ")
  } else {
    cli::cli_rule(center = " * parents of current env * ")
  }
  parent_envs <- rlang::env_parents(env)
  parent_envs %>%
    purrr::map(.f = rlang::env_print)

}

#' get mode value of a vector
#'
#' @param x  A vector of numbers, characters, factors
#'
#' @return A value with mode numbers
#'
#' @examples
#' \dontrun{
#'
#'  # Get mode value of a vector of numeric
#'  data <- c(1, 2, 2, 3, 3, 3, 4, 4, 5)#'
#'  mode_value(data)
#'
#'  # Get model value of a vector of character
#'  data <- c("A", "B", "B", "C", "C", "C", "D", "D", "E")
#'  mode_value(data)
#'
#'  # Get model value of a vector of factor
#'  data <- as.factor(c("A", "B", "B", "C", "C", "C", "D", "D", "E"))
#'  mode_value(data)
#'
#' }
#'
#' @export
mode_value <- function(x) {

  assertive::assert_any_are_true(is.vector(x)|is.factor(x))

  mode_value <- names(which.max(table(x)))

  if(is.numeric(x)) {
    mode_value <- as.numeric(mode_value)
  }

  mode_value
}
