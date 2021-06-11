# Utility functions - Command Line Interactive User Interface(cliui)


#' Prompt user to input value for a argument
#'
#' @param arg_name  a character name of argument.
#' @param choices vector of values to be chose for argument. Default NULL means
#'   to use information in argument `fun` to get value of `choices`.
#' @param quo_fun a function quoted by [rlang::quo()] or [rlang::enquo()] to get
#'   input values of arguments.
#'
#' @return a list of a argument.
#'
#' @family utils_cliui
#'
#' @noRd

arg_value <- function(arg_name, choices = NULL, quo_fun = NULL) {

  # Validate params
  assertive::assert_is_character(arg_name)
  assertive::assert_is_non_empty(arg_name)
  if (!is.null(quo_fun)) {
    assertive::assert_is_inherited_from(quo_fun, c("quosure", "formula"))
  }

  fun_name <- rlang::as_label(quo_fun)
  fun <- rlang::eval_tidy(quo_fun)

  if (!is.null(fun)) {
    assertive::assert_is_function(fun)
  } else {
    # use parent call as the fun
    fun <- sys.function(sysP <- sys.parent())
  }

  # Get value of a argument
  arg_value <- NULL
  no_default <- "NO_DEFAULT"
  if (arg_name != "...") {

    # build candidate list of choice
    if (is.null(choices)) {
      eval_tidy_with_dfault <- purrr::possibly(rlang::eval_tidy,
        otherwise = no_default,
        quiet = TRUE
      )
      formal.args <- formals(fun)
      choices <- eval_tidy_with_dfault(
        formal.args[[arg_name]],
        env = parent.frame()
      )
      if (is.logical(choices)) {
        choices <- c(FALSE, TRUE)
      }
    }

    # prompt user to choose a value for argument
    if (length(choices) > 1) {
      cli::cli_rule(center = " * select value for {arg_name} * ")
      arg_value <- user_select_values(choices, multiple = TRUE)
    } else {
      # ask user input new value when choice is NULL or choice with 1 default value
      # value
      cli::cli_rule(center = " * input value for {arg_name} * ")
      default_value <- ifelse(is.null(choices), "NULL", choices)
      cli::cli_alert_info("{arg_name} default: {default_value}
                          input a value or multiple values separate by ',', or 'q' to cancel.")
      user_input <- user_input_values()
      if (user_input[1] != "") {
        if (user_input[1] != "q") {
          arg_value <- unlist(stringr::str_split(user_input, ","))
          arg_value <- stringr::str_trim(arg_value)
          arg_value <- readr::parse_guess(arg_value)
          if (stringr::str_to_upper(arg_value)[1] == "NULL") {
            arg_value <- NULL
          }
        } else {
          # user input "q" to cancel input
          arg_value <- character(0)
        }
      } else {
        # when user input "", we should use choices as default value
        # notice: NULL value is still valid since NULL could be default value
        arg_value <- choices
      }
    }

    # notify selection results
    if ((is.null(arg_value)) || (length(arg_value) > 0)) {
      if (is.null(arg_value)) {
        input_arg_value <- "NULL"
      } else {
        if (arg_value[1] != no_default) {
          input_arg_value <- arg_value
        } else {
          input_arg_value <- character(0)
        }
      }
      input_arg_value <- paste0(input_arg_value, collapse = ",")
      cli::cli_alert_success("selected {arg_name}: {.strong {input_arg_value}}\n")
    } else {
      if (!is.null(quo_fun)) {
        # only character(0) means cancel select/input value
        cli::cli_alert_warning("please see function {.strong usage} carefully.")
        help_fun_(quo_fun)
      }

      rlang::abort("Abort without a value for argument.\n")
    }

    if ((!is.null(arg_value)) && (arg_value[1] == no_default)) {
      arg_value <- rlang::list2(!!arg_name := rlang::missing_arg())
    } else {
      arg_value <- rlang::list2(!!arg_name := arg_value)
    }
  }

  arg_value
}



#' Prompt user to input values for arguments of a function
#'
#' @param quo_fun a function quoted by [rlang::quo()] or [rlang::enquo()] to get
#'   input values of arguments.
#' @param arg_value_fun a function used for getting value for a argument,
#'   default `arg_value` to get a value for a argument.
#'
#' @return a list of multiple arguments.
#'
#' @family utils_cliui
#'
#' @noRd

args_values <- function(quo_fun, arg_value_fun = arg_value) {

  # Validate params
  if (!is.null(quo_fun)) {
    assertive::assert_is_inherited_from(quo_fun, c("quosure", "formula"))
  }
  assertive::assert_is_function(arg_value_fun)

  fun_name <- rlang::as_label(quo_fun)
  fun <- rlang::eval_tidy(quo_fun)

  # Get values for all arguments of fun
  args_defs <- formals(fun)
  args_names <- names(args_defs)
  args_values <- purrr::map(args_names,
    .f = arg_value_fun,
    quo_fun = quo_fun
  )
  args_values <- purrr::reduce(args_values, .f = c)

  args_values
}

#' Execute a function with list of arguments
#'
#' @param quo_fun a function quoted by [rlang::quo()] or [rlang::enquo()] to be
#'   executed.
#' @param args_values a list of multiple arguments.
#' @param quiet a logical of whether display interactive message. Default FALSE
#'   means display interactive message in execution.
#' @param debug a logical of whether execute `fun` in simulation mode or real
#'   mode. Default FALSE means to execute function in real mode.
#'
#' @return return value from `fun` if `debug = TRUE`, otherwise NULL.
#'
#' @family utils_cliui
#'
#' @noRd
execute_fun <- function(quo_fun, args_values, quiet = FALSE, debug = FALSE) {

  # Validate params
  assertive::assert_is_inherited_from(quo_fun, classes = c("quosure", "formula"))
  assertive::assert_is_not_null(args_values)
  assertive::assert_is_inherited_from(args_values, classes = "list")

  fun_name <- rlang::as_label(quo_fun)
  fun <- rlang::eval_tidy(quo_fun)

  # Execute fun with arguments
  result <- NULL
  if (quiet) {
    if (!debug) {
      # rlang::eval_tidy(action)
      result <- do.call(fun, args = args_values)
    }
  } else {
    action <- rlang::expr(fun(!!!args_values))
    action_str <- rlang::as_label(action)
    action_str <- stringr::str_replace(action_str,
      pattern = "fun",
      replacement = fun_name
    )
    cli::cli_code(format(rlang::parse_expr(action_str)))
    user_answer <- user_select_values(c("Yes", "No"),
      title = "It may take long time. Are you sure to execute?"
    )
    if (user_answer == "Yes") {
      if (!debug) {
        cli::cli_alert_info("Run {fun_name}... ")
        # rlang::eval_tidy(action)
        result <- do.call(fun, args = args_values)
        cli::cli_alert_success("{fun_name} done.")
      } else {
        cli::cli_alert_info("Simulate to run {fun_name}... ")
        Sys.sleep(3)
        cli::cli_alert_success("{fun_name} done.")
      }
    } else {
      cli::cli_alert_warning("Action is aborted.")
    }
  }

  invisible(result)
}


#' Call a function interactively
#'
#' Since calling a function needs arguments, we should provide values explicitly
#' to a argument without default value. Except that, we could call it
#' interactively, i.e. we could ask user to provide value for each argument
#' during execution of function.
#'
#' @param fun a function to be executed.
#' @param quiet a logical of whether display interactive message. Default FALSE
#'   means display interactive message in execution.
#' @param debug a logical of whether execute fun in simulation mode or real
#'   mode. Default FALSE means to execute function in real mode.
#' @return return value from `fun` if `debug = TRUE`, otherwise NULL.
#' @export
#'
#' @family utils_cliui
#'
#' @examples
#' \dontrun{
#'
#' # NSE examples -- Usage in interactive mode
#'
#' # Call a function by prompting user to provide values for arguments
#' interactive_call(print)
#'
#' # Call a function interactively which execute without displaying information
#' interactive_call(print, quiet = TRUE)
#'
#' # Call a function interactively which execute it in debug mode
#' interactive_call(print, debug = TRUE)
#'
#' # SE examples -- Usage in program mode
#'
#' # Program with SE version function
#' inter_call <- function(fun) {
#'   interactive_call_(rlang::quo(fun), quiet = TRUE)
#' }
#'
#' inter_call(print)
#' }
#'
#' # NSE version of function
interactive_call <- function(fun, quiet = FALSE, debug = FALSE) {
  interactive_call_(
    quo_fun = rlang::enquo(fun),
    quiet = quiet,
    debug = debug
  )
}

# SE version of function
#' @param quo_fun a function quoted by [rlang::quo()] or [rlang::enquo()] to be
#'   executed.
#' @rdname interactive_call
#' @export
interactive_call_ <- function(quo_fun, quiet = FALSE, debug = FALSE) {

  # Validate params
  assertive::assert_is_inherited_from(quo_fun, c("quosure", "formula"))

  # Prompt user to input values for arguments
  args_values <- args_values(quo_fun)

  # Execute function by input values of arguments
  result <- execute_fun(quo_fun, args_values = args_values, quiet = quiet, debug = debug)

  invisible(result)
}

#' Turn a function into interactive function
#'
#' A function normally needs be provided value for arguments to be called. We
#' could turn a normal function into a function with ability to be called
#' interactively, i.e. we could call it interactively by asking user to provide
#' value for each argument during execution.
#'
#' @param fun a function to be executed.
#' @param quiet a logical of whether display interactive message. Default FALSE
#'   means display interactive message in execution.
#' @param debug a logical of whether execute fun in simulation mode or real
#'   mode. Default FALSE means to execute function in real mode.
#'
#' @return a function which could be called in interactive mode.
#'
#' @family utils_cliui
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' # Turn print() into interactive function and call it interactively
#' inter_print <- interactive_fun(print)
#' inter_print()
#'
#'
#' # Turn print() into interactive function and call it interactively,
#' # execute it without displaying information
#' inter_print <- interactive_fun(print, quiet = TRUE)
#' inter_print()
#'
#' # Turn print() into interactive function and call it interactively,
#' # execute it in debug mode
#' inter_print <- interactive_fun(print, debug = TRUE)
#' inter_print()
#' }
interactive_fun <- function(fun, quiet = FALSE, debug = FALSE) {
  inter_fun <- purrr::partial(
    .f = interactive_call_,
    quo_fun = rlang::enquo(fun),
    quiet = quiet,
    debug = debug
  )

  inter_fun
}

#' Display usage of function
#'
#' Display help for a function by arguments usage and examples.
#'
#' @param fun a function to be display usage.
#' @param argument_desc a character arguments description.
#' @param examples a character examples.
#'
#' @return standard output
#' @family utils_cliui
#'
#' @export

# NSE version function
help_fun <- function(fun,
                     argument_desc = NULL,
                     examples = NULL) {
  help_fun_(
    quo_fun = rlang::enquo(fun),
    argument_desc = argument_desc,
    examples = examples
  )
}

# SE version of function
#' @param quo_fun a function quoted by [rlang::quo()] or [rlang::enquo()] to be
#'   display usage.
#' @rdname help_fun
#' @export
help_fun_ <- function(quo_fun,
                      argument_desc = NULL,
                      examples = NULL) {

  # Validate params
  assertive::assert_is_inherited_from(quo_fun, c("quosure", "formula"))

  fun_name <- rlang::as_label(quo_fun)
  fun <- rlang::eval_tidy(quo_fun)

  cli::cli_rule(center = "{fun_name} Help")
  fun_str <- rlang::expr_text(args(fun))
  fun_str <- stringr::str_replace(fun_str, "function", fun_name)
  fun_str <- stringr::str_remove(fun_str, "NULL$")
  cli::cli_code(fun_str)

  # usage of arguments
  if (!is.null(argument_desc)) {
    cli::cli_rule(center = "Arguments")
    # cat(argument_desc, "\n")
    cli::cat_line(argument_desc)
  }

  # usage of examples
  if (!is.null(examples)) {
    cli::cli_rule(center = "Examples")
    # cat(examples, "\n")
    cli::cat_line(examples)
  }

  # display help doc
  if (is.null(argument_desc) || is.null(examples)) {
    cli::cli_alert_info("please see document of {fun_name} in help window...")
    # help(topic = fun_name)
  }
}

# User select values from console - easy for mock testing
user_select_values <- function(choices, multiple = FALSE, title = NULL) {
  utils::select.list(choices, multiple = multiple, title = title)
}

# User input values to console - easy for mock testing
user_input_values <- function() {
  readline(prompt = ">")
}
