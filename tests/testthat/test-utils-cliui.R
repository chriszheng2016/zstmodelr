# Tests for utility functions of cliui ----

skip_on_ci()

library(zstmodelr)

context("Tests for utility functions of cliui")

test_fun <- function(arg1,
                     arg2 = c("val_1", "val_2", "val_3"),
                     arg3 = FALSE,
                     arg4 = "DEFAULT") {
  cat(glue::glue("arg1:{arg1}"), "\n")
  cat(glue::glue("arg2:", glue::glue_collapse(arg2, sep = ",")), "\n")
  cat(glue::glue("arg3:{arg3}"), "\n")
  cat(glue::glue("arg4:{arg4}"), "\n")
}

# User's input or select in interaction
arg_choices <- c("val_1", "val_2", "val_3")
user_select_string <- arg_choices[c(2, 3)]
user_select_logical <- FALSE
user_select_quit <- character(0)
user_input_string <- "a, b, c"
user_input_number <- "1, 2, 3"
user_select_logical <- FALSE
user_input_quit <- "q"
user_input_enter <- ""

# Save namespace of pkg for restoration
pkg_namespace <- rlang::fn_env(arg_value)

# Notice:
#
# After reading code of mockery pkg, I have following findings about how to use
# mockery::stub:
#
# * rationale of stub:
#   Mockery::stub replace environment of target function('where', or
#   other functions created in same environment of target function ) with a new
#   environment with binding 'what' with 'how' and parent environment of
#   original environment of target function.
#
# * depth argument of mockery::stub():
#   depth 1: means only replace environment of target function('where') with a
#   new environment with binding of 'what' with 'how';
#   depth 2: means replace environment of all functions created in same
#   environment as target function with a new with binding of 'what'
#   with 'how'.
#
#   * Avoid side-effect of mockery::stub
#   Since mockery::stub change function environment of all function related to
#   target function, if you mockery::stub a function again whenever in same test
#   or another test after that, the changed function environment sometimes
#   cause mockery::stub to produce unpredictable result. In order to avoiding
#   such side-effect, you have to restore the function environment of target
#   function before mockery::stub, e.g.
#
#   Restore function environment to avoid influence of previous mockery::stub
#   rlang::fn_env(interactive_call) <- pkg_namespace
#
#   # mockery::sub target function
#   mockery::stub(interactive_call,
#                what = "user_input_values",
#                how = "hello", depth = 2
#

test_that("arg_value, with various arguments", {

  # arg_value on argument without a function  ====
  # >>arg_value on argument for user to select values ====

  # arg_value on argument for user to select values of string
  mockery::stub(arg_value, what = "user_select_values", how = user_select_string)
  expect_value <- user_select_string
  actual_value <- arg_value("x", choices = arg_choices)
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), "x")
  expect_equal(actual_value$x, expect_value)

  # arg_value on argument for user to select quit (ENTER)
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_select_values", how = user_select_quit)
  expect_error(
    {
      actual_value <- arg_value("x", choices = arg_choices)
    },
    regexp = "Abort without a value for argument"
  )

  # >>arg_value on argument for user to input values ====

  # arg_value on argument without function for user to input strings
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_string)

  expect_value <- stringr::str_split(user_input_string, pattern = ",")[[1]]
  expect_value <- stringr::str_trim(expect_value)
  expect_value <- readr::parse_guess(expect_value)
  actual_value <- arg_value("x")
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), "x")
  expect_equal(actual_value$x, expect_value)

  # arg_value on argument for user to input numbers
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_number)
  expect_value <- stringr::str_split(user_input_number, pattern = ",")[[1]]
  expect_value <- stringr::str_trim(expect_value)
  expect_value <- readr::parse_guess(expect_value)
  actual_value <- arg_value("x")
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), "x")
  expect_equal(actual_value$x, expect_value)

  # arg_value on argument for user to input a logic
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_select_logical)
  expect_value <- stringr::str_split(user_select_logical, pattern = ",")[[1]]
  expect_value <- stringr::str_trim(expect_value)
  expect_value <- readr::parse_guess(expect_value)
  actual_value <- arg_value("x")
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), "x")
  expect_equal(actual_value$x, expect_value)

  # arg_value on argument for user to input 'ENTER' for default value
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_enter)
  expect_value <- NULL
  actual_value <- arg_value("x")
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), "x")
  expect_equal(actual_value$x, expect_value)

  # arg_value on argument for user to input quit
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_quit)
  expect_value <- NULL
  expect_error(
    {
      actual_value <- arg_value("x")
    },
    regexp = "Abort without a value for argument"
  )

  # arg_value on argument with a function  ====
  # >>arg_value on argument for user to select values ====

  # arg_value on argument for user to select values
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_select_values", how = user_select_string)
  arg_name <- "arg2"
  expect_value <- user_select_string
  actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), arg_name)
  expect_equal(actual_value[[arg_name]], expect_value)

  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_select_values", how = user_select_logical)
  arg_name <- "arg3"
  expect_value <- user_select_logical
  actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), arg_name)
  expect_equal(actual_value[[arg_name]], expect_value)


  # arg_value on argument for user to select quit (ENTER)
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_select_values", how = user_select_quit)
  arg_name <- "arg3"
  expect_error(
    {
      actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
    },
    regexp = "Abort without a value for argument"
  )


  # >>arg_value on argument for user to input values ====

  # arg_value on argument without function for user to input strings
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_string)
  arg_name <- "arg4"
  expect_value <- stringr::str_split(user_input_string, pattern = ",")[[1]]
  expect_value <- stringr::str_trim(expect_value)
  expect_value <- readr::parse_guess(expect_value)
  actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), arg_name)
  expect_equal(actual_value[[arg_name]], expect_value)

  # arg_value on argument for user to input 'ENTER' for default value
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_enter)
  arg_name <- "arg1"
  expect_value <- NULL
  actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), arg_name)
  expect_equal(actual_value[[arg_name]], expect_value)

  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_enter)
  arg_name <- "arg4"
  expect_value <- "DEFAULT"
  actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
  expect_is(actual_value, class = "list")
  expect_equal(names(actual_value), arg_name)
  expect_equal(actual_value[[arg_name]], expect_value)

  # arg_value on argument for user to input quit
  rlang::fn_env(arg_value) <- pkg_namespace
  mockery::stub(arg_value, what = "user_input_values", how = user_input_quit)
  arg_name <- "arg4"
  expect_error(
    {
      actual_value <- arg_value(arg_name, quo_fun = rlang::quo(test_fun))
    },
    regexp = "Abort without a value for argument"
  )
})

test_that("args_values, with various arguments", {

  # args_values with default arguments ====

  # Simulate user's input or select
  mock_input_values <- mockery::mock(user_input_number, user_input_string,
    cycle = TRUE
  )
  mock_select_values <- mockery::mock(user_select_string, user_select_logical,
    cycle = TRUE
  )

  rlang::fn_env(args_values) <- pkg_namespace
  mockery::stub(args_values,
    what = "user_input_values",
    how = mock_input_values, depth = 2
  )

  rlang::fn_env(args_values) <- pkg_namespace
  mockery::stub(args_values,
    what = "user_select_values",
    how = mock_select_values, depth = 2
  )

  expect_values <- list(
    arg1 = as.numeric(unlist(
      stringr::str_split(user_input_number, pattern = ",")
    )),
    arg2 = user_select_string,
    arg3 = user_select_logical,
    arg4 = stringr::str_trim(unlist(
      stringr::str_split(user_input_string, pattern = ",")
    ))
  )
  actual_values <- args_values(rlang::quo(test_fun))
  expect_equal(actual_values, expect_values)
})

test_that("execute_fun, with various arguments", {

  # Simulate user's input or select
  mock_yes_no <- mockery::mock("Yes", "No", cycle = TRUE)
  rlang::fn_env(execute_fun) <- pkg_namespace
  mockery::stub(execute_fun, what = "user_select_values", how = mock_yes_no)


  # execute_fun with default arguments ====

  # User select "Yes"
  expect_message(
    {
      result <- execute_fun(rlang::quo(sum), args_values = list(x = 1:10))
    },
    regexp = "Run sum..."
  )

  # User select "No"
  expect_message(
    {
      result <- execute_fun(rlang::quo(sum), args_values = list(x = 1:10))
    },
    regexp = "Action is aborted"
  )

  # execute_fun with argument: quiet ====

  result <- execute_fun(rlang::quo(sum),
    args_values = list(x = 1:10),
    quiet = TRUE
  )
  expect_equal(result, sum(1:10))

  # execute_fun with argument: debug ====
  # User select "Yes"
  expect_message(
    {
      result <- execute_fun(rlang::quo(sum),
        args_values = list(x = 1:10),
        debug = TRUE
      )
    },
    regexp = "Simulate to run sum..."
  )

  # User select "No"
  expect_message(
    {
      result <- execute_fun(rlang::quo(sum),
        args_values = list(x = 1:10),
        debug = TRUE
      )
    },
    regexp = "Action is aborted"
  )
})

test_that("interactive_call, with various arguments", {

  # Simulate user's input or select

  # Restore function environment to avoid influence of previous mockery::stub
  rlang::fn_env(interactive_call) <- pkg_namespace
  mockery::stub(interactive_call,
    what = "user_input_values",
    how = "hello", depth = 2
  )

  # Restore function environment to avoid influence of previous mockery::stub
  rlang::fn_env(interactive_call) <- pkg_namespace
  mock_yes <- mockery::mock("Yes", cycle = TRUE)
  mockery::stub(interactive_call,
    what = "user_select_values",
    how = mock_yes, depth = 2
  )

  # interactive_call with default arguments ====

  expect_output(
    {
      result <- interactive_call(print)
    },
    regexp = "hello"
  )

  # interactive_call with arguments: quiet ====

  expect_output(
    {
      result <- interactive_call(print, quiet = TRUE)
    },
    regexp = "hello"
  )

  # interactive_call with arguments: debug ====
  expect_message(
    {
      result <- interactive_call(print, debug = TRUE)
    },
    regexp = "Simulate to run print..."
  )
})

test_that("interactive_call_, with various arguments", {

  # Simulate user's input or select

  rlang::fn_env(interactive_call_) <- pkg_namespace
  mockery::stub(interactive_call_,
    what = "user_input_values",
    how = "hello", depth = 2 # 3
  )

  rlang::fn_env(interactive_call_) <- pkg_namespace
  mock_yes <- mockery::mock("Yes", cycle = TRUE)
  mockery::stub(interactive_call_,
    what = "user_select_values",
    how = mock_yes, depth = 2
  )

  # interactive_call_ with default arguments ====

  expect_output(
    {
      result <- interactive_call_(rlang::quo(print))
    },
    regexp = "hello"
  )

  # interactive_call_ with arguments: quiet ====

  expect_output(
    {
      result <- interactive_call_(rlang::quo(print), quiet = TRUE)
    },
    regexp = "hello"
  )

  # interactive_call_ with arguments: debug ====
  expect_message(
    {
      result <- interactive_call_(rlang::quo(print), debug = TRUE)
    },
    regexp = "Simulate to run print..."
  )
})

test_that("interactive_fun, with various arguments", {

  # Simulate user's input or select

  rlang::fn_env(interactive_call_) <- pkg_namespace
  mockery::stub(interactive_call_,
    what = "user_input_values",
    how = "hello", depth = 2 # 3
  )

  rlang::fn_env(interactive_call_) <- pkg_namespace
  mock_yes <- mockery::mock("Yes", cycle = TRUE)
  mockery::stub(interactive_call_,
    what = "user_select_values",
    how = mock_yes, depth = 2
  )

  # interactive_fun with default arguments ====
  inter_print <- interactive_fun(print)
  expect_output(
    {
      result <- inter_print()
    },
    regexp = "hello"
  )

  # interactive_fun with arguments: quiet ====

  inter_print <- interactive_fun(print, quiet = TRUE)
  expect_output(
    {
      result <- inter_print()
    },
    regexp = "hello"
  )

  # interactive_fun with arguments: debug ====
  inter_print <- interactive_fun(print, debug = TRUE)
  expect_message(
    {
      result <- inter_print()
    },
    regexp = "Simulate to run print..."
  )
})

test_that("help_fun, with various arguments", {

  # help_fun with default arguments ====
  output <- capture_messages({
    help_fun(print)
  })

  expect_equal(length(output), 3)
  expect_true(stringr::str_detect(output[1], pattern = "print Help"))
  expect_true(stringr::str_detect(output[2], pattern = "print"))
  expect_true(stringr::str_detect(output[3],
    pattern = "please see document of print in help window..."
  ))


  # # help_fun with various arguments ====
  argument_desc <- "argument descption ..."
  examples <- "examples ...."

  output <- capture_output({
    help_fun(print, argument_desc = argument_desc, examples = examples)
  })

  expect_true(stringr::str_detect(output, pattern = argument_desc))
  expect_true(stringr::str_detect(output, pattern = examples))
})

test_that("help_fun_, with various arguments", {

  # help_fun_ with default arguments ====
  output <- capture_messages({
    help_fun_(rlang::quo(print))
  })

  expect_equal(length(output), 3)
  expect_true(stringr::str_detect(output[1], pattern = "print Help"))
  expect_true(stringr::str_detect(output[2], pattern = "print"))
  expect_true(stringr::str_detect(output[3],
    pattern = "please see document of print in help window..."
  ))

  # help_fun_ with various arguments ====
  argument_desc <- "argument descption ..."
  examples <- "examples ...."
  output <- capture_output({
    help_fun_(rlang::quo(print), argument_desc = argument_desc, examples = examples)
  })
  expect_true(stringr::str_detect(output, pattern = argument_desc))
  expect_true(stringr::str_detect(output, pattern = examples))
})
