# Tests for utility functions of general tools ----

context("Tests for utility functions of general tools")

test_that("suppress_warnings, with various arguments", {

  # suppress_warnings on default arguments  ====
  expect_silent({
    suppress_warnings({
      sqrt(-1)
      warning("ooops", call. = FALSE)
    })
  })

  # suppress_warnings on various arguments ====
  expect_warning(
    {
      suppress_warnings(
        {
          sqrt(-1)
          warning("ooops", call. = FALSE)
        },
        warn_pattern = "NaN"
      )
    },
    regexp = "ooops"
  )

  expect_warning(
    {
      suppress_warnings(
        {
          sqrt(-1)
          warning("ooops", call. = FALSE)
        },
        warn_pattern = "op"
      )
    },
    regexp = "NaN"
  )
})

test_that("print_fn_envs, with various arguments", {

  withr::local_output_sink(tempfile())

  # print_fn_envs on default arguments  ====
  output <- capture_messages({
    fun_name <- "print"
    print_fn_envs(print)
  })
  expect_true(stringr::str_detect(output[1],
    pattern = glue::glue("current env of {fun_name}")
  ))
  expect_true(stringr::str_detect(output[2],
    pattern = glue::glue("parents of current env of {fun_name}")
  ))
})

test_that("print_env_chain, with various arguments", {

  withr::local_output_sink(tempfile())

  # print_env_chain on default arguments  ====
  output <- capture_messages({
    print_env_chain(baseenv())
  })
  expect_true(stringr::str_detect(output[1], pattern = "current env"))
  expect_true(stringr::str_detect(output[2], pattern = "parents of current env"))

  # print_env_chain on various arguments ====
  output <- capture_messages({
      env_name <- "baseenv"
      print_env_chain(baseenv(), env_name = env_name)
  })
  expect_true(stringr::str_detect(output[1],
    pattern = glue::glue("current env of {env_name}")
  ))
  expect_true(stringr::str_detect(output[2],
    pattern = glue::glue("parents of current env of {env_name}")
  ))
})
