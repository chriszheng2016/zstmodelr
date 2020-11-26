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

test_that("mode_value, with various arguments", {

  # mode_value on a numeric vector ====
  x <- c(rep(1,1), rep(2, 2), rep(3,3), rep(4,2), rep(5,1))
  expect_mode_val <- 3
  actual_mode_val <- mode_value(x)
  expect_equal(actual_mode_val, expect_mode_val)

  # mode_value on a charcater vector ====
  x <-c(rep("A",1), rep("B", 2), rep("C",3), rep("D",2), rep("E",1))
  expect_mode_val <- "C"
  actual_mode_val <- mode_value(x)
  expect_equal(actual_mode_val, expect_mode_val)

  # mode_value on a factor vector ====
  x <-c(rep("A",1), rep("B", 2), rep("C",3), rep("D",2), rep("E",1))
  x <- factor(x)
  expect_mode_val <- "C"
  actual_mode_val <- mode_value(x)
  expect_equal(actual_mode_val, expect_mode_val)

})

