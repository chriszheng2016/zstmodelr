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
        warn_pattern = "o"
      )
    },
    regexp = "NaN"
  )
})
