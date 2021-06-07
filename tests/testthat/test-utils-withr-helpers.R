# Tests for helper functions for withr ----

# context("Tests for helper functions for withr")

test_that("with_parallel/local_parallel, with various arguments", {

  # with_parallel with various arguments ====
  old_parallel_is_on <- parallel_is_on()
  with_parallel(new = "ON", {
     expect_true(parallel_is_on())
  })
  expect_equal(old_parallel_is_on, parallel_is_on())

  with_parallel(new = "OFF", {
    expect_false(parallel_is_on())
  })
  expect_equal(old_parallel_is_on, parallel_is_on())

  # local_parallel with various arguments ====
  local_parallel("ON")
  expect_true(parallel_is_on())

  local_parallel("OFF")
  expect_false(parallel_is_on())

})
