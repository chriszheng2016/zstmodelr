# Tests for helper functions for testthat ----

# context("Tests for helper functions for testthat")


test_that("expect_not_null, with various arguments", {

  # expect_not_null with default arguments ====
  expect_error(expect_not_null(NULL), regexp = "is null")
  expect_silent(expect_not_null("a"))

  # expect_not_null with various arguments ====
  expect_error(expect_not_null(NULL, info = "Info", label = "Object"),
               regexp = "Object is null.\nInfo")
  expect_silent(expect_not_null("a"))
})

test_that("skip_if_stock_db_not_ready, with various arguments", {

  # Skip following tests in environment without stock db
  skip_if_stock_db_not_ready()

  # skip_if_stock_db_not_ready with default arguments ====

  # A skip doesn't happen when stock db is able to be connected
  expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")

  # skip_if_stock_db_not_ready with various arguments ====

  # A skip happens when stock db is unable to be connected
  expect_condition(skip_if_stock_db_not_ready("GTA_SQLData1"), class = "skip")

  # Skip happen when "NO_STOCK_DB" is true)
  withr::with_envvar(new = c("NO_STOCK_DB" = "true"),{
    expect_condition(skip_if_stock_db_not_ready(), class = "skip")
  })

  # Skip doesn't happen when "NO_STOCK_DB" is not true)
  withr::with_envvar(new = c("NO_STOCK_DB" = ""),{
    expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")
  })
})
