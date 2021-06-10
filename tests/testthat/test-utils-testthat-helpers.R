# Tests for helper functions for testthat ----

# context("Tests for helper functions for testthat")


test_that("expect_not_null, with various arguments", {

  # expect_not_null with default arguments ====
  expect_error(expect_not_null(NULL), regexp = "is null")
  expect_silent(expect_not_null("a"))

  # expect_not_null with various arguments ====
  expect_error(expect_not_null(NULL, info = "Info", label = "Object"),
    regexp = "Object is null.\nInfo"
  )
  expect_silent(expect_not_null("a"))
})

test_that("skip_if_stock_db_not_ready, with various arguments", {

  # Skip on ci and cran
  skip_on_ci()
  skip_on_cran()

  # skip_if_stock_db_not_ready with real environment with database ====

  # Skip doesn't happen when stock db is able to be connected
  expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")

  # Skip happens when stock db is unable to be connected
  expect_condition(skip_if_stock_db_not_ready("GTA_SQLData1"), class = "skip")

  # skip_if_stock_db_not_ready in simulated environment without database  ====

  # Skip happens when "NO_STOCK_DB" is true)
  withr::with_envvar(new = c("NO_STOCK_DB" = "true"), {
    expect_condition(skip_if_stock_db_not_ready(), class = "skip")
  })

  # Skip doesn't happen when "NO_STOCK_DB" is not true)
  withr::with_envvar(new = c("NO_STOCK_DB" = ""), {
    expect_condition(skip_if_stock_db_not_ready(), NA, class = "skip")
  })
})

test_that("skip_on_ci_for_os, with various arguments", {

  # skip_on_ci_for_os with various arguments ====

  running_os_array <- c("windows", "darwin", "linux", "sunos")
  skip_os_array <- c("windows", "mac", "linux", "solaris")

  # Skip happens when CI on specific OS
  withr::with_envvar(new = c("CI" = "true"), {
    for (i in seq_along(running_os_array)) {
      skip_os <- skip_os_array[i]
      running_os <- running_os_array[i]
      names(running_os) <- "sysname"
      mockery::stub(skip_on_ci_for_os,
        what = "Sys.info",
        how = running_os
      )
      expect_condition(skip_on_ci_for_os(skip_os), class = "skip")
    }
  })

  # Skip doesn't happen when "NO_STOCK_DB" is not true)
  withr::with_envvar(new = c("CI" = ""), {

    for (i in seq_along(running_os_array)) {
      skip_os <- skip_os_array[length(skip_os_array)-i+1]
      running_os <- running_os_array[i]
      names(running_os) <- "sysname"
      mockery::stub(skip_on_ci_for_os,
                    what = "Sys.info",
                    how = running_os
      )
      expect_condition(skip_on_ci_for_os(skip_os), NA, class = "skip")
    }

  })
})
