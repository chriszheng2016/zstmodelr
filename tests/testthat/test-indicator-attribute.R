# Tests for function of indicator attribute  ----
context("Tests for function of indicator attribute")

# set up testing context
dsn <- "GTA_SQLData"
DB_PROFILE_FILE <- "gta_profile.xlsx"

stock_db <- stock_db(gta_db, dsn)
suppressMessages(db_ready <- open_stock_db(stock_db))
withr::defer({
  close_stock_db(stock_db)
})
# skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for stock_db", dsn)
)
suppressMessages(init_stock_db(stock_db))

# Enable parallel process for test
if (is.null(parallel_status()$cluster)) {
  suppressMessages(enable_parallel())
  withr::defer({
    suppressMessages(disable_parallel())
  })
}

test_that("attr_indicators", {

  # define attribute
  attr_name <- "attr_01"
  attr_fun <- function(x, ...) {
    "attr_value"
  }
  new_attr_def <- create_attribute_def_fun(attr_name,
    attr_fun = attr_fun
  )

  # attr_indicators on ts_indicator of wide format ====

  # load indicators dataset to add attribute
  ts_indicators_wide <- readRDS("./data/ts_indicators_wide.rds")

  # add attribute
  ts_indicators_wide_with_attr <- attr_indicators(ts_indicators_wide,
    new_attr_def = new_attr_def
  )

  expect_is(ts_indicators_wide_with_attr, "data.frame")
  expect_fields <- c(names(ts_indicators_wide), attr_name)
  acutal_fields <- names(ts_indicators_wide_with_attr)
  expect_true(all(acutal_fields %in% expect_fields))
  expect_true(all(ts_indicators_wide_with_attr$attr_01 %in% "attr_value"))

  # attr_indicators on ts_indicator long format ====

  # load indicators dataset to add attribute
  ts_indicators_long <- readRDS("./data/ts_indicators_long.rds")

  # add attribute
  ts_indicators_long_with_attr <- attr_indicators(ts_indicators_long,
    new_attr_def = new_attr_def
  )

  expect_is(ts_indicators_long_with_attr, "data.frame")
  expect_fields <- c(names(ts_indicators_long), attr_name)
  acutal_fields <- names(ts_indicators_long_with_attr)
  expect_true(all(acutal_fields %in% expect_fields))
  expect_true(all(ts_indicators_long_with_attr$attr_01 %in% "attr_value"))
})

test_that("attr_indicators_indcd", {

  # attr_indicators_indcd on ts_indicator of wide format ====

  # load indicators dataset to add attribute
  ts_indicators_wide <- readRDS("./data/ts_indicators_wide.rds")

  # add attribute
  ts_indicators_wide_with_attr <- attr_indicators_indcd(stock_db,
    ts_indicators = ts_indicators_wide
  )

  expect_is(ts_indicators_wide_with_attr, "data.frame")
  expect_fields <- c(names(ts_indicators_wide), "indcd")
  acutal_fields <- names(ts_indicators_wide_with_attr)
  expect_true(all(acutal_fields %in% expect_fields))

  # attr_indicators_indcd on ts_indicator long format ====

  # load indicators dataset to add attribute
  ts_indicators_long <- readRDS("./data/ts_indicators_long.rds")

  # add attribute
  ts_indicators_long_with_attr <- attr_indicators_indcd(stock_db,
    ts_indicators = ts_indicators_long
  )

  expect_is(ts_indicators_long_with_attr, "data.frame")
  expect_fields <- c(names(ts_indicators_long), "indcd")
  acutal_fields <- names(ts_indicators_long_with_attr)
  expect_true(all(acutal_fields %in% expect_fields))
})

test_that("attr_indicators_trdstat", {

  # attr_indicators_trdstat on ts_indicator of wide format ====

  # load indicators dataset to add attribute
  ts_indicators_wide <- readRDS("./data/ts_indicators_wide.rds")

  # add attribute
  ts_indicators_wide_with_attr <- attr_indicators_trdstat(stock_db,
    ts_indicators = ts_indicators_wide
  )

  expect_is(ts_indicators_wide_with_attr, "data.frame")
  expect_fields <- c(names(ts_indicators_wide), "trdstat")
  acutal_fields <- names(ts_indicators_wide_with_attr)
  expect_true(all(acutal_fields %in% expect_fields))

  # attr_indicators_trdstat on ts_indicator long format ====

  # load indicators dataset to add attribute
  ts_indicators_long <- readRDS("./data/ts_indicators_long.rds")

  # add attribute
  ts_indicators_long_with_attr <- attr_indicators_trdstat(stock_db,
    ts_indicators = ts_indicators_long
  )

  expect_is(ts_indicators_long_with_attr, "data.frame")
  expect_fields <- c(names(ts_indicators_long), "trdstat")
  acutal_fields <- names(ts_indicators_long_with_attr)
  expect_true(all(acutal_fields %in% expect_fields))
})
