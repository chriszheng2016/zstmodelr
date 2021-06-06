
# Tests for stock_db - non-generic internal functions of gta_db class ----
context("Tests for stock_db - non-generic internal functions of gta_db class")


# set up testing context
dsn <- "GTA_SQLData"
DB_PROFILE_FILE <- "gta_profile.xlsx"

skip_if_stock_db_not_ready(dsn)
stock_db <- stock_db(gta_db, dsn)
suppressMessages(open_stock_db(stock_db))
withr::defer({
  close_stock_db(stock_db)
})
suppressMessages(init_stock_db(stock_db))

test_that("stock_field_list.gta_db", {

  # stock_field_list.gta_db with default arguments ====
  stock_field_list <- stock_field_list.gta_db(stock_db)
  expect_is(stock_field_list, "code_name_list")
  expect_true(all(c("name", "code") %in% names(stock_field_list)))
  expect_true(is.character(stock_field_list$code))
  expect_true(is.character(stock_field_list$name))
})

test_that("stock_name_list.gta_db.gta_db", {

  # stock_name_list.gta_db with default arguments ====
  stock_name_list <- stock_name_list.gta_db(stock_db)
  expect_is(stock_name_list, "code_name_list")
  expect_true(all(c("name", "code") %in% names(stock_name_list)))
  expect_true(is.character(stock_name_list$code))
  expect_true(is.character(stock_name_list$name))
})

test_that("industry_name_list.gta_db", {

  # industry_name_list.gta_db with default arguments ====
  industry_name_list <- industry_name_list.gta_db(stock_db)
  expect_is(industry_name_list, "code_name_list")
  expect_true(all(c("name", "code") %in% names(industry_name_list)))
  expect_true(is.character(industry_name_list$code))
  expect_true(is.character(industry_name_list$name))
})
