# unit testing for stock_db (generic class of stock database)

library(zstmodelr)
context("Tests related to stock_db class")

# set up testing context
stock_db <- stock_db(gta_db, "GTA_SQLData")
suppressMessages(open_stock_db(stock_db))

test_that("Open and close stock_db", {
  stock_db1 <- stock_db(gta_db, "GTA_SQLData")
  expect_is(stock_db1, "stock_db")
  expect_true(open_stock_db(stock_db1))
  expect_not_null(stock_db1$connection)
  expect_true(close_stock_db(stock_db1))
})

test_that("Init param of stock db", {
  expect_true(init_stock_db(stock_db))
  expect_error(init_stock_db(non_stock_db))
})

test_that("Translation between code and name", {
  expect_equal(name2code(stock_db, "资产报酬率A", type = "field"), "f050101b")
  expect_equal(code2name(stock_db, "f050101b", type = "field"), "资产报酬率A")
  expect_equal(name2code(stock_db, "三一重工", type = "stock"), 600031)
  expect_equal(code2name(stock_db, 600031, type = "stock"), "三一重工")
})

test_that("get_stock_return, with various arguments", {

})

test_that("get_stock_return, with various arguments", {

})


test_that("get_market_return, with various arguments", {

})

test_that("get_factor_indicator, with various arguments", {

})

test_that("get_factors_info_info, with various arguments", {

  # get_factors_info with default arguments ====
  ds_matched_factors <- get_factors_info(stock_db)
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  expected_fields <- c("factor_code", "factor_name", "factor_type", "factor_group")
  actual_fields   <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)

  # get_factors_info with one factor group ====
  factor_groups = "Operating Profitability"
  ds_matched_factors <- get_factors_info(stock_db,
                                    factor_groups = factor_groups)
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  actual_fields   <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)
  # fetched factor_groups
  expect_true(all(unique(ds_matched_factors$factor_group) %in% factor_groups))

  # get_factors_info with multi factor groups ====
  factor_groups = c("Operating Profitability", "Valuation")
  ds_matched_factors <- get_factors_info(stock_db,
                                    factor_groups = factor_groups)
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  actual_fields   <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)
  # fetched factor_groups
  expect_true(all(unique(ds_matched_factors$factor_group) %in% factor_groups))

  # get_factors_info with all factor groups ====
  ds_matched_factors <- get_factors_info(stock_db, factor_groups = NULL)
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  actual_fields   <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)

  # fetched factor_groups
  expect_gte(length(unique(ds_matched_factors$factor_group)), 0)
})


# clear up testing conext
suppressMessages(close_stock_db(stock_db))





