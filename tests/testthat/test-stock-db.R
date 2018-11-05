# unit testing for stock_db (generic class of stock database)

library(zstmodelr)

# Tests for stock_db class - generic functions ----
context("Tests for stock_db class - generic functions")

# set up testing context
dsn <- "GTA_SQLData"
stock_db <- stock_db(gta_db, dsn)
suppressMessages(db_ready <- open_stock_db(stock_db))
# skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for stock_db", dsn)
)
suppressMessages(init_stock_db(stock_db))


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


test_that("list_stock_tables, with various arguments", {

  # list_stock_tables with default arguments ====
  db_tables <- list_stock_tables(stock_db)
  expect_gt(length(db_tables), 1)
})

test_that("get_table_dataset, with various arguments", {

  # get_table_dataset with default arguments ====
  table_name <- stock_db$table_list$TRD_Co
  expect_message(ds_table <- get_table_dataset(stock_db, table_name), "successfully")
  expect_gte(nrow(ds_table), 0)
  expect_true(is.character(ds_table[, "stkcd"]))
})

test_that("get_stock_dataset, with various arguments", {
  table_name <- stock_db$table_list$TRD_Co

  # get_stock_dataset with default arguments ====
  ds_stocks <- get_stock_dataset(stock_db, table_name)
  expect_gte(nrow(ds_stocks), 0)
  expect_true(is.character(ds_stocks$stkcd))

  # get_stock_dataset with customized arguments ====
  stock_stkcds_list <- c("600066", "000550", "600031", "000157", "000651", "000333")
  expect_message(ds_stocks <- get_stock_dataset(stock_db,
    table_name = table_name,
    stock_cd_list = stock_stkcds_list
  ), "successfully")
  expect_true(all((unique(ds_stocks$stkcd)) %in% stock_stkcds_list))
  expect_true(is.character(ds_stocks$stkcd))
})

test_that("fetch_table_dataset, with various arguments", {

  # fetch_table_dataset with default arguments ====
  table_list <- c(
    stock_db$table_list$TRD_Co,
    stock_db$table_list$gta_fieldname_list
  )
  expect_message(
    result_table_list <- fetch_table_dataset(stock_db, table_list),
    "successfully"
  )
  expect_length(result_table_list, length(table_list))
  for (i in seq_along(result_table_list)) {
    expect_is(get(result_table_list[[i]]), "data.frame")
  }
})

test_that("get_stock_return, with various arguments", {

  # get_stock_return with arguments: output_type ====
  stock_stkcds_list <- c("600066", "000550", "600031", "000157", "000651", "000333")
  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "month",
    period_date = "start",
    output_type = "timeSeries"
  )
  expect_is(ds_stock_return, "timeSeries")
  expect_true(all(names(ds_stock_return) %in% stock_stkcds_list))

  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "month",
    period_date = "start",
    output_type = "tibble"
  )
  expect_is(ds_stock_return, "tbl_df")
  expect_fields <- c("date", "stkcd", "return")
  expect_equal(names(ds_stock_return), expect_fields)
  expect_true(all(unique(ds_stock_return$stkcd) %in% stock_stkcds_list))
  expect_true(is.character(ds_stock_return$stkcd))

  # get_stock_return with arguments: period_type ====
  stock_stkcds_list <- c("600066")
  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "day",
    period_date = "start",
    output_type = "tibble"
  )
  # expect_true(timeDate::isDaily(timeDate::as.timeDate(ds_stock_return$date)))
  expect_true(mean(lag(ds_stock_return$date) - ds_stock_return$date, na.rm = TRUE) < 2)

  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "month",
    period_date = "start",
    output_type = "tibble"
  )
  expect_true(timeDate::isMonthly(timeDate::as.timeDate(ds_stock_return$date)))

  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "year",
    period_date = "start",
    output_type = "tibble"
  )
  expect_true(timeDate::frequency(timeDate::as.timeDate(ds_stock_return$date)) == 1)


  # get_stock_return with arguments: period_date ====
  stock_stkcds_list <- c("600066")
  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "month",
    period_date = "start",
    output_type = "tibble"
  )
  expect <- as.Date(timeDate::timeFirstDayInMonth
  (format(ds_stock_return$date, "%Y-%m-%d")))
  actual <- ds_stock_return$date
  expect_equivalent(expect, actual)

  ds_stock_return <- get_stock_return(stock_db,
    stock_cd_list = stock_stkcds_list,
    period_type = "month",
    period_date = "end",
    output_type = "tibble"
  )
  expect <- as.Date(timeDate::timeLastDayInMonth
  (format(ds_stock_return$date, "%Y-%m-%d")))
  actual <- ds_stock_return$date
  expect_equivalent(expect, actual)
})


test_that("get_market_return, with various arguments", {

  # get_market_return with arguments: output type ====
  ds_market_return <- get_market_return(stock_db,
    period_type = "month",
    period_date = "start",
    output_type = "timeSeries"
  )
  expect_is(ds_market_return, "timeSeries")
  expect_fields <- c("market_index")
  expect_equal(names(ds_market_return), expect_fields)

  ds_market_return <- get_market_return(stock_db,
    period_type = "month",
    period_date = "start",
    output_type = "tibble"
  )
  expect_is(ds_market_return, "tbl_df")
  expect_fields <- c("date", "market_index")
  expect_equal(names(ds_market_return), expect_fields)

  # get_market_return with arguments: period_type ====
  ds_market_return <- get_market_return(stock_db,
    period_type = "day",
    period_date = "start",
    output_type = "tibble"
  )
  # expect_true(timeDate::isDaily(timeDate::as.timeDate(ds_market_return$date)))
  expect_true(mean(lag(ds_market_return$date) - ds_market_return$date, na.rm = TRUE) < 2)

  ds_market_return <- get_market_return(stock_db,
    period_type = "month",
    period_date = "start",
    output_type = "tibble"
  )
  expect_true(timeDate::isMonthly(timeDate::as.timeDate(ds_market_return$date)))

  ds_market_return <- get_market_return(stock_db,
    period_type = "year",
    period_date = "start",
    output_type = "tibble"
  )
  expect_true(timeDate::frequency(timeDate::as.timeDate(ds_market_return$date)) == 1)

  # get_market_return with arguments: period_date ====
  ds_market_return <- get_market_return(stock_db,
    period_type = "month",
    period_date = "start",
    output_type = "tibble"
  )
  expect <- as.Date(timeDate::timeFirstDayInMonth
  (format(ds_market_return$date, "%Y-%m-%d")))
  actual <- ds_market_return$date
  expect_equivalent(expect, actual)

  ds_market_return <- get_market_return(stock_db,
    period_type = "month",
    period_date = "end",
    output_type = "tibble"
  )
  expect <- as.Date(timeDate::timeLastDayInMonth
  (format(ds_market_return$date, "%Y-%m-%d")))
  actual <- ds_market_return$date
  expect_equivalent(expect, actual)
})

test_that("get_factor_indicator, with various arguments", {

  # get_factor_indicator with default arguments ====
  factor_list <- c("FAT", "ROCE")
  ds_factors <- get_factor_indicator(stock_db, factor_list)
  expect_fields <- c("date", "periodtype", "stkcd", "indcd", "ROCE", "FAT")
  expect_true(all(names(ds_factors) %in% expect_fields))
  expect_true(is.character(ds_factors$stkcd))
})

test_that("get_factors_info, with various arguments", {

  # get_factors_info with default arguments ====
  ds_matched_factors <- get_factors_info(stock_db)
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  expected_fields <- c(
    "factor_code", "factor_name", "factor_type",
    "factor_group", "factor_description"
  )
  actual_fields <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)

  # get_factors_info with one factor group ====
  factor_groups <- "Operating Profitability"
  ds_matched_factors <- get_factors_info(stock_db,
    factor_groups = factor_groups
  )
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  actual_fields <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)
  # fetched factor_groups
  expect_true(all(unique(ds_matched_factors$factor_group) %in% factor_groups))

  # get_factors_info with multi factor groups ====
  factor_groups <- c("Operating Profitability", "Valuation")
  ds_matched_factors <- get_factors_info(stock_db,
    factor_groups = factor_groups
  )
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  actual_fields <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)
  # fetched factor_groups
  expect_true(all(unique(ds_matched_factors$factor_group) %in% factor_groups))

  # get_factors_info with all factor groups ====
  ds_matched_factors <- get_factors_info(stock_db, factor_groups = NULL)
  expect_is(ds_matched_factors, "data.frame")
  # data.frame fields
  actual_fields <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)

  # fetched factor_groups
  expect_gte(length(unique(ds_matched_factors$factor_group)), 0)
})

test_that("Translation between code and name", {
  expect_equal(name2code(stock_db, "资产报酬率A", type = "field"), "f050101b")
  expect_equal(code2name(stock_db, "f050101b", type = "field"), "资产报酬率A")
  expect_equal(name2code(stock_db, "三一重工", type = "stock"), "600031")
  expect_equal(code2name(stock_db, "600031", type = "stock"), "三一重工")
})

# Tests for stock_db class - non generic functions ----
context("Tests for stock_db class - non generic functions")

test_that("get_assets_return, with various arguments", {

  # get_assets_return with default arguments ====
  ts_stocks_return <- readRDS("ts_stocks_return.rds")
  ts_market_return <- readRDS("ts_market_return.rds")
  ts_assets_return <- get_assets_return(
    benchmark_return = ts_market_return,
    stocks_return = ts_stocks_return
  )
  expect_is(ts_assets_return, "timeSeries")
  expect_fields <- c(names(ts_stocks_return), names(ts_market_return))
  expect_true(all(names(ts_assets_return) %in% expect_fields))
  expect_start_date <- max(min(time(ts_stocks_return)), min(time(ts_market_return)))
  expect_end_date <- min(max(time(ts_stocks_return)), max(time(ts_market_return)))
  actual_start_date <- min(time(ts_assets_return))
  actual_end_date <- max(time(ts_assets_return))
  expect_equal(actual_start_date, expect_start_date)
  expect_equal(actual_end_date, expect_end_date)
})

test_that("get_stock_field_dataset, with various arguments", {

  # load test dataset
  ds_trd_mnth.df <- readRDS("ds_trd_mnth.df.RDS")

  # get_assets_return with default arguments: tseries_type = "timeSeries" ====
  ds_stock_mretnd.fts <- get_stock_field_dataset(
    ds_source.df = ds_trd_mnth.df,
    stock_cd = "600066",
    target_field = "mretnd",
    date_field = "trdmnt"
  )
  expect_is(ds_stock_mretnd.fts, "timeSeries")
  expect_equal(names(ds_stock_mretnd.fts), "600066")

  # get_assets_return with arguments: tseries_type = "xts" ====
  ds_stock_mretnd.xts <- get_stock_field_dataset(
    ds_source.df = ds_trd_mnth.df,
    stock_cd = "600066",
    target_field = "mretnd",
    date_field = "trdmnt",
    tseries_type = "xts"
  )
  expect_is(ds_stock_mretnd.xts, "xts")
  expect_equal(names(ds_stock_mretnd.xts), "600066")
})

test_that("fetch_stock_field_dataset, with various arguments", {

  # load test dataset
  ds_trd_mnth.df <- readRDS("ds_trd_mnth.df.RDS")
  stock_cd_list <- c("600066", "000550", "600031", "000157", "000651", "000333")

  ds_stocks_mretnd.fts <- fetch_stock_field_dataset(
    ds_source.df = ds_trd_mnth.df,
    stock_cd_list = stock_cd_list,
    target_field = "mretnd",
    date_field = "trdmnt"
  )
  expect_is(ds_stocks_mretnd.fts, "timeSeries")
  expect_fields <- stringr::str_c("X", stock_cd_list)
  actual_fields <- names(ds_stocks_mretnd.fts)
  expect_true(all(actual_fields %in% expect_fields))
})



# clear up testing conext
suppressMessages(close_stock_db(stock_db))
