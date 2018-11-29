# unit testing for stock_db (generic class of stock database)

# Tests for stock_db class - generic functions ----
context("Tests for stock_db class - generic functions")

# set up testing context
dsn <- "GTA_SQLData"
DB_PROFILE_FILE <- "gta_profile.xlsx"

stock_db <- stock_db(gta_db, dsn)
suppressMessages(db_ready <- open_stock_db(stock_db))
# skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for stock_db", dsn)
)
suppressMessages(init_stock_db(stock_db))


test_that("Open and close stock_db", {

  # open_stock_db/close_stock_db with default arguments ====
  stock_db1 <- stock_db(gta_db, "GTA_SQLData")
  expect_is(stock_db1, "stock_db")
  expect_true(open_stock_db(stock_db1))
  expect_not_null(stock_db1$connection)
  expect_true(close_stock_db(stock_db1))
})

test_that("get profile of stock db", {

  # get_profile with default arguments ====
  expect_true(file.exists(get_profile(stock_db)))

  # get_profile with various arguments ====
  expect_true(file.exists(get_profile(stock_db, DB_PROFILE_FILE)))
  expect_error(get_profile(stock_db, "invalid_profile.xlsx"))
})


test_that("Init param of stock db", {

  # init_stock_db with default arguments ====
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

test_that("get_indicators_from_source, with various arguments", {

  # get_indicators_from_source with default arguments ====
  ds_indicators <- get_indicators_from_source(stock_db,
                                              indicator_source = "TRD_Year")

  expect_fields <- c("date", "period", "stkcd", "ind_name", "ind_value")
  expect_true(all(expect_fields %in% names(ds_indicators)))
  expect_true(inherits(ds_indicators$date, "Date"))
  expect_true(unique(ds_indicators$period) == "year")
  expect_true(is.character(ds_indicators$stkcd))

  # get_indicators_from_source with various arguments: table of predfined indicator ====
  indicator_source <- "FR_T1"
  indicator_codes <- c("F010101A", "F010201A")

  ds_indicators <- get_indicators_from_source(stock_db,
    indicator_source = indicator_source,
    indicator_list = indicator_codes,
    ouput_format = "wide"
  )
  expect_fields <- c(
    "date", "period", "stkcd", "indcd",
    tolower(indicator_codes)
  )
  expect_true(all(expect_fields %in% names(ds_indicators)))
  expect_true(inherits(ds_indicators$date, "Date"))
  expect_true(unique(ds_indicators$period) == "quarter")
  expect_true(is.character(ds_indicators$stkcd))

  # get_indicators_from_source with various arguments: file of customized indicator ====


})

test_that("save_indicators_to_source, with various arguments", {

  # save_indicators_to_source with default arguments ====

  ts_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "month")
  ts_indicators <- tibble::tibble(date = ts_dates,
                                  ind1 = 1:12)
  indicator_source <- "test-ind01.rds"

  result <- save_indicators_to_source(stock_db,
                                      indicator_source = indicator_source,
                                      ts_indicators = ts_indicators)

  if ( result == TRUE) {
    path_indicators <- dir_path_db(stock_db,
                                dir_id = "DIR_DB_DATA_INDICATOR")
    path_indicator_source <- file.path(path_indicators, indicator_source)
    expect_true(file.exists(path_indicator_source))
  }

})

test_that("get_indicators, with various arguments", {

  # get_indicators with default arguments ====
  indicator_codes <- c("f010101a", "f010201a")
  ds_indicators <- get_indicators(stock_db, indicator_codes)
  expect_fields <- c("date", "period", "stkcd", "ind_name", "ind_value")
  expect_true(all(expect_fields %in% names(ds_indicators)))
  expect_true(all(ds_indicators$ind_name %in% tolower(indicator_codes)))
})

test_that("get_factors, with various arguments", {

  # get_factors with default arguments ====
  factor_codes <- c("FAT", "ROCE")
  ds_factors <- get_factors(stock_db, factor_codes)
  expect_fields <- c(
    "date", "period", "stkcd", "indcd",
    "factor_name", "factor_value"
  )
  expect_true(all(expect_fields %in% names(ds_factors)))
  expect_true(all(ds_factors$factor_name %in% factor_codes))

})

test_that("get_factors_info, with various arguments", {

  # get_factors_info with default arguments ====
  ds_matched_factors <- get_factors_info(stock_db)
  expect_is(ds_matched_factors, "data.frame")
  expected_fields <- c(
    "factor_code", "factor_name", "factor_type",
    "factor_group", "factor_description"
  )
  actual_fields <- names(ds_matched_factors)
  expect_equal(actual_fields, expected_fields)

  # get_factors_info with factor_codes ====
  factor_codes <- c("GPM", "OPM")
  ds_matched_factors <- get_factors_info(stock_db,
                                         factor_codes = factor_codes
  )
  if (!is.null(ds_matched_factors)) {
    expect_is(ds_matched_factors, "data.frame")
    actual_fields <- names(ds_matched_factors)
    expect_equal(actual_fields, expected_fields)
    expect_true(all(ds_matched_factors$factor_code %in% factor_codes))
  }

  # get_factors_info with factor_groups ====
  factor_groups <- c("Operating Profitability", "Valuation")
  ds_matched_factors <- get_factors_info(stock_db,
    factor_groups = factor_groups
  )
  if (!is.null(ds_matched_factors)) {
    expect_is(ds_matched_factors, "data.frame")
    actual_fields <- names(ds_matched_factors)
    expect_equal(actual_fields, expected_fields)
    expect_true(all(ds_matched_factors$factor_group %in% factor_groups))
  }

  # get_factors_info with factor_codes and factor_groups ====
  factor_codes <- c("GPM", "OPM")
  factor_groups <- c("Operating Profitability", "Valuation")
  ds_matched_factors <- get_factors_info(stock_db,
                                         factor_groups = factor_groups
  )
  if (!is.null(ds_matched_factors)) {
    expect_is(ds_matched_factors, "data.frame")
    actual_fields <- names(ds_matched_factors)
    expect_equal(actual_fields, expected_fields)
    expect_true(all(ds_matched_factors$factor_group %in% factor_groups))
  }
})

test_that("dir_path_db, with various arguments", {

  # dir_path with default arguments ====
  dir_id = c("DIR_DB_DATA",
             "DIR_DB_DATA_SOURCE",
             "DIR_DB_DATA_ORIGIN",
             "DIR_DB_DATA_LOG",
             "DIR_DB_DATA_INDICATOR")

  for (i in seq_along(dir_id)) {
     dir_path <- dir_path_db(stock_db, dir_id = dir_id[i])
     expect_true(dirname(dir_path) != "")
  }

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
  ts_stocks_return <- readRDS("./data/ts_stocks_return.rds")
  ts_market_return <- readRDS("./data/ts_market_return.rds")
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
  ds_trd_mnth.df <- readRDS("./data/ds_trd_mnth.df.rds")

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
  ds_trd_mnth.df <- readRDS("./data/ds_trd_mnth.df.rds")
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
