# Tests for function of indicator define - non-generic functions of gta_db class ----
context("Tests for function of indicator define - non-generic functions of gta_db class")

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

# get stock industry info from stock_db
suppressMessages(
  ds_stock_industry <- get_stock_industry(stock_db)
)

test_that("build_indicator_defs.gta_db", {

  # get profile of stock_db
  gta_profile_name <- get_profile(stock_db)

  # get info of customized indicators from profile
  customized_indicators_info <- profile_get_customized_indicators(gta_profile_name)

  # build_indicator_defs.gta_db with default arguments ====
  indicator_defs <- build_indicator_defs.gta_db(stock_db, customized_indicators_info)

  expect_fields <- c(
    "ind_code", "ind_type", "ind_name", "ind_category",
    "ind_source", "ind_description", "ind_formula",
    "ind_keys", "rolling_window", "period", "output_format",
    "is_active", "ind_expr", "ind_def_fun", "ind_vars"
  )
  actual_fields <- names(indicator_defs)
  expect_true(all(actual_fields %in% expect_fields))

  expect_is(indicator_defs$ind_code, "character")
  expect_is(indicator_defs$ind_type, "character")
  expect_is(indicator_defs$ind_name, "character")
  expect_is(indicator_defs$ind_category, "character")
  expect_is(indicator_defs$ind_source, "character")
  expect_is(indicator_defs$ind_description, "character")
  expect_is(indicator_defs$ind_formula, "character")
  expect_is(indicator_defs$ind_keys, "list")
  expect_is(indicator_defs$rolling_window, "numeric")
  expect_is(indicator_defs$period, "character")
  expect_is(indicator_defs$output_format, "character")
  expect_is(indicator_defs$is_active, "logical")

  expect_is(indicator_defs$ind_expr, "list")
  expect_true(rlang::is_call(indicator_defs$ind_expr[[1]]))
  expect_is(indicator_defs$ind_def_fun, "list")
  expect_true(rlang::is_function(indicator_defs$ind_def_fun[[1]]))
  expect_is(indicator_defs$ind_vars, "list")
  expect_true(is.character(indicator_defs$ind_vars[[1]]))
})

test_that("find_stock_indcd.gta_db", {

  # find_stock_indcd.gta_db with default arguments ====
  stkcd <- "000829"

  start_date <- min(ds_stock_industry$date[ds_stock_industry$stkcd == stkcd])
  end_date <- Sys.Date()
  dates <- seq(start_date, end_date, by = "month")
  dates <- lubridate::ceiling_date(dates, unit = "month") - 1

  for (i in seq_len(length(dates))) {
    date <- dates[i]

    actual_indcd <- find_stock_indcd.gta_db(date, stkcd, ds_stock_industry)

    # expect record is the latest matched record
    ds_expect_indcd <- ds_stock_industry %>%
      dplyr::filter(stkcd == !!stkcd, date <= !!date) %>%
      dplyr::select(indcd, date)
    expect_indcd <- dplyr::last(
      ds_expect_indcd$indcd,
      ds_expect_indcd$date
    )

    expect_equal(actual_indcd, expect_indcd)
  }
})

test_that("find_stock_trdstat.gta_db", {

  # find_stock_trdstat.gta_db with default arguments ====
  # >> stock has record in spt_stocks ----
  stkcd <- "000004"
  suppressMessages(
    ds_spt_stocks <- get_spt_stocks(stock_db)
  )

  start_date <- min(ds_spt_stocks$date[ds_spt_stocks$stkcd == stkcd])
  end_date <- Sys.Date()
  dates <- seq(start_date, end_date, by = "month")
  dates <- lubridate::ceiling_date(dates, unit = "month") - 1

  for (i in seq_len(length(dates))) {
    date <- dates[i]
    actual_trdstat <- find_stock_trdstat.gta_db(date, stkcd, ds_spt_stocks)

    # expect record is the latest matched record
    ds_expect_trdstat <- ds_spt_stocks %>%
      dplyr::filter(stkcd == !!stkcd, date <= !!date) %>%
      dplyr::select(trade_status, date)
    expect_trdstat <- dplyr::last(
      ds_expect_trdstat$trade_status,
      ds_expect_trdstat$date
    )

    expect_equal(actual_trdstat, expect_trdstat)
  }

  # >> stock hasn't record in spt_stocks ----
  stkcd <- "600031"
  date <- lubridate::as_date("2018-9-30")
  suppressMessages(
    ds_spt_stocks <- get_spt_stocks(stock_db)
  )

  actual_trdstat <- find_stock_trdstat.gta_db(date, stkcd, ds_spt_stocks)
  expect_trdstat <- "list"
  expect_equal(actual_trdstat, expect_trdstat)
})

test_that("compute_attr_value.gta_db", {

  # compute_attr_value.gta_db with default arguments ====
  stkcds <- c("000829", "600066")

  dates_stkcds_indcds <- ds_stock_industry %>%
    dplyr::arrange(stkcd, date) %>%
    dplyr::filter(stkcd %in% !!stkcds) %>%
    dplyr::select(date, stkcd, indcd)

  actual_indcds <- compute_attr_value.gta_db(
    dates = dates_stkcds_indcds$date,
    stkcds = dates_stkcds_indcds$stkcd,
    find_stock_attr_fun = find_stock_indcd.gta_db,
    ds_attr_source = ds_stock_industry
  )

  expect_indcds <- as.character(dates_stkcds_indcds$indcd)

  expect_equal(actual_indcds, expect_indcds)
})


