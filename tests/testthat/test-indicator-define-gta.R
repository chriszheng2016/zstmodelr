# Tests for function of indicator define - non-generic functions of gta_db class ----
context("Tests for function of indicator define - non-generic functions of gta_db class")

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

# get stock industry info from stock_db
suppressMessages(
  ds_stock_industry <- get_stock_industry(stock_db)
)

test_that("build_indicator_defs.gta_db", {

  # get profile of stock_db
  gta_profile_name <- get_profile(stock_db)

  # get info of customized indicators from profile
  customized_indictors_info <- profile_get_customized_indicators(gta_profile_name)

  # build_indicator_defs.gta_db with default arguments ====
  indicator_defs <- build_indicator_defs.gta_db(stock_db, customized_indictors_info)

  expect_fields <- c("ind_code", "ind_type", "ind_name", "ind_category",
                     "ind_source", "ind_description", "ind_formula",
                     "ind_keys", "rolling_window", "period", "output_format",
                     "is_active", "ind_expr", "ind_def_fun", "ind_vars")
  actual_fields <- names(indicator_defs)
  expect_true(all(actual_fields %in% expect_fields))
  expect_is(indicator_defs$ind_expr, "list")
  expect_true(rlang::is_call(indicator_defs$ind_expr[[1]]))
  expect_is(indicator_defs$ind_def_fun, "list")
  expect_true(rlang::is_function(indicator_defs$ind_def_fun[[1]]))
  expect_is(indicator_defs$ind_vars, "list")
  expect_true(is.character(indicator_defs$ind_vars[[1]]))

})

test_that("find_indcd.gta_db", {

  # find_indcd.gta_db with default arguments ====
  date <- as.Date("2018-9-30")
  stkcd <- "000829"

  actual_indcd <- find_indcd.gta_db(date, stkcd, ds_stock_industry)

  expect_indcd <- ds_stock_industry %>%
    dplyr::filter(stkcd == !!stkcd, date == !!date) %>%
    dplyr::select(indcd) %>%
    as.character()

  expect_equal(actual_indcd, expect_indcd)
})

test_that("match_indcds.gta_db", {

  # find_indcd.gta_db with default arguments ====
  stkcds <- c("000829", "600066")

  dates_stkcds_indcds <- ds_stock_industry %>%
    dplyr::arrange(stkcd, date) %>%
    dplyr::filter(stkcd %in% !!stkcds) %>%
    dplyr::select(date, stkcd, indcd)

  actual_indcds <- match_indcds.gta_db(
    dates_stkcds_indcds$date,
    dates_stkcds_indcds$stkcd,
    ds_stock_industry
  )

  expect_indcds <- as.character(dates_stkcds_indcds$indcd)

  expect_equal(actual_indcds, expect_indcds)
})
