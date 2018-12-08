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

  actual_indcds <- match_indcds.gta_db(dates_stkcds_indcds$date,
                                dates_stkcds_indcds$stkcd,
                                ds_stock_industry)

  expect_indcds <- as.character(dates_stkcds_indcds$indcd)

  expect_equal(actual_indcds, expect_indcds)

})
