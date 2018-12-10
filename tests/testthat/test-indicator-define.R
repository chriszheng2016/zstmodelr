# Tests for function of indicator define - generic functions ----
context("Tests for function of indicator define - generic functions")

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

# prepare test datasets

test_indicator_defs <- tibble::tibble(
  ind_code = "m_ep_ttm",
  ind_type = "decimal",
  ind_name = "Month_EP_TTM",
  ind_category = "customized",
  ind_source = "m_ep_ttm.csv",
  ind_description = "Monthly TTM EP ratio ",
  ind_formula = "price <- dplyr::lag(mclsprc, n=2)
                 earning <- f090101c
                 ep <- earning/price",
  ind_keys = list(c("stkcd")),
  rolling_window = 0,
  period = "month",
  output_format = "csv",
  is_active = TRUE,
  ind_expr = list(NA),
  ind_def_fun = list(NA),
  ind_vars = list(NA)
)

# dates series
day_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "day")
month_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "month")
month_dates <- lubridate::ceiling_date(month_dates, unit = "month") - 1
quarter_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "quarters")
quarter_dates <- lubridate::ceiling_date(quarter_dates, unit = "quarters") - 1
year_dates <- seq(as.Date("2001/1/1"), as.Date("2018/1/1"), by = "years")
year_dates <- lubridate::ceiling_date(year_dates, unit = "years") - 1

# date list for building test ds_vars
list_dates <- list(day = day_dates,
                   month = month_dates,
                   quarter = quarter_dates,
                   year = year_dates)


# function to build ds_vars for test
test_ds_vars <- function(dates, peroid) {

  ds_vars <- tibble::tibble(
    date = rep(dates, 2),
    period = peroid,
    stkcd = "stkcd_01",
    indcd = "incd_01",
    ind_name = c(rep("f090101c", length(dates)),
                 rep("mclsprc", length(dates))),
    ind_value = c(rep(1, length(dates)),
                  rep(2,  length(dates)))
    # ind_value = c(runif(length(dates)),
    #               runif(length(dates)))
  )

  return(ds_vars)

}

# build list of ds_vars for test
list_ds_vars <- purrr::map2(list_dates, names(list_dates), test_ds_vars)


test_that("get_indicator_defs", {

  # get_indicator_defs with default arguments ====
  ind_defs <- get_indicator_defs(stock_db)
  if (!is.null(ind_defs)) {
    expect_is(ind_defs, "data.frame")
    expect_fields <- names(test_indicator_defs)
    actual_fields <- names(ind_defs)
    expect_true(all(actual_fields %in% expect_fields))
  }

})

test_that("parse_indicator_vars", {

  # parse_indicator_vars with default arguments ====
  test_expr <- create_expr(!!test_indicator_defs$ind_formula)
  syms_expr <- find_syms(!!test_expr)
  vars <- parse_indicator_vars(stock_db, test_expr)
  expect_true(all(vars %in% syms_expr))

})

test_that("get_indicator_vars", {

  # get_indicator_vars with default arguments ====
  test_expr <- create_expr(!!test_indicator_defs$ind_formula)
  test_indicator_defs$ind_vars <- list(parse_indicator_vars(
                                       stock_db, test_expr))

  ds_vars <- get_indicator_vars(stock_db, test_indicator_defs)
  expect_is(ds_vars, "data.frame")
  expect_fields <- c("date", "period","stkcd", "indcd",
                       "ind_name", "ind_value")
  actual_fields <- names(ds_vars)
  expect_true(all(actual_fields %in% expect_fields))

})

test_that("ind_attr_def_indcd", {

  # ind_attr_def_indcd with default arguments ====
  new_attr_indcd <- ind_attr_def_indcd(stock_db)
  expect_true(is.function(new_attr_indcd))

})


# Tests for function of indicator define - non-generic functions ----
#
context("Tests for function of indicator definie - non-generic functions")

test_that("create_indicator_def_fun", {

  # create_indicator_def_fun with default arguments ====
  indicator_expr <- create_expr(!!test_indicator_defs$ind_formula)
  indicator_code <- test_indicator_defs$ind_code

  ind_def_fun <- create_indicator_def_fun(indicator_code,
                      indicator_expr = indicator_expr,
                      rolly_window = 0,
                      period = "month")
  expect_true(is.function(ind_def_fun))

  for (i in seq_along(list_ds_vars)) {

    period <- test_indicator_defs$period
    ds_vars <- list_ds_vars[[i]]
    key_fields <- test_indicator_defs$ind_keys[[1]]

    # create indicator definition function
    ind_def_fun <- create_indicator_def_fun(indicator_code,
                                    indicator_expr = indicator_expr,
                                    rolly_window = 0,
                                    period = period)

    # use definition function to compute indicator -- No debug
    ds_indicator <- ind_def_fun(ds_vars, date_index_field = c("date"),
                      key_fields = key_fields)

    expect_is(ds_indicator, "data.frame")
    expect_fields <- c("date", "stkcd", "indcd", "period", test_indicator_defs$ind_code)
    actual_fields <- names(ds_indicator)
    expect_true(all(actual_fields %in% expect_fields))
    expect_equal(unique(ds_indicator$period), period)

    # use definition function to compute indicator  -- Debug
    ds_indicator <- ind_def_fun(ds_vars, date_index_field = c("date"),
                            key_fields = key_fields, debug = TRUE)

    expect_is(ds_indicator, "data.frame")
    expect_fields <- c("date", "stkcd", "indcd", "period",
                       unique(ds_vars$ind_name),
                       test_indicator_defs$ind_code)
    actual_fields <- names(ds_indicator)
    expect_true(all(actual_fields %in% expect_fields))
    expect_equal(unique(ds_indicator$period), period)

  }

})

test_that("create_attribute_def_fun",{

  # create_attribute_def_fun with default arguments ====
  attr_name <- "indcd"
  attr_fun <- function(x, ...) {"indcd_value"}
  ind_attr_df_fun <- create_attribute_def_fun(attr_name,
                                         attr_fun = attr_fun)

  expect_true(is.function(ind_attr_df_fun))

})



