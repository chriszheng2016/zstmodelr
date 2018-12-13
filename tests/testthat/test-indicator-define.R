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
  ind_expr = list(NULL),
  ind_def_fun = list(NULL),
  ind_vars = list(NULL)
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
list_dates <- list(
  day = day_dates,
  month = month_dates,
  quarter = quarter_dates,
  year = year_dates
)


# function to build ds_vars for test
test_ds_vars <- function(dates, peroid) {
  ds_vars <- tibble::tibble(
    date = rep(dates, 2),
    period = peroid,
    stkcd = "stkcd_01",
    indcd = "incd_01",
    ind_code = c(
      rep("f090101c", length(dates)),
      rep("mclsprc", length(dates))
    ),
    ind_value = c(
      rep(1.1, length(dates)),
      rep(2.1, length(dates))
    )
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
    stock_db, test_expr
  ))

  ds_vars <- get_indicator_vars(stock_db, test_indicator_defs)
  expect_is(ds_vars, "data.frame")
  expect_fields <- c(
    "date", "period", "stkcd", "indcd",
    "ind_code", "ind_value"
  )
  actual_fields <- names(ds_vars)
  expect_true(all(actual_fields %in% expect_fields))
})

test_that("ind_attr_def_indcd", {

  # ind_attr_def_indcd with default arguments ====
  new_attr_indcd <- ind_attr_def_indcd(stock_db)
  expect_true(is.function(new_attr_indcd))
})


# Tests for function of indicator define - non-generic functions ----

context("Tests for function of indicator define - non-generic functions")

test_that("create_indicator_def_fun", {

  # create_indicator_def_fun with default arguments ====
  indicator_expr <- create_expr(!!test_indicator_defs$ind_formula)
  indicator_code <- test_indicator_defs$ind_code

  ind_def_fun <- create_indicator_def_fun(indicator_code,
    indicator_expr = indicator_expr,
    rolly_window = 0,
    period = "month"
  )
  expect_true(is.function(ind_def_fun))

  for (i in seq_along(list_ds_vars)) {
    period <- test_indicator_defs$period
    ds_vars <- list_ds_vars[[i]]
    key_fields <- test_indicator_defs$ind_keys[[1]]

    # create indicator definition function
    ind_def_fun <- create_indicator_def_fun(indicator_code,
      indicator_expr = indicator_expr,
      rolly_window = 0,
      period = period
    )

    # use definition function to compute indicator -- No debug
    ds_indicator <- ind_def_fun(ds_vars,
      date_index_field = c("date"),
      key_fields = key_fields
    )

    expect_is(ds_indicator, "data.frame")
    expect_fields <- c("date", "stkcd", "indcd", "period", test_indicator_defs$ind_code)
    actual_fields <- names(ds_indicator)
    expect_true(all(actual_fields %in% expect_fields))
    expect_equal(unique(ds_indicator$period), period)

    # use definition function to compute indicator  -- Debug
    ds_indicator <- ind_def_fun(ds_vars,
      date_index_field = c("date"),
      key_fields = key_fields, debug = TRUE
    )

    expect_is(ds_indicator, "data.frame")
    expect_fields <- c(
      "date", "stkcd", "indcd", "period",
      unique(ds_vars$ind_code),
      test_indicator_defs$ind_code
    )
    actual_fields <- names(ds_indicator)
    expect_true(all(actual_fields %in% expect_fields))
    expect_equal(unique(ds_indicator$period), period)
  }
})

test_that("create_attribute_def_fun", {

  # create_attribute_def_fun with default arguments ====
  attr_name <- "indcd"
  attr_fun <- function(x, ...) {
    "indcd_value"
  }
  ind_attr_df_fun <- create_attribute_def_fun(attr_name,
    attr_fun = attr_fun
  )

  expect_true(is.function(ind_attr_df_fun))
})

test_that("prioritize_indicator_defs", {
  depend_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2-3.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2-3.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2-3.3",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.1",
      ind_vars = list(c("ind_1-2-3.1", "ind_1-2-3.2"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.2",
      ind_vars = list(c("ind_1-2-3.2", "ind_1-2-3.3"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_1",
      ind_vars = list(c("ind_1-2.1", "ind_1-2.2"))
    ) %>%
    # -- defs tree 2 --
    tibble::add_row(
      ind_code = "ind_2-2-3.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_2-2-3.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_2-2-3.3",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_2-2.1",
      ind_vars = list(c("ind_2-2-3.1", "ind_2-2-3.2"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_2-2.2",
      ind_vars = list(c("ind_2-2-3.2", "ind_2-2-3.3"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_2",
      ind_vars = list(c("ind_2-2.1", "ind_2-2.2"))
    )


  # prioritize_indicator_defs with default arguments ====
  prioritized_indicator_defs <- prioritize_indicator_defs(depend_indicators_defs)

  expect_fields <- c("priority", "ds_indicator_defs")
  actual_fields <- names(prioritized_indicator_defs)
  expect_true(all(actual_fields %in% expect_fields))

  expect_proity <- tibble::tibble(
    priority = c(1, 2, 3),
    ind_code = c(
      list(c(
        "ind_1-2-3.1", "ind_1-2-3.2", "ind_1-2-3.3",
        "ind_2-2-3.1", "ind_2-2-3.2", "ind_2-2-3.3"
      )),
      list(c(
        "ind_1-2.1", "ind_1-2.2",
        "ind_2-2.1", "ind_2-2.2"
      )),
      list(c("ind_1", "ind_2"))
    )
  )

  for (i in seq_len(NROW(prioritized_indicator_defs))) {
    actual_ind_codes <- prioritized_indicator_defs$ds_indicator_defs[[i]]$ind_code
    expect_ind_codes <- expect_proity$ind_code[[i]]
    expect_true(all(actual_ind_codes %in% expect_ind_codes))
  }
})

# Tests for function of indicator define - Internal functions ----

context("Tests for function of indicator define - Internal functions")

test_that("create_defs_trees", {
  new_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2-3.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2-3.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2-3.3",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.1",
      ind_vars = list(c("ind_1-2-3.1", "ind_1-2-3.2"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.2",
      ind_vars = list(c("ind_1-2-3.2", "ind_1-2-3.3"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_1",
      ind_vars = list(c("ind_1-2.1", "ind_1-2.2"))
    )

  # create_defs_trees with default arguments ====

  ind_defs_trees <- create_ind_defs_trees(new_indicators_defs)
  expect_is(ind_defs_trees, "ind_defs_trees")
  expect_equal(names(ind_defs_trees), c("ind_code", "depend_ind_codes"))
})

test_that("validate_indicators", {
  valid_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1",
      ind_vars = list(c("ind_1-2.1", "ind_1-2.2"))
    )

  invalid_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2.1",
      ind_vars = list(c("ind_1-2-3.1", "ind_1-2-3.2"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.2",
      ind_vars = list(c("ind_1-2-3.2", "ind_1-2-3.3"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_1",
      ind_vars = list(c("ind_1-2.1", "ind_1-2.2"))
    )


  # validate_indicators on valid indicators_defs ====
  ind_defs_trees <- create_ind_defs_trees(valid_indicators_defs)
  valid_ind_defs_trees <- validate_indicators(ind_defs_trees)

  expect_is(valid_ind_defs_trees, "ind_defs_trees")
  expect_equal(names(valid_ind_defs_trees), c("ind_code", "depend_ind_codes"))
  expect_equal(
    valid_ind_defs_trees$ind_code,
    ind_defs_trees$ind_code
  )
  expect_equal(
    valid_ind_defs_trees$depend_ind_codes,
    ind_defs_trees$depend_ind_codes
  )

  # validate_indicators on invalid indicators_defs ====
  ind_defs_trees <- create_ind_defs_trees(invalid_indicators_defs)
  new_valid_ind_defs_trees <- validate_indicators(ind_defs_trees)

  expect_is(new_valid_ind_defs_trees, "ind_defs_trees")
  expect_equal(names(new_valid_ind_defs_trees), c("ind_code", "depend_ind_codes"))
  expect_equal(
    new_valid_ind_defs_trees$ind_code,
    valid_ind_defs_trees$ind_code
  )
  expect_equal(
    new_valid_ind_defs_trees$depend_ind_codes,
    valid_ind_defs_trees$depend_ind_codes
  )
})

test_that("check_duplicated_indicators", {
  duplicated_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2-3.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2-3.1",
      ind_vars = list(NULL)
    )

  unique_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2-3.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2-3.2",
      ind_vars = list(NULL)
    )


  # check_duplicated_indicators on duplicated indicators_defs ====
  ind_defs_trees <- create_ind_defs_trees(duplicated_indicators_defs)
  expect_error(check_duplicated_indicators(ind_defs_trees),
    regexp = "Found duplicated indicators"
  )

  # check_duplicated_indicators on unique indicators_defs ====
  ind_defs_trees <- create_ind_defs_trees(unique_indicators_defs)
  ind_defs_trees_pass <- check_duplicated_indicators(ind_defs_trees)

  expect_is(ind_defs_trees_pass, "ind_defs_trees")
  expect_equal(names(ind_defs_trees_pass), c("ind_code", "depend_ind_codes"))
  expect_equal(
    ind_defs_trees_pass$ind_code,
    ind_defs_trees$ind_code
  )
  expect_equal(
    ind_defs_trees_pass$depend_ind_codes,
    ind_defs_trees$depend_ind_codes
  )
})

test_that("check_loop_depdency", {
  loop_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2.1",
      ind_vars = list("ind_2")
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1",
      ind_vars = list(c("ind_1-2.1", "ind_1-2.2"))
    ) %>%
    # -- defs tree 2 --
    tibble::add_row(
      ind_code = "ind_2-2.1",
      ind_vars = list(c("ind_1"))
    ) %>%
    tibble::add_row(
      ind_code = "ind_2-2.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_2",
      ind_vars = list(c("ind_2-2.1", "ind_2-2.2"))
    )

  non_loop_indicators_defs <- test_indicator_defs[FALSE, ] %>%
    # -- defs tree 1 --
    tibble::add_row(
      ind_code = "ind_1-2.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1-2.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_1",
      ind_vars = list(c("ind_1-2.1", "ind_1-2.2"))
    ) %>%
    # -- defs tree 2 --
    tibble::add_row(
      ind_code = "ind_2-2.1",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_2-2.2",
      ind_vars = list(NULL)
    ) %>%
    tibble::add_row(
      ind_code = "ind_2",
      ind_vars = list(c("ind_2-2.1", "ind_2-2.2"))
    )


  # check_loop_depdency on loop indicators_defs ====
  ind_defs_trees <- create_ind_defs_trees(loop_indicators_defs)
  expect_error(check_loop_depdency(ind_defs_trees),
    regexp = "Found loop dependency among indicators"
  )

  # check_loop_depdency on loop indicators_defs ====
  ind_defs_trees <- create_ind_defs_trees(non_loop_indicators_defs)
  ind_defs_trees_pass <- check_loop_depdency(ind_defs_trees)

  expect_is(ind_defs_trees_pass, "ind_defs_trees")
  expect_equal(names(ind_defs_trees_pass), c("ind_code", "depend_ind_codes"))
  expect_equal(
    ind_defs_trees_pass$ind_code,
    c(
      "ind_1", "ind_2",
      "ind_1-2.1", "ind_1-2.2",
      "ind_2-2.1", "ind_2-2.2"
    )
  )
})



# clear up testing conext
suppressMessages(close_stock_db(stock_db))
