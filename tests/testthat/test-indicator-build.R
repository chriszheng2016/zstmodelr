# Tests for function of indicator build  ----
context("Tests for function of indicator build")

test_that("compute_indicator", {

  # indicator definition params
  indicator_expr <- create_expr("f090101c/mclsprc")
  indicator_code <- "ind01"
  period <- "month"

  # create indicator definition function
  ind_def_fun <- create_indicator_def_fun(indicator_code,
    indicator_expr = indicator_expr,
    rolly_window = 0,
    period = period
  )

  # compute_indicator on good dataset of ds_vars ====

  # load good dataset of ds_vars for computing indicator
  ds_vars <- readRDS("./data/ds_test_ind_good_vars.rds")

  # test whether run parallel or not
  run_parallel <- c(TRUE, FALSE)
  for (parallel in run_parallel) {
    # compute indicators by using indicator definition
    ds_indicator <- compute_indicator(ds_vars,
      compute_fun = ind_def_fun,
      date_index_field = "date",
      key_fields = "stkcd",
      parallel = parallel
    )

    expect_is(ds_indicator, "data.frame")
    expect_fields <- c("date", "stkcd", "indcd", "period", indicator_code)
    actual_fields <- names(ds_indicator)
    expect_true(all(actual_fields %in% expect_fields))
    expect_equal(unique(ds_indicator$period), period)
    expect_true(length(unique(ds_indicator$stkcd))
    == length(unique(ds_vars$stkcd)))
  }



  # compute_indicator on bad dataset of ds_vars ====

  # load bad dataset of ds_vars for computing indicator
  ds_vars <- readRDS("./data/ds_test_ind_bad_vars.rds")
  # compute indicators by using indicator definition
  expect_message(
    ds_indicator <- compute_indicator(ds_vars,
      compute_fun = ind_def_fun,
      date_index_field = "date",
      key_fields = "stkcd",
      parallel = FALSE
    )
  )

  # return a tibble with 0 length
  expect_true(NROW(ds_indicator) == 0)

})

test_that("create_indicator", {

  # create_indicator on ds_vars with value of keys aren't NA ====

  # indicator definition params
  indicator_expr <- create_expr("price <- dplyr::lag(mclsprc, n=2)
                                earning <- f090101c
                                ep <- earning/price")
  indicator_code <- "m_ep"
  period <- "month"

  # create indicator definition function
  ind_def_fun <- create_indicator_def_fun(indicator_code,
    indicator_expr = indicator_expr,
    rolly_window = 0,
    period = period
  )

  # load dataset of ds_vars for creating indicator
  ds_vars <- readRDS("./data/ds_test_ind_good_vars.rds")

  # create indicators by using indicator definition
  ds_indicator <- create_indicator(ds_vars,
    ind_def_fun = ind_def_fun,
    date_index_field = "date",
    key_fields = "stkcd",
    parallel = TRUE
  )

  expect_is(ds_indicator, "data.frame")
  expect_fields <- c("date", "stkcd", "indcd", "period", indicator_code)
  actual_fields <- names(ds_indicator)
  expect_true(all(actual_fields %in% expect_fields))
  expect_equal(unique(ds_indicator$period), period)
  expect_true(length(unique(ds_indicator$stkcd))
  == length(unique(ds_vars$stkcd)))



  # create_indicator on ds_vars with some value of keys are NA ====
  indicator_expr <- create_expr("stock_return <- mretwd
                                market_return <- cmretwdtl
                                model <- lm(stock_return ~ market_return)
                                beta <- coef(model)['market_return']")
  indicator_code <- "m_stock_beta1"
  period <- "month"

  # create indicator definition function
  ind_def_fun <- create_indicator_def_fun(indicator_code,
    indicator_expr = indicator_expr,
    rolly_window = 12,
    period = period
  )

  # load dataset of ds_vars for creating indicator
  ds_vars <- readRDS("./data/ds_test_ind_keys_are_na.rds")
  # compute indicators by using indicator definition
  ds_indicator <- create_indicator(ds_vars,
    ind_def_fun = ind_def_fun,
    date_index_field = "date",
    key_fields = "stkcd",
    parallel = TRUE
  )

  expect_is(ds_indicator, "data.frame")
  expect_fields <- c("date", "stkcd", "period", indicator_code)
  actual_fields <- names(ds_indicator)
  expect_true(all(actual_fields %in% expect_fields))
  expect_equal(unique(ds_indicator$period), period)
  expect_true(length(unique(ds_indicator$stkcd))
  == length(unique(ds_vars$stkcd)) - 1)

  # create_indicator with with debug ====

  # indicator definition params
  indicator_expr <- create_expr("price <- dplyr::lag(mclsprc, n=2)
                                earning <- f090101c
                                ep <- earning/price")
  indicator_code <- "m_ep"
  period <- "month"

  # create indicator definition function
  ind_def_fun <- create_indicator_def_fun(indicator_code,
                                  indicator_expr = indicator_expr,
                                  rolly_window = 0,
                                  period = period
  )

  # load dataset of ds_vars for creating indicator
  ds_vars <- readRDS("./data/ds_test_ind_good_vars.rds")

  # create indicators by using indicator definition
  ds_indicator <- create_indicator(ds_vars,
                                   ind_def_fun = ind_def_fun,
                                   debug = TRUE,
                                   date_index_field = "date",
                                   key_fields = "stkcd",
                                   parallel = FALSE
  )

  expect_is(ds_indicator, "data.frame")
  expect_fields <- c("date", "stkcd", "indcd", "period",
                     unique(ds_vars$ind_name),
                     indicator_code)
  actual_fields <- names(ds_indicator)
  expect_true(all(actual_fields %in% expect_fields))
  expect_equal(unique(ds_indicator$period), period)
  expect_true(length(unique(ds_indicator$stkcd))
              == length(unique(ds_vars$stkcd)))



})

test_that("modify_indicator", {

  # load dataset of indicator dataset to modify
  ds_origin_indicator <- readRDS("./data/ds_test_ind_m_ep_ttm.rds")

  # a factory to build attr_fun
  make_attr_fun <- function(attr_value) {
    attr_fun <- function(date, stkcd, ...) {
      attr_value
    }
  }

  # modify_indicator with default arguments ====
  attr_name <- "indcd"
  attr_value1 <- "indcd_code1"
  attr_fun <- make_attr_fun(attr_value1)
  ind_attr_def_fun <- create_attribute_def_fun(attr_name,
    attr_fun = attr_fun
  )

  # modify indicators by using attribute definition
  ds_modify_indicator <- modify_indicator(ds_origin_indicator,
    modify_fun = ind_attr_def_fun,
    date_index_field = "date",
    key_fields = "stkcd",
    parallel = FALSE
  )

  expect_is(ds_modify_indicator, "data.frame")
  expect_fields <- c(names(ds_origin_indicator), attr_name)
  actual_fields <- names(ds_modify_indicator)
  expect_true(all(actual_fields %in% expect_fields))
  expect_true(all(ds_modify_indicator[attr_name] == attr_value1))

  # modify indicators with existed attrubute ====
  attr_name <- "indcd"
  attr_value2 <- "indcd_code2"
  attr_fun <- make_attr_fun(attr_value2)
  ind_attr_def_fun <- create_attribute_def_fun(attr_name,
                                      attr_fun = attr_fun
  )

  # don't change existed attribute
  expect_warning(
    ds_modify_indicator <- modify_indicator(ds_modify_indicator,
      modify_fun = ind_attr_def_fun,
      date_index_field = "date",
      key_fields = "stkcd",
      parallel = FALSE
    )
  )

  expect_is(ds_modify_indicator, "data.frame")
  expect_fields <- c(names(ds_origin_indicator), attr_name)
  actual_fields <- names(ds_modify_indicator)
  expect_true(all(actual_fields %in% expect_fields))
  expect_true(all(ds_modify_indicator[attr_name] == attr_value1))

  # replace exsited attribute
  expect_warning(
    ds_modify_indicator <- modify_indicator(ds_modify_indicator,
                                            modify_fun = ind_attr_def_fun,
                                            replace_exist = TRUE,
                                            date_index_field = "date",
                                            key_fields = "stkcd",
                                            parallel = FALSE
    )
  )

  expect_is(ds_modify_indicator, "data.frame")
  expect_fields <- c(names(ds_origin_indicator), attr_name)
  actual_fields <- names(ds_modify_indicator)
  expect_true(all(actual_fields %in% expect_fields))
  expect_true(all(ds_modify_indicator[attr_name] == attr_value2))

})
