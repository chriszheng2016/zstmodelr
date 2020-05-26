# Tests for factor_test classes - factor_test_uniregress ----
context("Tests for factor_test classes - factor_test_uniregress")

test_that("factor_test_uniregress, with general arguments", {

  #Load test data
  ds_test_uniregression <- readRDS("./data/ds_test_uniregression.rds")

  # Model of conducting univariate regression test
  model_univariate_regression <- function(df, ...) {
    lm(return ~ factor_exposure, data = df, ...)
  }

  # Condcut factor uniregress test with summary ouput ====
  result_test_uniregression <- factor_test_uniregress( ds_test_uniregression,
                                    regress_fun = model_univariate_regression,
                                    output_type = "summary",
                                    factor_field = "factor_name",
                                    date_field = "date")


  # Validate results
  expect_is(result_test_uniregression, "factor_test_uniregress")
  expect_not_null(result_test_uniregression@summary)
  expect_not_null(result_test_uniregression@factor_returns)
  expect_null(result_test_uniregression@raw_result)

  # fields of summary info
  expected <- c("factor_name","obs", "nas", "avg", "med", "min", "max","std",
                "skew", "kurt", "pos_pct", "neg_pct", "odds", "t.test_t",
                "t.test_p", "normal.test_p")
  actual   <- names(result_test_uniregression@summary)
  expect_true(setequal(actual, expected))

  # fields of factor return
  expected <- c("date", unique(ds_test_uniregression$factor_name))
  actual  <- names(result_test_uniregression@factor_returns)
  expect_true(setequal(actual, expected))

  # Condcut factor uniregress test with raw ouput ====
  result_test_uniregression <- factor_test_uniregress( ds_test_uniregression,
                                    regress_fun = model_univariate_regression,
                                    output_type = "raw",
                                    factor_field = "factor_name",
                                    date_field = "date")

  # Validate results
  expect_is(result_test_uniregression, "factor_test_uniregress")
  expect_not_null(result_test_uniregression@factor_returns)
  expect_not_null(result_test_uniregression@summary)
  expect_not_null(result_test_uniregression@raw_result)

})

# Tests for factor_test classes - factor_test_IC" ----
context("Tests for factor_test classes - factor_test_IC")

test_that("factor_test_IC, with general arguments", {

  #Load test data
  ds_test_IC <- readRDS("./data/ds_test_IC.rds")

  # Method of conducting IC test
  Model_compute_IC <- function(df, ...) {
    suppressWarnings(cor.test(x = df$return, y = df$factor_exposure, ... ))
  }

  # Conduct factor IC test with summary ouput ====
  result_test_IC <- factor_test_IC( ds_test_IC,
                                    IC_fun = Model_compute_IC,
                                    method = "pearson",
                                    output_type = "summary",
                                    factor_field = "factor_name",
                                    date_field = "date")

  # Validate results
  expect_is(result_test_IC, "factor_test_IC")
  expect_not_null(result_test_IC@summary)
  expect_not_null(result_test_IC@factor_ICs)
  expect_null(result_test_IC@raw_result)

  # fields of summary info
  expected <- c("factor_name","obs", "nas", "avg", "med", "min", "max","std",
                "skew", "kurt", "pos_pct", "neg_pct", "odds", "t.test_t",
                "t.test_p", "normal.test_p")
  actual   <- names(result_test_IC@summary)
  expect_true(setequal(actual, expected))

  # field of factor_ICs
  expected <- c("date", unique(ds_test_IC$factor_name))
  actual  <- names(result_test_IC@factor_ICs)
  expect_true(setequal(actual, expected))


  # Conduct factor IC test with raw ouput ====
  result_test_IC <- factor_test_IC( ds_test_IC,
                                    IC_fun = Model_compute_IC,
                                    method = "pearson",
                                    output_type = "raw",
                                    factor_field = "factor_name",
                                    date_field = "date")

  # Validate results
  expect_is(result_test_IC, "factor_test_IC")
  expect_not_null(result_test_IC@summary)
  expect_not_null(result_test_IC@factor_ICs)
  expect_not_null(result_test_IC@raw_result)

})

# Tests for factor_test classes - factor_test_sort_portfolios ----
context("Tests for factor_test classes - factor_test_sort_portfolios")

test_that("factor_test_sort_portfolios, with general arguments", {

  #Load test data
  ds_test_sort_portfolios <- readRDS("./data/ds_test_sort_portfolios.rds")

  # Method to build sort portfolios
  model_sort_portfolios <- function(df, ngroup = 5, ...) {
    # build portfolio group by portfolio groups
    sort_portfolios <- build_sort_portfolios(
        stocks_list = df$stkcd,
        factor_value_list = df$factor_exposure,
        stocks_weight_list = NULL,
        ngroup = ngroup
      )

    return(sort_portfolios)
  }

  # Conduct portfolio sorts test with summary ouput ====
  # withr::with_options(
  # c(warn = -1),
  #   {
  #     result_sort_portfolios <- factor_test_sort_portfolios(ds_test_sort_portfolios,
  #                                        sort_portfolios_fun = model_sort_portfolios,
  #                                        ngroup = 5,
  #                                        output_type = "summary",
  #                                        factor_field = "factor_name",
  #                                        date_field = "date",
  #                                        stkcd_field = "stkcd",
  #                                        return_field = "return")
  #
  #   }
  # )


  result_sort_portfolios <- factor_test_sort_portfolios(ds_test_sort_portfolios,
                                  sort_portfolios_fun = model_sort_portfolios,
                                  ngroup = 5,
                                  output_type = "summary",
                                  factor_field = "factor_name",
                                  date_field = "date",
                                  stkcd_field = "stkcd",
                                  return_field = "return")


  # Validate results
  expect_is(result_sort_portfolios, "factor_test_sort_portfolios")
  expect_not_null(result_sort_portfolios@summary)
  expect_not_null(result_sort_portfolios@factor_returns)
  expect_not_null(result_sort_portfolios@portfolios_summary)
  expect_not_null(result_sort_portfolios@portfolios_return)
  expect_null(result_sort_portfolios@raw_result)

  # fields of summary info
  expected <- c("factor_name","obs", "nas", "avg", "med", "min", "max","std",
                "skew", "kurt", "pos_pct", "neg_pct", "odds", "t.test_t",
                "t.test_p", "normal.test_p")
  actual   <- names(result_sort_portfolios@summary)
  expect_true(setequal(actual, expected))

  # fields of factor_returns
  expected <- c("date", unique(ds_test_sort_portfolios$factor_name))
  actual   <- names(result_sort_portfolios@factor_returns)
  expect_true(setequal(actual, expected))

  # fields of portfolios_summary
  expected <- c("factor_name", "indicator", "group_lo", "group_2", "group_3",
                "group_4", "group_hi", "group_zero" )
  actual   <- names(result_sort_portfolios@portfolios_summary)
  expect_true(setequal(actual, expected))

  # fields of portfolios_return
  expected <- c("factor_name", "date", "group_lo", "group_2", "group_3",
                "group_4", "group_hi", "group_zero" )
  actual   <- names(result_sort_portfolios@portfolios_return)
  expect_true(setequal(actual, expected))



  # Conduct portfolio sorts test with raw ouput ====
  result_sort_portfolios <- factor_test_sort_portfolios(ds_test_sort_portfolios,
                                    sort_portfolios_fun = model_sort_portfolios,
                                    ngroup = 5,
                                    output_type = "raw",
                                    factor_field = "factor_name",
                                    date_field = "date",
                                    stkcd_field = "stkcd",
                                    return_field = "return")


  # Validate results
  expect_is(result_sort_portfolios, "factor_test_sort_portfolios")
  expect_not_null(result_sort_portfolios@summary)
  expect_not_null(result_sort_portfolios@factor_returns)
  expect_not_null(result_sort_portfolios@portfolios_summary)
  expect_not_null(result_sort_portfolios@portfolios_return)
  expect_not_null(result_sort_portfolios@raw_result)


})


