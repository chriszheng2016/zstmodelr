library(withr)

context("Tests related to factor_test classes")

test_that("factor_test_uniregress, with regress_method = 'pooling'", {

  #Load test data
  ds_test_uniregression_pooling <- readRDS("ds_test_uniregression_pooling.rds")

  # Model of conducting univariate regression test
  model_univariate_regression_pooling <- function(df) {
    lm(return ~ factor_exposure, data = df)
  }

  # Condcut factor uniregress test with summary ouput ====
  result_test_uniregression_pooling <- factor_test_uniregress(ds_test_uniregression_pooling,
                                                              regress_method = "pooling",
                                                              regress_fun = model_univariate_regression_pooling,
                                                              factor_field = "factor_name",
                                                              output_type = "summary",
                                                              date_field = "date")

  # Validate results
  expect_is(result_test_uniregression_pooling, "factor_test_uniregress")
  expect_not_null(result_test_uniregression_pooling@factor_returns)
  expect_not_null(result_test_uniregression_pooling@summary)
  expect_null(result_test_uniregression_pooling@raw_result)

  # fields of summary info
  expected <- c("factor_name", "r.squared", "f_pvalue", "beta_t_abs_mean",
                "beta_t_sig_ratio", "beta_t_mean", "beta_t_sd", "beta_t_mean_std_ratio",
                "factor_return", "factor_return_tvalue", "factor_return_pvalue")
  actual   <- names(result_test_uniregression_pooling@summary)
  expect_equal(actual, expected)

  # Condcut factor uniregress test with raw ouput ====
  result_test_uniregression_pooling <- factor_test_uniregress(ds_test_uniregression_pooling,
                                                              regress_method = "pooling",
                                                              regress_fun = model_univariate_regression_pooling,
                                                              factor_field = "factor_name",
                                                              output_type = "raw",
                                                              date_field = "date")
  expect_is(result_test_uniregression_pooling, "factor_test_uniregress")
  expect_not_null(result_test_uniregression_pooling@factor_returns)
  expect_not_null(result_test_uniregression_pooling@summary)
  expect_not_null(result_test_uniregression_pooling@raw_result)

})


test_that("factor_test_uniregress, with regress_method = 'cross_section'", {

  #Load test data
  ds_test_uniregression_barra <- readRDS("ds_test_uniregression_barra.rds")

  # Model of conducting univariate regression test
  model_univariate_regression_barra <- function(df, ...) {
    lm(return ~ factor_exposure, data = df, ...)
  }

  # Condcut factor uniregress test with summary ouput ====
  result_test_uniregression_barra <- factor_test_uniregress( ds_test_uniregression_barra,
                                                             regress_method = "cross_section",
                                                             regress_fun = model_univariate_regression_barra,
                                                             output_type = "summary",
                                                             factor_field = "factor_name",
                                                             date_field = "date")

  # Validate results
  expect_is(result_test_uniregression_barra, "factor_test_uniregress")
  expect_not_null(result_test_uniregression_barra@factor_returns)
  expect_not_null(result_test_uniregression_barra@summary)
  expect_null(result_test_uniregression_barra@raw_result)

  # fields of summary info
  expected <- c("factor_name", "r.squared", "f_pvalue", "beta_t_abs_mean",
                "beta_t_sig_ratio", "beta_t_mean", "beta_t_sd",
                "beta_t_mean_std_ratio", "factor_return",
                "factor_return_tvalue", "factor_return_pvalue" )
  actual   <- names(result_test_uniregression_barra@summary)
  expect_equal(actual, expected)

  # Condcut factor uniregress test with raw ouput ====
  result_test_uniregression_barra <- factor_test_uniregress( ds_test_uniregression_barra,
                                                             regress_method = "cross_section",
                                                             regress_fun = model_univariate_regression_barra,
                                                             output_type = "raw",
                                                             factor_field = "factor_name",
                                                             date_field = "date")

  # Validate results
  expect_is(result_test_uniregression_barra, "factor_test_uniregress")
  expect_not_null(result_test_uniregression_pooling@factor_returns)
  expect_not_null(result_test_uniregression_pooling@summary)
  expect_not_null(result_test_uniregression_pooling@raw_result)

})

test_that("factor_test_IC, with general arguments", {

  #Load test data
  ds_test_IC <- readRDS("ds_test_IC.rds")

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
  expect_null(result_test_IC@raw_result)

  # fields of summary info
  expected <- c("factor_name", "IC_mean", "IC_std", "IR",
                "IC_positive_ratio", "IC_avg_pvalue")
  actual   <- names(result_test_IC@summary)
  expect_equal(actual, expected)

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
  expect_not_null(result_test_IC@raw_result)

})

test_that("factor_test_sort_portfolios, with general arguments", {

  #Load test data
  ds_test_sort_portfolios <- readRDS("ds_test_sort_portfolios.rds")

  # Method to build sort portfolios
  model_sort_portfolios <- function(df, ...) {
    # build portfolio group by portfolio groups
    sort_portfolios <- build_sort_portfolios(
        stocks_list = df$stkcd,
        factor_value_list = df$factor_exposure,
        ngroup = 5,
        first_group_index = 1,
        factor_group_order = "asc"
      )
    return(sort_portfolios)
  }

  # Conduct portfolio sorts test with summary ouput ====
  with_options(
  c(warn = -1),
    {
      result_sort_portfolios <- factor_test_sort_portfolios(ds_test_sort_portfolios,
                                                            sort_portfolios_fun = model_sort_portfolios,
                                                            output_type = "summary",
                                                            factor_field = "factor_name",
                                                            date_field = "date",
                                                            stkcd_field = "stkcd",
                                                            return_field = "return")

    }
  )

  # Validate results
  expect_is(result_sort_portfolios, "factor_test_sort_portfolios")
  expect_not_null(result_sort_portfolios@portfolios_return)
  expect_not_null(result_sort_portfolios@summary)
  expect_null(result_sort_portfolios@raw_result)

  # fields of summary info
  expected <- c("factor_name", "indicator", "group_1", "group_2", "group_3",
                "group_4", "group_5", "group_zero" )
  actual   <- names(result_sort_portfolios@summary)
  expect_equal(actual, expected)

  # Conduct portfolio sorts test with raw ouput ====
  with_options(
    c(warn = -1),
    {
      result_sort_portfolios <- factor_test_sort_portfolios(ds_test_sort_portfolios,
                                                            sort_portfolios_fun = model_sort_portfolios,
                                                            output_type = "raw",
                                                            factor_field = "factor_name",
                                                            date_field = "date",
                                                            stkcd_field = "stkcd",
                                                            return_field = "return")

    }
  )

  # Validate results
  expect_is(result_sort_portfolios, "factor_test_sort_portfolios")
  expect_not_null(result_sort_portfolios@portfolios_return)
  expect_not_null(result_sort_portfolios@summary)
  expect_not_null(result_sort_portfolios@raw_result)


})


