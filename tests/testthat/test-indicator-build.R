# Tests for function of indicator build  ----
context("Tests for function of indicator build")

test_that("compute_indicator", {

  # indicator definition params
  indicator_expr <- create_expr("f090101c/mclsprc")
  indicator_name <- "ind01"
  period <- "month"

  # create indicator definition function
  ind_def <- create_indicator_def(indicator_expr,
    indicator_name,
    rolly_window = 0,
    period = period
  )

  # compute_indicator on good datsaset of ds_vars ====

  # load good dataset of ds_vars for computing indicator
  ds_vars <- readRDS("./data/ds_test_ind_good_vars.rds")

  # test whether run parallel or not
  run_parallel <- c(TRUE, FALSE)
  for (parallel in run_parallel) {
    # compute indicators by using indicator definition
    ds_indicator <- compute_indicator(ds_vars,
      ind_def_fun = ind_def,
      date_index_field = "date",
      key_fields = "stkcd",
      parallel = parallel
    )

    expect_is(ds_indicator, "data.frame")
    expect_fields <- c("date", "stkcd", "period", indicator_name)
    actual_fields <- names(ds_indicator)
    expect_true(all(actual_fields %in% expect_fields))
    expect_equal(unique(ds_indicator$period), period)
    expect_true(length(unique(ds_indicator$stkcd))
    == length(unique(ds_vars$stkcd)))
  }


  # compute_indicator on bad datsaset of ds_vars ====

  # load bad dataset of ds_vars for computing indicator
  ds_vars <- readRDS("./data/ds_test_ind_bad_vars.rds")
  # compute indicators by using indicator definition
  expect_message(
    ds_indicator <- compute_indicator(ds_vars,
      ind_def_fun = ind_def,
      date_index_field = "date",
      key_fields = "stkcd",
      parallel = FALSE
    )
  )

  #return a tibble with 0 length
  expect_true(NROW(ds_indicator) == 0)

})
