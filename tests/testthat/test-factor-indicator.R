context("Tests related to functions of processing indicators")

test_that("normalize_indicators, with various arguments", {

  #Load test data
  ds_test_indicators <- readRDS("ds_test_indicators.rds")

  # normalize indicators with default arguments ====
  result_normalize_indicators <- normalize_indicators(ds_test_indicators)
  # mean of normalized indicators is 0
  actual_means <- result_normalize_indicators %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all( mean, na.rm = TRUE)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of normalized indicators is 1
  actual_sds <- result_normalize_indicators %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(sd, na.rm = TRUE)
  expect_equal(mean(colMeans(actual_sds)), 1)

  # normalize inidcators by groups ====
  result_normalize_indicators <- normalize_indicators(ds_test_indicators,
                                          indicators = NULL,
                                          group_by = c("date"))
  # mean of normalized indicators is 0
  actual_means <- result_normalize_indicators %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all( mean, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of normalized indicators is 1
  actual_sds <- result_normalize_indicators %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(sd, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_sds)), 1)

})


test_that("normalize, with various arguments", {

  set.seed(1234)
  x <- rnorm(100, mean = 5, sd = 5)

  # clean extremes with default argurments ====
  x_2_extremes <- c(100,100, x)
  x_result <- normalize(x_2_extremes)
  expect_true(any(duplicated(x_result[1:2])))
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

  # clean extremes with sigma method, but not standardize ====
  x_result <- normalize(x, clean_extremes_method = "sigma", n_sigma = 1,
                                  extreme_value = "NA",standard_method = NULL)
  expect_true(any(is.na(x_result)))

  # clean extremes with mad method , but not standardize ====
  x_result <- normalize(x, clean_extremes_method = "mad", n_dmad = 1,
                                  extreme_value = "NA",standard_method = NULL)
  expect_true(any(is.na(x_result)))

  # standardize with normal method but not clean extremes ====
  x_result <- normalize(x, clean_extremes_method = NULL,
                                  standard_method = "normal")
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

  # standardize with rank method but not clean extremes ====
  x_result <- normalize(x, clean_extremes_method = NULL,
                                  standard_method = "rank")
  expect_equal(median(x_result), 50.5 )
  expect_equal(min(x_result), 1)
  expect_equal(max(x_result), 100)

  # clean extremes and standize with default arguments ====
  x_result <- normalize(x)
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

})


test_that("clean_extremes_sigma, with various arguments", {

  set.seed(1234)
  x <- rnorm(100)

  # clean extremes with default argments ====
  x_2_extremes <- c(100,100, x)
  x_result <- clean_extremes_sigma(x_2_extremes)
  # extreme value(100) was identified and replace the limit value
  expect_true(any(duplicated(x_result[1:2])))

  # clean extremes by using small n_sigma and replacing with NA ====
  x_result <- clean_extremes_sigma(x, n_sigma = 1, extreme_value = "NA")
  expect_true(any(is.na(x_result)))

  # clean extremes by using small n_sigma and replacing with limit value ====
  x_result <- clean_extremes_sigma(x, n_sigma = 1, extreme_value = "limit")
  expect_true(any(duplicated(x_result)))

})

test_that("clean_extremes_mad, with various arguments", {

  set.seed(1234)
  x <- rnorm(100)

  # clean extremes with default argments ====
  x_2_extremes <- c(100,100, x)
  x_result <- clean_extremes_mad(x_2_extremes)
  # extreme value(100) was identified and replace the limit value
  expect_true(any(duplicated(x_result[1:2])))

  # clean extremes by using small n_dmad and replacing with NA ====
  x_result <- clean_extremes_mad(x, n_dmad = 1, extreme_value = "NA")
  expect_true(any(is.na(x_result)))

  # clean extremes by using small n_dmad and replacing with limit value ====
  x_result <- clean_extremes_mad(x, n_dmad = 1, extreme_value = "limit")
  expect_true(any(duplicated(x_result)))

})

test_that("standardize_normal_scale, with various arguments", {

  set.seed(1234)
  x <- rnorm(100, mean = 5, sd = 5)

  # standardize with default argments ====
  x_result <- standardize_normal_scale(x)
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

})

test_that("standardize_rank_scale, with various arguments", {

  set.seed(1234)
  x <- rnorm(100, mean = 5, sd = 5)

  # standardize with default arguments ====
  x_result <- standardize_rank_scale(x)
  expect_equal(median(x_result), 50.5 )
  expect_equal(min(x_result), 1)
  expect_equal(max(x_result), 100)

})
