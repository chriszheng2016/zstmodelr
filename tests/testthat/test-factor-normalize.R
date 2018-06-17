
# Tests for normalizing factors functions - normalize_factors ----
context("Tests for normalizing factors functions - normalize_factors")

test_that("normalize_factors, with various arguments", {

  #Load test data
  ds_test_factors <- readRDS("ds_test_factors.rds")

  # normalize factors with default arguments ====
  result_normalize_factors <- normalize_factors(ds_test_factors)
  # mean of normalized factors is 0
  actual_means <- result_normalize_factors %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all( mean, na.rm = TRUE)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of normalized factors is 1
  actual_sds <- result_normalize_factors %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(sd, na.rm = TRUE)
  expect_equal(mean(colMeans(actual_sds)), 1)

  # normalize factors by groups ====
  result_normalize_factors <- normalize_factors(ds_test_factors,
                                          factors = NULL,
                                          group_by = c("date"))
  # mean of normalized factors is 0
  actual_means <- result_normalize_factors %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all( mean, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of normalized factors is 1
  actual_sds <- result_normalize_factors %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(sd, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_sds)), 1)

})

# Tests for normalizing factors functions - normalize" ----
context("Tests for normalizing factors functions - normalize")

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
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

  # clean extremes and standize with default arguments ====
  x_result <- normalize(x)
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

  # clean extremes and standize with non-default arguments ====
  x_result <- normalize(x, clean_extremes_method = "mad", n_dmad = 1,
                        extreme_value = "NA",standard_method = "rank")
  expect_equal(mean(x_result, na.rm = TRUE), 0)
  expect_equal(sd(x_result, na.rm = TRUE), 1)

  x_result <- normalize(x, clean_extremes_method = "sigma", n_sigma = 1,
                        extreme_value = "NA",standard_method = "normal")
  expect_equal(mean(x_result, na.rm = TRUE), 0)
  expect_equal(sd(x_result, na.rm = TRUE), 1)

})

# Tests for normalizing factors functions - clean_extremes_sigma ----
context("Tests for normalizing factors functions - clean_extremes_sigma")

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

  # make sure deel with abnormal data robustly
  x <- 0
  expect_failure(expect_error(x_result <- clean_extremes_sigma(x)))
  x <- c(NA, NA, NA)
  expect_failure(expect_error(x_result <- clean_extremes_sigma(x)))

})

# Tests for normalizing factors functions - clean_extremes_mad ----
context("Tests for normalizing factors functions - clean_extremes_mad")

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

  # make sure deel with abnormal data robustly
  x <- 0
  expect_failure(expect_error(x_result <- clean_extremes_mad(x)))
  x <- c(NA, NA, NA)
  expect_failure(expect_error(x_result <- clean_extremes_mad(x)))

})

# Tests for normalizing factors functions - standardize_normal_scale ----
context("Tests for normalizing factors functions - standardize_normal_scale")

test_that("standardize_normal_scale, with various arguments", {

  set.seed(1234)
  x <- rnorm(100, mean = 5, sd = 5)

  # standardize with default argments ====
  x_result <- standardize_normal_scale(x)
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

})

# Tests for normalizing factors functions - standardize_rank_scale ----
context("Tests for normalizing factors functions - standardize_rank_scale")

test_that("standardize_rank_scale, with various arguments", {

  set.seed(1234)
  x <- runif(100,min = 1, max = 10)
  # x <- rnorm(100, mean = 5, sd = 5)

  # standardize with default arguments ====
  x_result <- standardize_rank_scale(x)
  expect_equal(mean(x_result), 0)
  expect_equal(sd(x_result), 1)

})
