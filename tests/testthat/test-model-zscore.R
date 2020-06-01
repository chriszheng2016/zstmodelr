context("Tests for z-score model functions")

test_that("factors_zscore, with various arguments", {

  # Load test data
  ds_test_factors <- readRDS("./data/ds_test_factors.rds")

  # compute zscore with default arguments ====
  result_factors_zscore <- factors_zscore(ds_test_factors)
  # mean of z-score is 0
  actual_means <- result_factors_zscore %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of z-score is 1
  actual_sds <- result_factors_zscore %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(sd, na.rm = TRUE)
  expect_equal(mean(colMeans(actual_sds)), 1)

  # compute zscore with group and no aggregate_formula ====
  result_factors_zscore <- factors_zscore(ds_test_factors,
    aggregate_formula = NULL,
    group_by = c("date")
  )
  # mean of z-score is 0
  actual_means <- result_factors_zscore %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of z-score is 1
  actual_sds <- result_factors_zscore %>%
    dplyr::select(-(1:4)) %>%
    dplyr::summarise_all(sd, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_sds)), 1)

  # compute zscore with group and aggregate_formula ====
  result_factors_zscore_1 <- factors_zscore(ds_test_factors,
    aggregate_formula = 0.5 * GPM + 0.5 * ROCE + 0.5 * PE + 0.5 * PB + 0.5 * CUR + 0.5 * QR,
    group_by = c("date")
  )



  # compute zscore with group and aggregate_formula ====
  aggregate_formula <- rlang::quo(0.5 * GPM + 0.5 * ROCE + 0.5 * PE + 0.5 * PB + 0.5 * CUR + 0.5 * QR)
  result_factors_zscore_2 <- factors_zscore(ds_test_factors,
    aggregate_formula = !!aggregate_formula,
    group_by = c("date")
  )

  expect_identical(result_factors_zscore_1, result_factors_zscore_2)

  result_factors_zscore <- result_factors_zscore_2

  # mean of z-score is 0
  actual_means <- result_factors_zscore %>%
    dplyr::summarise_at(all.vars(aggregate_formula),
      mean,
      na.rm = TRUE
    ) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of z-score is 1
  actual_sds <- result_factors_zscore %>%
    dplyr::summarise_at(all.vars(aggregate_formula),
      sd,
      na.rm = TRUE
    ) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_sds)), 1)

  actual_stk_score <- result_factors_zscore %>%
    na.omit() %>%
    dplyr::summarise_at(c(all.vars(aggregate_formula), "stk_score"),
      sum,
      na.rm = TRUE
    ) %>%
    dplyr::select(-date) %>%
    dplyr::mutate(expected_stk_score = !!aggregate_formula)
  expect_equal(mean(with(actual_stk_score, stk_score - expected_stk_score)), 0)
})

test_that("zscore_filter_stocks, with various arguments", {

  # Load test data
  ds_test_factors <- readRDS("./data/ds_test_factors.rds")

  # filter stocks with default arguments ====

  result_factors_zscore <- factors_zscore(ds_test_factors,
    aggregate_formula = GPM + ROCE + PE + PB + CUR + QR,
    group_by = c("date")
  )

  # Select top stocks basing on z-scores ====
  top_score_stocks_list <- zscore_filter_stocks(result_factors_zscore)

  # top scores:  number=5, rank from 1 to 5
  actual_top_ranks <- top_score_stocks_list %>%
    dplyr::mutate(rank = rank(stk_score)) %>%
    dplyr::summarise(max_rank = max(rank), min_rank = min(rank), n = dplyr::n())
  expect_equal(mean(actual_top_ranks$n), 5)
  expect_equal(mean(actual_top_ranks$max_rank), 5)
  expect_equal(mean(actual_top_ranks$min_rank), 1)
})
