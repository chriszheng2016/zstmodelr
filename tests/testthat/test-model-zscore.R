context("Tests related to z-score compuation")

test_that("factors_zscore, with various arguments", {

  #Load test data
  ds_test_indicators <- readRDS("ds_test_indicators.rds")

  # compute zscore with default argments ====
  result_factors_zscore <- factors_zscore(ds_test_indicators)
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
  result_factors_zscore <- factors_zscore(ds_test_indicators,
                                          aggregate_formula = NULL,
                                          group_by = c("date"))
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
  # aggregate_formula = quote(GPM + ROCE + PE + PB + CUR + QR)
  result_factors_zscore <- factors_zscore(ds_test_indicators,
                                          aggregate_formula = GPM + ROCE + PE + PB + CUR + QR,
                                          group_by = c("date"))

  # mean of z-score is 0
  actual_means <- result_factors_zscore %>%
    dplyr::summarise_at((c("GPM","ROCE","PE","PB", "CUR", "QR")),
                        mean, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_means)), 0)

  # sd of z-score is 1
  actual_sds <- result_factors_zscore %>%
    dplyr::summarise_at((c("GPM","ROCE","PE","PB", "CUR", "QR")),
                        sd, na.rm = TRUE) %>%
    dplyr::select(-date)
  expect_equal(mean(colMeans(actual_sds)), 1)

  # stk_score = GPM + ROCE + PE + PB + CUR + QR
  actual_stk_score <- result_factors_zscore %>%
    na.omit() %>%
    dplyr::summarise_at((c("GPM","ROCE","PE","PB", "CUR", "QR", "stk_score")),
                        sum, na.rm = TRUE)  %>%
    dplyr::select(-date) %>%
    dplyr::mutate(expected_stk_score = (GPM + ROCE + PE + PB + CUR + QR))
  expect_equal(mean(with(actual_stk_score, stk_score - expected_stk_score)), 0)

})


test_that("zscore_filter_stocks, with varous argments", {

  #Load test data
  ds_test_indicators <- readRDS("ds_test_indicators.rds")

  # filter stocks with default argments ====

  result_factors_zscore <- factors_zscore(ds_test_indicators,
                                          aggregate_formula = GPM + ROCE + PE + PB + CUR + QR,
                                          group_by = c("date"))

  # Select top stocks basing on z-scores ====
  top_score_stocks_list <- zscore_filter_stocks(result_factors_zscore)

  # top scores:  number=5, rank from 1 to 5
  actual_top_ranks <- top_score_stocks_list %>%
    dplyr::mutate(rank = rank(stk_score)) %>%
    dplyr::summarise(max_rank = max(rank), min_rank = min(rank), n = n())
  expect_equal(mean(actual_top_ranks$n), 5)
  expect_equal(mean(actual_top_ranks$max_rank), 5)
  expect_equal(mean(actual_top_ranks$min_rank), 1)

})
