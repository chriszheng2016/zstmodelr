context("Tests for factors classification functions")

test_that("composite_factor, with various arguments", {

  # Load test data
  ds_classify_factors <- readRDS("./data/ds_composite_factors.rds")
  factors_list <- c("GPM", "ROCE", "PE", "PB", "CUR", "QR")
  factors_weight_formula <- weight_formula(
    factors_weight_equal(factors_list),
    new_factor_name = "new_factor"
  )

  # weight_formula with default arguments ====
  ds_composite_factor <- composite_factor(ds_classify_factors,
    aggregate_formula = factors_weight_formula
  )
  expect_true(all("new_factor" %in% names(ds_composite_factor)))
  expect_false(all(factors_list %in% names(ds_composite_factor)))


  # weight_formula with customized arguments ====
  ds_composite_factor <- composite_factor(ds_classify_factors,
    aggregate_formula = Profit ~ 0.3 * GPM + 0.7 * ROCE,
    drop = FALSE
  )
  expect_true(all("Profit" %in% names(ds_composite_factor)))
  expect_true(all(c("GPM", "ROCE") %in% names(ds_composite_factor)))
})

test_that("weight_formula, with various arguments", {
  factors_list <- c("GPM", "ROCE", "PE", "PB", "CUR", "QR")
  factors_weight <- factors_weight_equal(factors_list)

  # weight_formula with default arguments ====
  factors_weight_formula <- weight_formula(factors_weight)
  expect_is(factors_weight_formula, "formula")
  expect_true(all(factors_list %in% all.vars(factors_weight_formula)))
  expect_true(all("composite_factor" %in% all.vars(factors_weight_formula)))

  # weight_formula with customized arguments ====
  factors_weight_formula <- weight_formula(factors_weight,
    new_factor_name = "new_factor"
  )
  expect_is(factors_weight_formula, "formula")
  expect_true(all(factors_list %in% all.vars(factors_weight_formula)))
  expect_true(all("new_factor" %in% all.vars(factors_weight_formula)))
})

test_that("factors_weight_equal, with various arguments", {
  factors_list <- c("GPM", "ROCE", "PE", "PB", "CUR", "QR")

  # factors_weight_equal with default arguments ====
  factors_weight <- factors_weight_equal(factors_list)
  expect_is(factors_weight, "list")
  expected <- c("factor_name", "factor_weight")
  actual <- names(factors_weight)
  expect_equal(actual, expected)
  expect_true(all(factors_weight$factor_weight == 1 / length(factors_list)))
  expect_equal(sum(factors_weight$factor_weight), 1)
})
