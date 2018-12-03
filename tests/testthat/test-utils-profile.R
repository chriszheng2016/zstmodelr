# Tests for utitlity functions of profile ----

context("Tests for utitlity functions of profile")

test_that("get_profile_path, with various arguments", {

  # get_profile_path with default argurments ====
  profile_name <- .GTA_PROFILE_FILE
  profile <- get_profile_path(profile_name)
  expect_true(file.exists(profile))

  profile_name <- "gta_profile1.xlsx"
  expect_error(get_profile_path(profile_name))

  # get_profile_path with various argurments ====
  profile_name <- "gta_profile.xlsx"
  profile_dir <- "etc"
  profile <- get_profile_path(profile_name, profile_dir = profile_dir)
  expect_true(file.exists(profile))

  profile_name <- "gta_profile1.xlsx"
  expect_error(get_profile_path(profile_name, profile_dir = profile_dir))

})

test_that("profile_get_varible_setting, with various arguments", {

  profile_name <- .GTA_PROFILE_FILE
  profile <- get_profile_path(profile_name)

  # profile_get_varible_setting with various argurments ====
  result_value <- profile_get_varible_setting(profile,
                                           variable = "gta_fieldname_list")
  expect_true(is.character(result_value))

})

test_that("profile_get_factors, with various arguments", {

  profile_name <- .GTA_PROFILE_FILE
  profile <- get_profile_path(profile_name)

  # profile_get_factors with default argurments ====
  result_value <- profile_get_factors(profile)
  if (!is.null(result_value)) {
    expect_fields <- c("factor_code", "factor_name","factor_type",
                       "factor_group", "factor_description",
                       "indicator_code", "indicator_name",
                       "indicator_source", "indicator_desciption")
  }

  # profile_get_factors with argurments:factor_codes ====
  factor_codes = c("GPM", "OPM")
  result_value <- profile_get_factors(profile,
                                     factor_codes = factor_codes)
  if (!is.null(result_value)) {
    expect_fields <- c("factor_code", "factor_name","factor_type",
                        "factor_group", "factor_description",
                        "indicator_code", "indicator_name",
                        "indicator_source", "indicator_desciption")

    expect_true(all(expect_fields %in% names(result_value)))
    expect_true(all(result_value$factor_code %in% factor_codes))
  }

  # profile_get_factors with argurments:factor_groups ====
  factor_groups = "Operating Profitability"
  result_value <- profile_get_factors(profile,
                                      factor_groups = factor_groups)
  if (!is.null(result_value)) {
    expect_fields <- c("factor_code", "factor_name","factor_type",
                       "factor_group", "factor_description",
                       "indicator_code", "indicator_name",
                       "indicator_source", "indicator_desciption")

    expect_true(all(expect_fields %in% names(result_value)))
    expect_true(all(result_value$factor_group %in% factor_groups))
  }

})

test_that("profile_get_datasource_files, with various arguments", {

  profile_name <- .GTA_PROFILE_FILE
  profile <- get_profile_path(profile_name)

  # profile_get_datasource_files with various argurments ====
  result_value <- profile_get_datasource_files(profile)
  if (!is.null(result_value)) {
    expect_fields <- c("target_table", "input_file", "input_type",
                       "start_index", "description",
                       "is_valid")

    expect_true(all(expect_fields %in% names(result_value)))
  }

})

test_that("profile_get_indicators, with various arguments", {

  profile_name <- .GTA_PROFILE_FILE
  profile <- get_profile_path(profile_name)

  # profile_get_indicators with default argurments ====
  result_value <- profile_get_indicators(profile)
  if (!is.null(result_value)) {
    expect_fields <- c("ind_code", "ind_type", "ind_name",
                       "ind_category", "ind_source",
                       "ind_description")

    expect_true(all(expect_fields %in% names(result_value)))
  }

  # profile_get_indicators with various argurments ====
  indicator_codes = c("b001100000", "b001101000")
  result_value <- profile_get_indicators(profile,
                            indicator_codes = indicator_codes)
  if (!is.null(result_value)) {
    expect_fields <- c("ind_code", "ind_type", "ind_name",
                       "ind_category", "ind_source",
                       "ind_description")

    expect_true(all(expect_fields %in% names(result_value)))
    expect_true(all(result_value$ind_code %in% indicator_codes))
  }

})

test_that("profile_get_customized_indicators, with various arguments", {
  profile_name <- .GTA_PROFILE_FILE
  profile <- get_profile_path(profile_name)

  # profile_get_customized_indicators with various argurments ====
  result_value <- profile_get_customized_indicators(profile)
  if (!is.null(result_value)) {
    expect_fields <- c("ind_code", "ind_type", "ind_name",
                       "ind_category", "ind_source", "ind_description",
                       "ind_formula", "rolling_window", "period",
                       "is_active")

    expect_true(all(expect_fields %in% names(result_value)))
  }

})
