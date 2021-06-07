# Tests for function of indicator generate  ----
context("Tests for function of indicator generate")

# Set up testing context
dsn <- "GTA_SQLData"
DB_PROFILE_FILE <- "gta_profile.xlsx"

skip_if_stock_db_not_ready(dsn)
# Open database for tests
stock_db <- stock_db(gta_db, dsn)
suppressMessages(open_stock_db(stock_db))
withr::defer({
  close_stock_db(stock_db)
})
suppressMessages(init_stock_db(stock_db))


# Enable parallel process for test
local_parallel("ON")


# prepare test data sets
customized_indicators_info <- tibble::tibble(
  ind_code = c("m_ep_ttm", "q_ep_ttm"),
  ind_type = "",
  ind_name = c("Month_EP_TTM", "Quater_EP_TTM"),
  ind_category = "customized",
  ind_source = c("test_m_ep_ttm.csv", "test_q_ep_ttm.csv"),
  ind_description = "",
  ind_formula = c(
    "price <- dplyr::lag(mclsprc, n=2)
                 earning <- f090101c
                 ep <- earning/price",
    "price <- dplyr::lag(mclsprc, n=2)
                 earning <- f090101c
                 ep <- earning/price"
  ),
  ind_keys = list(c("stkcd")),
  rolling_window = 0,
  period = c("month", "quarter"),
  fillna_method = "ffill",
  output_format = "csv",
  is_active = TRUE
)

# create test_indicator_defs
test_indicator_defs <- build_indicator_defs.gta_db(stock_db, customized_indicators_info)

test_that("generate_indicators", {
  log_file_prefix <- "generate_indicator_log"
  log_dir <- "log"
  validate_stkcds <- c("600031", "000157", "600066", "000550")

  # generate_indicators with default arguments ====
  ds_indicators <- generate_indicators(stock_db,
    ds_indicator_defs = test_indicator_defs,
    validate_def = TRUE
  )

  # check output file
  dir_indicators <- dir_path_db(stock_db, "DIR_DB_DATA_INDICATOR")
  output_files_path <- paste0(dir_indicators, "/", test_indicator_defs$ind_source)
  # Notice: Don't delete output files, because they will be used for tests of
  # delete_indicators
  purrr::map(output_files_path, ~ expect_true(file.exists(.x)))

  # check log file
  log_file_path <- sprintf(
    "%s/%s(current).csv",
    log_dir,
    log_file_prefix
  )
  withr::with_file(log_file_path, {
    expect_true(file.exists(log_file_path))
    log_info <- read_log(basename(log_file_path), log_dir = log_dir)
    expect_true(all(test_indicator_defs$ind_code %in% log_info$ind_code))
  })


  # generate_indicators with various arguments ====
  log_file_prefix <- "generate_indicator_log1"
  log_dir <- "log"

  ds_indicators <- generate_indicators(stock_db,
    ds_indicator_defs = test_indicator_defs,
    validate_def = TRUE,
    validate_stkcds = validate_stkcds,
    parallel = FALSE,
    log_file_prefix = log_file_prefix,
    log_dir = log_dir
  )

  # check output file
  dir_indicators <- dir_path_db(stock_db, "DIR_DB_DATA_INDICATOR")
  output_files_path <- paste0(dir_indicators, "/", test_indicator_defs$ind_source)
  purrr::map(output_files_path, ~ expect_true(file.exists(.x)))

  # check log file
  log_file_path <- sprintf(
    "%s/%s(current).csv",
    log_dir,
    log_file_prefix
  )
  withr::with_file(log_file_path, {
    expect_true(file.exists(log_file_path))
    log_info <- read_log(basename(log_file_path), log_dir = log_dir)
    expect_true(all(test_indicator_defs$ind_code %in% log_info$ind_code))
  })
})

test_that("backup_indicators", {

  ## backup_indicators with various arguments ====
  backup_dir <- "backup"
  backup_dir_path <- backup_indicators(stock_db,
    ds_indicator_defs = test_indicator_defs,
    backup_dir = backup_dir
  )

  # check output file
  backup_files_path <- paste0(backup_dir_path, "/", test_indicator_defs$ind_source)
  purrr::map(backup_files_path, ~ expect_true(file.exists(.x)))
})

test_that("delete_indicators", {
  ## delete_indicators with various arguments ====
  delete_indicators(stock_db,
    ds_indicator_defs = test_indicator_defs
  )

  # check output file
  dir_indicators <- dir_path_db(stock_db, "DIR_DB_DATA_INDICATOR")
  target_files_path <- paste0(dir_indicators, "/", test_indicator_defs$ind_source)
  purrr::map(target_files_path, ~ expect_false(file.exists(.x)))
})
