# Tests for utility functions of logfile ----

context("Tests for utitlity functions of logfile")

ds_test_log_info <- tibble::tribble(
  ~target_table, ~input_file, ~success,
  #------------/-------------------/---------
  "test_table01", "test_table01.csv", TRUE,
  "test_table02", "test_table02.txt", TRUE
)


test_that("save_log, with various arguments", {

  # save_log on default arguments  ====
  log_file_prefix <- "test_log_info"
  actual_log_file_path <- save_log(ds_test_log_info, log_file_prefix)

  expect_log_file_path <- sprintf(
    "%s/%s(current).csv",
    "./log",
    log_file_prefix
  )
  expect_equal(actual_log_file_path, expect_log_file_path)
  expect_true(file.exists(actual_log_file_path))

  # save_log on various arguments  ====
  log_file_prefix <- "test_log_info"
  log_dir <- "./log"
  actual_log_file_path <- save_log(ds_test_log_info,
                                   log_file_prefix,
                                   log_dir = log_dir)
  expect_log_file_path <- sprintf(
    "%s/%s(current).csv",
    log_dir,
    log_file_prefix
  )
  expect_equal(actual_log_file_path, expect_log_file_path)
  expect_true(file.exists(actual_log_file_path))
})

test_that("read_log, with various arguments", {

  # read_log on default arguments  ====
  log_file_name <- "test_log_info(current).csv"
  test_log_info <- read_log(log_file_name)
  expect_fields <- names(test_log_info)
  actual_fields <- names(ds_test_log_info)
  expect_true(all(actual_fields %in% expect_fields))

  # read_log on various arguments  ====

  # existed log file
  log_file_name <- "test_log_info(current).csv"
  log_dir <- "./log"
  test_log_info <- read_log(log_file_name, log_dir)
  expect_is(test_log_info, "data.frame")
  expect_fields <- names(test_log_info)
  actual_fields <- names(ds_test_log_info)
  expect_true(all(actual_fields %in% expect_fields))

  # nonexised log file
  log_file_name <- "test_log_info_non_exisit(current).csv"
  log_dir <- "./log"
  test_log_info <- read_log(log_file_name, log_dir)
  expect_null(test_log_info)

})
