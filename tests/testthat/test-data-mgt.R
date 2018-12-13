
# Tests for data management for stock_db class - generic functions ----
context("Tests for data management for stock_db - generic functions")

# Set up testing context
dsn <- "GTA_SQLData_TEST"
stock_db <- stock_db(gta_db, dsn)
suppressMessages(db_ready <- open_stock_db(stock_db))
# skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for data-mangement of stock_db!", dsn)
)
suppressMessages(init_stock_db(stock_db))
db_info <- DBI::dbGetInfo(stock_db$connection)
target_database <- db_info$dbname


test_that("get_datasource, with various arguments", {

  # get_datasource with default arguments ====
  ds_datasource <- get_datasource(stock_db)
  expect_is(ds_datasource, "data.frame")
  # data.frame fields
  expected_fields <- c(
    "target_table", "input_file", "input_type",
    "input_dir", "start_index"
  )
  actual_fields <- names(ds_datasource)
  expect_equal(actual_fields, expected_fields)
})

test_that("import_table, with various arguments", {

  # import_table with default arguments ====
  input_file <- "./data/test_table01.csv"
  target_table <- "test_table01"
  problems_dir <- "./log"
  original_data <- readr::read_csv(input_file,
    locale = readr::locale(encoding = "CP936")
  )
  success <- import_table(stock_db,
    input_file = input_file
  )

  # Check return result
  expect_true(success)

  # Check fields and rows
  expect_fields <- tolower(names(original_data))
  expect_rows <- nrow(original_data)
  actual_table <- get_table_dataset(stock_db, target_table)
  expect_true(all(names(actual_table) %in% expect_fields))
  expect_gte(nrow(actual_table), expect_rows)

  # Check problems files
  if (expect_rows > nrow(actual_table)) {
    problems_file <- sprintf(
      "%s/%s(%s).csv",
      problems_dir,
      target_table,
      format(Sys.Date(), "%Y-%m-%d")
    )
    expect_true(file.exists(problems_file))
  }

  # import_table with arguments: input_type====
  input_file <- "./data/test_table02.txt"
  target_table <- "test_table02"
  problems_dir <- "./output"
  original_data <- readr::read_tsv(input_file,
    locale = readr::locale(encoding = "CP936")
  )

  # Import txt data into table
  # Notice: real data start from line 4, so need skip 3 lines
  success <- import_table(stock_db,
    input_file = input_file,
    input_type = "txt",
    start_index = 4,
    target_table = target_table,
    ignore_problems = TRUE,
    problems_dir = problems_dir
  )

  # Check return result
  expect_true(success)

  # Check fields and rows
  expect_fields <- tolower(names(original_data))
  expect_rows <- nrow(original_data) - 3 # due to real data from line 4
  actual_table <- get_table_dataset(stock_db, target_table)
  expect_true(all(names(actual_table) %in% expect_fields))
  expect_gte(nrow(actual_table), expect_rows)

  # Check problems files
  if (expect_rows > nrow(actual_table)) {
    problems_file <- sprintf(
      "%s/%s(%s).csv",
      problems_dir,
      target_table,
      format(Sys.Date(), "%Y-%m-%d")
    )
    expect_true(file.exists(problems_file))
  }

  # import_table with arguments: ignore_problems ====
  success <- import_table(stock_db,
    input_file = input_file,
    input_type = "txt",
    start_index = 4,
    target_table = target_table,
    ignore_problems = FALSE,
    problems_dir = problems_dir
  )

  # Check return result
  expect_false(success)
})

test_that("update_db, with various arguments", {

  # test data
  data_source <- get_datasource(stock_db)
  data_source <- data_source[FALSE, ]
  log_file_prefix <- "update_db_log"
  log_dir <- "./log"

  data_source <- data_source %>%
    tibble::add_row(
      target_table = "test_table01",
      input_file = "test_table01.csv",
      input_type = "csv",
      input_dir = "./data/",
      start_index = 2
    ) %>%
    tibble::add_row(
      target_table = "test_table02",
      input_file = "test_table02.txt",
      input_type = "txt",
      input_dir = "./data/",
      start_index = 4
    )

  # update_db with default arguments ====
  expect_message(
    update_db(stock_db,
      data_source = data_source
    ),
    regexp = "Import data into test_table01|test_table02 ..."
  )

  # check log file
  log_file_path <- sprintf(
    "%s/%s_%s(current).csv",
    log_dir,
    log_file_prefix,
    target_database
  )
  expect_true(file.exists(log_file_path))
  log_info <- read_log(basename(log_file_path), log_dir = log_dir)
  expect_true(all(data_source$target_table %in% log_info$target_table))

  # update_db with retry arguments: one failure ====

  # build log file with error for retry dynamically
  ds_log_error <- tibble::tibble(
    target_table = c("test_table01", "test_table02"),
    input_file = c("test_table01.csv", "test_table02.txt"),
    success = c(TRUE, FALSE)
  )

  log_file_error_prefix <- sprintf(
    "update_db_log_error_%s",
    target_database
  )

  log_file_error_path <- save_log(ds_log_error,
                            log_file_prefix = log_file_error_prefix,
                            log_dir = log_dir)

  # use log file with errors to update db
  # when some table log with error in log file, we should update them again.
  expect_message(
    update_db(stock_db,
      data_source = data_source,
      retry_log = basename(log_file_error_path),
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    ),
    regexp = "Import data into test_table02 ..."
  )

  # check log file
  log_file_path <- sprintf(
    "%s/%s_%s(current).csv",
    log_dir,
    log_file_prefix,
    target_database
  )
  expect_true(file.exists(log_file_path))

  log_info <- read_log(basename(log_file_path), log_dir = log_dir)
  log_info <- log_info %>%
    dplyr::filter(target_table == "test_table02")
  expect_true(log_info$success == TRUE)


  # update_db with retry arguments: only one success  ====

  # build log file with error for retry dynamically
  ds_log_error <- tibble::tibble(
    target_table = c("test_table01"),
    input_file = c("test_table01.csv"),
    success = c(TRUE)
  )

  log_file_error_prefix <- sprintf(
    "update_db_log_error_%s",
    target_database
  )

  log_file_error_path <- save_log(ds_log_error,
                            log_file_prefix = log_file_error_prefix,
                            log_dir = log_dir)

  # use log file with errrs to update db
  # when some tables of data_source haven't any log info in log file,
  # we should update these file in data_source
  expect_message(
    update_db(stock_db,
              data_source = data_source,
              retry_log = basename(log_file_error_path),
              log_file_prefix = log_file_prefix,
              log_dir = log_dir
    ),
    regexp = "Import data into test_table02 ..."
  )

  # check log file
  log_file_path <- sprintf(
    "%s/%s_%s(current).csv",
    log_dir,
    log_file_prefix,
    target_database
  )
  expect_true(file.exists(log_file_path))

  log_info <- read_log(basename(log_file_path), log_dir = log_dir)
  log_info <- log_info %>%
    dplyr::filter(target_table == "test_table02")
  expect_true(log_info$success == TRUE)

  # update_db with retry arguments: wrong log error file ====
  # When there is retry log file, all tables will be updated.
  log_file_error_path <- sprintf(
    "update_db_log_error_bad_%s.csv",
    target_database
  )

  expect_warning(
    update_db(stock_db,
              data_source = data_source,
              retry_log = basename(log_file_error_path),
              log_file_prefix = log_file_prefix,
              log_dir = log_dir
    ),
    regexp = "Since there is no a retry log|all tables will be updated"
  )

  # check log file
  log_file_path <- sprintf(
    "%s/%s_%s(current).csv",
    log_dir,
    log_file_prefix,
    target_database
  )
  expect_true(file.exists(log_file_path))
  log_info <- read_log(basename(log_file_path), log_dir = log_dir)
  expect_true(all(data_source$target_table %in% log_info$target_table))

})

test_that("clear_tables, with various arguments", {

  # clear_tables with default arguments ====
  clear_tables(stock_db)

  ds_table <- get_table_dataset(stock_db, "test_table01")
  expect_true(nrow(ds_table) == 0)

  ds_table <- get_table_dataset(stock_db, "test_table02")
  expect_true(nrow(ds_table) == 0)
})


# clear up testing conext
DBI::dbRemoveTable(stock_db$connection, "test_table01")
DBI::dbRemoveTable(stock_db$connection, "test_table02")
suppressMessages(close_stock_db(stock_db))
