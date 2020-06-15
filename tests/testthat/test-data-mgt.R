
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

enable_parallel()

test_that("get_datasource, with various arguments", {

  # get_datasource with default arguments ====
  ds_datasource <- get_datasource(stock_db)
  expect_is(ds_datasource, "data.frame")
  # data.frame fields
  expected_fields <- c(
    "target_table", "input_file", "input_type",
    "input_dir", "start_index", "process",
    "process_source"
  )
  actual_fields <- names(ds_datasource)
  expect_equal(actual_fields, expected_fields)
})

test_that("import_table, with various arguments", {

  # import_table with default arguments ====
  input_file <- "./data/test_table01.csv"
  target_table <- "test_table01"
  log_dir <- "./log"
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

  # import_table with various arguments ====
  # >> argument: input_type ----
  input_file <- "./data/test_table02.txt"
  target_table <- "test_table02"
  log_dir <- "./log"
  original_data <- readr::read_tsv(input_file,
    locale = readr::locale(encoding = "CP936")
  )

  # Import txt data into table
  # Notice: real data start from line 4, so need skip 3 lines
  # Suppress Warnings from read_import_file
  suppressWarnings(
    success <- import_table(stock_db,
      input_file = input_file,
      input_type = "txt",
      start_index = 4L,
      target_table = target_table,
      ignore_problems = TRUE,
      log_dir = log_dir
    )
  )

  # Check return result
  expect_true(success)

  # Check fields and rows
  expect_fields <- tolower(names(original_data))
  # due to real data from line 4 means 2 redundant lines
  expect_rows <- nrow(original_data) - 2
  actual_table <- get_table_dataset(stock_db, target_table)
  expect_true(all(names(actual_table) %in% expect_fields))
  expect_gte(nrow(actual_table), expect_rows)

  # Check problems files
  problems_file <- sprintf(
    "%s/%s(%s).csv",
    log_dir,
    target_table,
    format(Sys.Date(), "%Y-%m-%d")
  )
  expect_true(file.exists(problems_file))

  # >> argument: ignore_problems ----
  input_file <- "./data/test_table02.txt"
  target_table <- "test_table02"
  log_dir <- "./log"
  original_data <- readr::read_tsv(input_file,
    locale = readr::locale(encoding = "CP936")
  )

  expect_error(
    suppressWarnings(
      import_table(stock_db,
        input_file = input_file,
        input_type = "txt",
        start_index = 4L,
        target_table = target_table,
        ignore_problems = FALSE,
        log_dir = log_dir
      )
    ),
    regexp = "Converting problems"
  )
})

test_that("update_db, with various arguments", {

  # test data
  data_source <- get_datasource(stock_db)
  log_file_prefix <- "update_db_log"
  log_dir <- "./log"

  data_source <- data_source[FALSE, ] %>%
    tibble::add_row(
      target_table = "test_table01",
      input_file = "test_table01.csv",
      input_type = "csv",
      input_dir = "./data/",
      start_index = 2L,
      process = NA,
      process_source = NA
    ) %>%
    tibble::add_row(
      target_table = "test_table02",
      input_file = "test_table02.txt",
      input_type = "txt",
      input_dir = "./data/",
      start_index = 4L,
      process = NA,
      process_source = NA
    )

  # update_db with default arguments ====
  expect_message(
    suppressWarnings(
      update_db(stock_db,
        data_source = data_source
      )
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

  # update_db with various arguments ====
  # >> argument: retry, one failure ----

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
    log_dir = log_dir
  )

  # use log file with errors to update db
  # If some table logged with error in log file, we should update them again.
  expect_message(
    suppressWarnings(
      update_db(stock_db,
        data_source = data_source,
        retry_log = basename(log_file_error_path),
        log_file_prefix = log_file_prefix,
        log_dir = log_dir
      )
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


  # >> argument: retry, only one success  ----

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
    log_dir = log_dir
  )

  # use log file with errrs to update db
  # If some tables of data_source haven't any log info in log file,
  # we should update these file in data_source
  expect_message(
    suppressWarnings(
      update_db(stock_db,
        data_source = data_source,
        retry_log = basename(log_file_error_path),
        log_file_prefix = log_file_prefix,
        log_dir = log_dir
      )
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

  # >> argument: retry, wrong log error file ----
  # If there is no retry log file, all tables will be updated.
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


# Tests for data management for stock_db class - non generic functions ----
context("Tests for data management for stock_db - non generic functions")

test_that("read_import_file, with various arguments", {

  # read_import_file with default arguments ====
  input_file <- "./data/test_table01.csv"
  log_dir <- "./log"

  ds_import_data <- read_import_file(input_file)

  expect_is(ds_import_data, "data.frame")
  expect_true(NROW(ds_import_data) > 0)
  expect_fields <- c("field_code", "field_name", "field_category")
  expect_true(all(names(ds_import_data) %in% expect_fields))


  # read_import_file with various arguments ====

  # >> argument: input_file ----
  log_dir <- "./log"

  # Read single file into data.frame
  input_file <- "./data/test_table03_01.txt"
  # Notice: real data start from line 4, so need skip 3 lines
  expect_message(
    ds_import_data_single_file <- read_import_file(
      input_file = input_file,
      input_type = "txt",
      start_index = 4L,
      ignore_problems = TRUE,
      log_dir = log_dir
    ),
    regexp = "test_table03_01.txt"
  )
  expect_is(ds_import_data_single_file, "data.frame")

  # Read multiple files into data.frame
  input_files <- "./data/test_table03_[0-9]+.txt"
  # test_table03_[0-9]+.txt include 3 files with same rows of record:
  # test_table03_01.txt, test_table03_02.txt, test_table03_03.txt
  expect_message(
    suppress_warnings(
      # Ignore some warnings caused by inconsistent type of Crcd column in
      # diferent files
      ds_import_data_multi_files <- read_import_file(
        input_file = input_files,
        input_type = "txt",
        start_index = 4L,
        ignore_problems = TRUE,
        log_dir = log_dir
      ),
      warn_pattern = "parsing failures|Converting problems"
    ),
    regexp = "test_table03_01.txt|test_table03_02.txt|test_table03_03.txt"
  )

  expect_is(ds_import_data_multi_files, "data.frame")
  expect_true(all(
    names(ds_import_data_multi_files) %in% names(ds_import_data_single_file)
  ))
  expect_true(NROW(ds_import_data_multi_files)
  == 3 * NROW(ds_import_data_single_file))

  # Read files that don't existed or matched
  input_file <- "./data/non_existed_files.txt"

  expect_warning(
    ds_import_data_single_file <- read_import_file(
      input_file = input_file,
      input_type = "txt",
      start_index = 4L,
      ignore_problems = TRUE,
      log_dir = log_dir
    ),
    regexp = "non_existed_files.txt"
  )

  expect_null(ds_import_data_single_file)


  # >> argument: input_type ----
  input_file <- "./data/test_table02.txt"
  file_name <- tools::file_path_sans_ext(basename(input_file))
  log_dir <- "./log"

  # Read txt data into data.frame
  # Notice: real data start from line 4, so need skip 3 lines
  expect_warning(
    ds_import_data <- read_import_file(
      input_file = input_file,
      input_type = "txt",
      start_index = 4L,
      ignore_problems = TRUE,
      log_dir = log_dir
    ),
    regexp = "Converting problems"
  )
  expect_is(ds_import_data, "data.frame")
  expect_true(NROW(ds_import_data) > 0)

  # Check problems files
  problems_file <- sprintf(
    "%s/%s(%s).csv",
    log_dir,
    file_name,
    format(Sys.Date(), "%Y-%m-%d")
  )
  expect_true(file.exists(problems_file))


  # >> argument: ignore_problems ----
  input_file <- "./data/test_table02.txt"
  log_dir <- "./log"

  expect_error(
    suppressWarnings(
      ds_import_data <- read_import_file(
        input_file = input_file,
        input_type = "txt",
        start_index = 4L,
        ignore_problems = FALSE,
        log_dir = log_dir
      )
    ),
    regexp = "Converting problems"
  )
})

test_that("write_import_file, with various arguments", {
  origin_file <- "./data/test_table01.csv"
  ds_output <- read_import_file(origin_file)

  # write_import_file with default arguments ====
  output_file_name <- tools::file_path_sans_ext(basename(origin_file))
  output_file_name <- paste0(output_file_name, "_origin")
  output_file_ext <- "csv"
  output_dir <- "./output"
  output_file_path <- file.path(
    output_dir,
    paste0(output_file_name, ".", output_file_ext)
  )

  write_import_file(ds_output, output_file = output_file_path)

  expect_true(file.exists(output_file_path))
  ds_output_readback <- read_import_file(input_file = output_file_path)
  expect_equal(ds_output, ds_output_readback)


  # write_import_file with various arguments ====
  # >> output_type ----
  output_file_name <- tools::file_path_sans_ext(basename(origin_file))
  output_dir <- "./output"

  output_types <- c("csv", "txt")
  for (output_type in output_types) {
    output_file_ext <- output_type
    output_file_path <- paste0(output_file_name, ".", output_file_ext)
    write_import_file(ds_output,
      output_file = output_file_path,
      output_type = output_type,
      output_dir = output_dir
    )
    expect_true(file.exists(file.path(output_dir, output_file_path)))
    ds_output_readback <- read_import_file(
      input_file = output_file_path,
      input_dir = output_dir,
      input_type = output_type
    )
    expect_equal(ds_output, ds_output_readback)
  }
})

test_that("convert_import_file, with various arguments", {
  origin_file <- "./data/test_ttm_FS_Comins.csv"

  # convert_import_file with default arguments ====
  output_file_name <- tools::file_path_sans_ext(basename(origin_file))
  output_file_ext <- "csv"
  output_dir <- "./output"
  output_file_path <- file.path(
    output_dir,
    paste0(output_file_name, ".", output_file_ext)
  )

  convert_import_file(origin_file, output_file = output_file_path)
  expect_true(file.exists(output_file_path))



  # convert_import_file with various arguments ====
  # >> output_type ----
  output_file_name <- tools::file_path_sans_ext(basename(origin_file))
  output_file_name <- paste0(output_file_name, "_convert_no_process_fun")
  output_dir <- "./output"

  output_types <- c("csv", "txt")
  for (output_type in output_types) {
    output_file_ext <- output_type
    output_file_path <- paste0(output_file_name, ".", output_file_ext)
    convert_import_file(origin_file,
      input_type = "csv",
      start_index = 2L,
      ignore_problems = TRUE,
      log_dir = "./log",
      output_file = output_file_path,
      output_type = output_type,
      output_dir = output_dir
    )
    expect_true(file.exists(file.path(output_dir, output_file_path)))
  }

  # >> process_fun ----
  output_file_name <- tools::file_path_sans_ext(basename(origin_file))
  output_file_name <- paste0(output_file_name, "_convert_process_fun")
  output_file_ext <- "csv"
  output_dir <- "./output"
  output_file_path <- file.path(
    output_dir,
    paste0(output_file_name, ".", output_file_ext)
  )

  process_fun <- function(x) {
    x <- x %>%
      dplyr::bind_rows(x)
  }

  convert_import_file(origin_file,
    process_fun = process_fun,
    output_file = output_file_path
  )

  expect_true(file.exists(output_file_path))
})

test_that("ttm_financial_report, with various arguments", {
  origin_report <- read_import_file("./data/test_ttm_FS_Comins.csv")

  # ttm_financial_report with default arguments ====
  ttm_report <- ttm_financial_report(origin_report,
    date_index_field = "Accper",
    key_fields = c("Stkcd", "Typrep")
  )

  # check output format
  expect_is(ttm_report, "data.frame")

  # check field names
  expect_non_value_fields <- c("Stkcd", "Typrep", "Accper")
  expect_value_fields <- stringr::str_subset(names(origin_report),
    pattern = sprintf(
      "[^%s]",
      stringr::str_c(expect_non_value_fields,
        collapse = "|"
      )
    )
  )
  expect_value_fields <- paste0(expect_value_fields, "_", "ttm")
  expect_fields <- c(expect_non_value_fields, expect_value_fields)
  actual_fields <- names(ttm_report)
  expect_true(all(actual_fields %in% expect_fields))

  # check period of date
  expect_true(is_periodic_dates(ttm_report[["Accper"]],
    freq_rule = "quarter",
    regular = FALSE
  ))

  # ttm_financial_report with various arguments ====
  for (parallel in c(TRUE, FALSE)) {
    ttm_report <- ttm_financial_report(origin_report,
      date_index_field = "Accper",
      key_fields = c("Stkcd", "Typrep"),
      parallel = parallel
    )

    # check output format
    expect_is(ttm_report, "data.frame")

    # check field names
    expect_non_value_fields <- c("Stkcd", "Typrep", "Accper")
    expect_value_fields <- stringr::str_subset(names(origin_report),
      pattern = sprintf(
        "[^%s]",
        stringr::str_c(expect_non_value_fields,
          collapse = "|"
        )
      )
    )
    expect_value_fields <- paste0(expect_value_fields, "_", "ttm")
    expect_fields <- c(expect_non_value_fields, expect_value_fields)
    actual_fields <- names(ttm_report)
    expect_true(all(actual_fields %in% expect_fields))

    # check period of date
    expect_true(is_periodic_dates(ttm_report[["Accper"]],
      freq_rule = "quarter",
      regular = FALSE
    ))
  }
})

test_that("process_files, with various arguments", {

  # test data
  data_source <- get_datasource(stock_db)
  log_file_prefix <- "process_files_log"
  log_dir <- "./log"

  data_source <- data_source[FALSE, ] %>%
    tibble::add_row(
      target_table = "FS_Comins",
      input_file = "test_ttm_FS_Comins.csv",
      input_type = "csv",
      input_dir = "./data/",
      start_index = 2L,
      process = NA,
      process_source = NA
    ) %>%
    tibble::add_row(
      target_table = "FS_Comins_ttm01",
      input_file = "test_ttm_FS_Comins_ttm01.csv",
      input_type = "csv",
      input_dir = "./output/",
      start_index = 2L,
      process = "TTM",
      process_source = "test_ttm_FS_Comins.csv"
    ) %>%
    tibble::add_row(
      target_table = "FS_Comins_ttm02",
      input_file = "test_ttm_FS_Comins_ttm02.csv",
      input_type = "csv",
      input_dir = "./output/",
      start_index = 2L,
      process = "TTM",
      process_source = "test_ttm_FS_Comins.csv"
    )

  # process_files with default arguments ====
  expect_message(
    process_files(stock_db,
      data_source = data_source
    ),
    regexp = "test_ttm_FS_Comins_ttm01.csv|test_ttm_FS_Comins_ttm02.csv"
  )

  # check output file
  data_source_ttm <- data_source %>%
    dplyr::filter(process == "TTM")

  for (i in seq_len(NROW(data_source_ttm))) {
    output_file_path <- data_source_ttm$input_file[i]
    output_dir <- data_source_ttm$input_dir[i]
    output_file_path <- file.path(output_dir, output_file_path)
    expect_true(file.exists(output_file_path))
  }

  # check log file
  log_file_path <- sprintf(
    "%s/%s_%s(current).csv",
    log_dir,
    log_file_prefix,
    target_database
  )
  expect_true(file.exists(log_file_path))
  log_info <- read_log(basename(log_file_path), log_dir = log_dir)
  expect_true(all(data_source_ttm$input_file %in% log_info$input_file))


  # process_files with various arguments ====
  # >> argument: retry, one failure ----
  # build log file with error for retry dynamically
  ds_log_error <- tibble::tibble(
    input_file = c(
      "test_ttm_FS_Comins_ttm01.csv",
      "test_ttm_FS_Comins_ttm02.csv"
    ),
    source_file = c(
      "test_ttm_FS_Comins.csv",
      "test_ttm_FS_Comins.csv"
    ),
    success = c(TRUE, FALSE)
  )

  log_file_error_prefix <- sprintf(
    "process_files_log_error_%s",
    target_database
  )

  log_file_error_path <- save_log(ds_log_error,
    log_file_prefix = log_file_error_prefix,
    log_dir = log_dir
  )

  # use log file with errors to process files
  # If some input_file logged with error in log file, we should process them again.
  expect_message(
    process_files(stock_db,
      data_source = data_source,
      retry_log = basename(log_file_error_path),
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    ),
    regexp = "test_ttm_FS_Comins_ttm02.csv"
  )

  data_source_ttm <- data_source %>%
    dplyr::filter(process == "TTM")

  output_file_path <- data_source_ttm$input_file[2]
  output_dir <- data_source_ttm$input_dir[2]
  output_file_path <- file.path(output_dir, output_file_path)
  expect_true(file.exists(output_file_path))

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
    dplyr::filter(input_file == "test_ttm_FS_Comins_ttm02.csv")
  expect_true(log_info$success == TRUE)


  # >> argument: retry, only one success  ----

  # build log file with error for retry dynamically
  ds_log_error <- tibble::tibble(
    input_file = c("test_ttm_FS_Comins_ttm01.csv"),
    source_file = c("test_ttm_FS_Comins.csv"),
    success = c(TRUE)
  )

  log_file_error_prefix <- sprintf(
    "process_files_log_error_%s",
    target_database
  )

  log_file_error_path <- save_log(ds_log_error,
    log_file_prefix = log_file_error_prefix,
    log_dir = log_dir
  )

  # use log file with errrs to process files
  # If some input_files of data_source haven't any log info in log file,
  # we should process these input_file in data_source
  expect_message(
    process_files(stock_db,
      data_source = data_source,
      retry_log = basename(log_file_error_path),
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    ),
    regexp = "test_ttm_FS_Comins_ttm02.csv ..."
  )

  # check output file
  data_source_ttm <- data_source %>%
    dplyr::filter(process == "TTM")

  output_file_path <- data_source_ttm$input_file[2]
  output_dir <- data_source_ttm$input_dir[2]
  output_file_path <- file.path(output_dir, output_file_path)
  expect_true(file.exists(output_file_path))


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
    dplyr::filter(input_file == "test_ttm_FS_Comins_ttm02.csv")
  expect_true(log_info$success == TRUE)

  # >> argument: retry, wrong log error file ----
  # If there is no retry log file, all tables will be updated.
  log_file_error_path <- sprintf(
    "process_files_log_error_bad_%s.csv",
    target_database
  )

  expect_warning(
    process_files(stock_db,
      data_source = data_source,
      retry_log = basename(log_file_error_path),
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    ),
    regexp = "Since there is no a retry log|all input files will be updated"
  )

  # check output file
  data_source_ttm <- data_source %>%
    dplyr::filter(process == "TTM")

  for (i in seq_len(NROW(data_source_ttm))) {
    output_file_path <- data_source_ttm$input_file[i]
    output_dir <- data_source_ttm$input_dir[i]
    output_file_path <- file.path(output_dir, output_file_path)
    expect_true(file.exists(output_file_path))
  }

  # check log file
  log_file_path <- sprintf(
    "%s/%s_%s(current).csv",
    log_dir,
    log_file_prefix,
    target_database
  )
  expect_true(file.exists(log_file_path))
  log_info <- read_log(basename(log_file_path), log_dir = log_dir)
  expect_true(all(data_source_ttm$input_file %in% log_info$input_file))
})

# Clear up testing context
DBI::dbRemoveTable(stock_db$connection, "test_table01")
DBI::dbRemoveTable(stock_db$connection, "test_table02")
suppressMessages(close_stock_db(stock_db))

disable_parallel()
