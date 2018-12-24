#' @include data-mgt.R
#' @include stock-db-gta.R


# Generic functions implemetation by gta_db class ------------------------

# Get data source info for importing raw data
# Method definition for s3 generic
# @describeIn get_datasource get data source for importing raw data into
#'   database of gta_db class
#' @export
get_datasource.gta_db <- function(stock_db) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # get profile of stock_db
  gta_profile_name <- get_profile(stock_db)
  if (is.null(gta_profile_name)) {
    success <- FALSE
  }

  # get data source info from profile
  ds_datasource <- NULL
  if (success) {
    ds_datasource_files <- profile_get_datasource_files(gta_profile_name)
    input_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_ORIGIN")

    # build specified result of datasource
    ds_datasource <- ds_datasource_files %>%
      dplyr::mutate(
        input_dir = input_dir,
        start_index = as.integer(start_index)
      ) %>%
      dplyr::select(
        target_table = target_table,
        input_file = input_file,
        input_type = input_type,
        input_dir = input_dir,
        start_index = start_index,
        process = process,
        process_source = process_source
      )
  }

  return(ds_datasource)
}

# Method definition for s4 generic
#' @describeIn get_datasource get data source for importing raw data into
#'   database of gta_db class
#' @export
setMethod(
  "get_datasource",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    get_datasource.gta_db(stock_db)
  }
)

# Clear data in all tables in stock database
# Method definition for s3 generic
# @describeIn clear_tables clear data in all tables in a database of gta_db class
#' @export
clear_tables.gta_db <- function(stock_db) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # get target database info
  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname
  assertive::assert_is_not_null(target_database)

  # clear old data in all tables
  sql_cmd <- "EXEC sp_MSforeachtable \"truncate table ?\""
  DBI::dbExecute(stock_db$connection, sql_cmd)

  # shrink database log
  sql_cmd <- sprintf("DBCC ShrinkDatabase(%s, 0, TRUNCATEONLY)", target_database)
  # sql_cmd <- sprintf("DBCC ShrinkFile(%s_log, 1, TRUNCATEONLY)", target_database)
  DBI::dbExecute(stock_db$connection, sql_cmd)

  return(invisible(NULL))
}

# Method definition for s4 generic
#' @describeIn clear_tables clear data in all tables in a database of gta_db class
#' @export
setMethod(
  "clear_tables",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    clear_tables.gta_db(stock_db)
  }
)

# Update tables in stock database
# Method definition for s3/s4 generic
# @describeIn update_db update tables in a database of gta_db class
#' @export
update_db.gta_db <- function(stock_db,
                             data_source = get_datasource(stock_db),
                             retry_log = NULL,
                             log_file_prefix = "update_db_log",
                             log_dir = "./log") {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_not_null(data_source)
  assertive::assert_is_data.frame(data_source)
  assertive::assert_is_character(log_file_prefix)
  assertive::assert_is_character(log_dir)

  # get target database info
  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname
  assertive::assert_is_not_null(target_database)

  # setup log info params
  log_file_prefix <- sprintf(
    "%s_%s",
    log_file_prefix,
    target_database
  )
  log_file_path <- NULL
  ds_log <- data_source %>%
    dplyr::select(target_table, input_file) %>%
    dplyr::mutate(success = FALSE)

  # collect failed tables from log file
  failed_tables <- NULL
  finished_tables <- NULL
  if (!is.null(retry_log)) {
    ds_retry_log <- read_log(retry_log, log_dir = log_dir)

    if (!is.null(ds_retry_log)) {

      # find out failed tables
      ds_faild_tables <- ds_retry_log %>%
        dplyr::filter(success == FALSE)
      if (nrow(ds_faild_tables) > 0) {
        failed_tables <- ds_faild_tables$target_table
      }

      # find out finished tables
      ds_finished_tables <- ds_retry_log %>%
        dplyr::filter(success == TRUE)
      if (nrow(ds_finished_tables) > 0) {
        finished_tables <- ds_finished_tables$target_table
      }
    } else {
      msg <- sprintf(
        "Since there is no a retry log(%s), \n all tables will be updated.",
        retry_log
      )
      rlang::warn(msg)
    }
  }

  # import data from data source
  ds_data_source <- data_source
  need_clear_dblog <- FALSE
  for (i in seq_len(nrow(ds_data_source))) {
    data_source_info <- ds_data_source[i, ]

    # conduct update for all tables or tables recording failed in retry log
    if (is.null(failed_tables) || data_source_info$target_table %in% failed_tables) {
      # conduct update for all tables or tables not recording successful in retry log
      if (is.null(finished_tables) || !(data_source_info$target_table %in% finished_tables)) {
        msg <- sprintf("Import data into %s ...\n", data_source_info$target_table)
        rlang:::inform(msg)

        result <- import_table(stock_db,
          input_file = data_source_info$input_file,
          input_type = data_source_info$input_type,
          input_dir = data_source_info$input_dir,
          start_index = as.integer(data_source_info$start_index),
          target_table = data_source_info$target_table,
          ignore_problems = TRUE,
          log_dir = log_dir
        )

        need_clear_dblog <- TRUE
        ds_log$success[i] <- result

        if (result != TRUE) {
          msg <- sprintf("fail to update %s !!\n", data_source_info$target_table)
          rlang:::inform(msg)
        }
      } else {
        ds_log$success[i] <- TRUE
      }
    } else {
      ds_log$success[i] <- TRUE
    }

    # write log for update operation
    log_file_path <- save_log(ds_log,
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    )
  }

  # shrink database log if needed
  if (need_clear_dblog) {
    # sql_cmd <- sprintf("DBCC ShrinkDatabase(%s, 0)", target_database )
    sql_cmd <- sprintf(
      "DBCC ShrinkFile(%s_log, 1, TRUNCATEONLY)",
      target_database
    )
    DBI::dbExecute(stock_db$connection, sql_cmd)
  }

  # Notify user log file info
  if (!is.null(log_file_path)) {
    msg <- sprintf(
      "\nFor more info about update db, plese check %s for detail.\n",
      log_file_path
    )
    rlang::inform(msg)
  }

  return(invisible(NULL))
}
# Method definition for s4 generic
#' @describeIn update_db update tables in a database of gta_db class
#' @export
setMethod(
  "update_db",
  signature(stock_db = "gta_db"),
  function(stock_db, data_source, retry_log,
             log_file_prefix, log_dir, ...) {
    update_db.gta_db(
      stock_db, data_source, retry_log,
      log_file_prefix, log_dir
    )
  }
)

# Import a raw data file into table in stock database
# Method definition for s3 generic
# @describeIn import_table import raw data into table in a database of gta_db class
#' @export
import_table.gta_db <- function(stock_db,
                                input_file,
                                input_type = c("csv", "txt"),
                                input_dir = NULL,
                                start_index = 2L,
                                target_table = NULL,
                                ignore_problems = TRUE,
                                log_dir = "./log") {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_not_null(input_file)
  assertive::assert_is_integer(start_index)
  assertive::assert_all_are_greater_than_or_equal_to(start_index, 1)
  assertive::assert_is_logical(ignore_problems)

  # get data object name
  object_name <- basename(input_file)
  object_name <- stringr::str_split(object_name, "\\.",
    simplify = TRUE
  )[1, 1]

  success <- TRUE

  # read raw file for importing
  raw_data_frame <- read_import_file(input_file,
    input_type = input_type,
    input_dir = input_dir,
    start_index = start_index,
    ignore_problems = ignore_problems,
    log_dir = log_dir
  )
  if (is.null(raw_data_frame)) success <- FALSE


  # transfer raw data dataframe to table in stock database
  if (success) {
    if (is.null(target_table)) {
      target_table <- object_name
    }

    result <- tryCatch({
      if (DBI::dbExistsTable(stock_db$connection, name = target_table)) {
        # DBI::dbWithTransaction(stock_db$connection, {
        # Clear old data in existed table
        sql_cmd <- sprintf("truncate table %s", target_table)
        DBI::dbExecute(stock_db$connection, sql_cmd)

        # Append new data into empty table
        DBI::dbWriteTable(stock_db$connection,
          name = target_table,
          value = raw_data_frame,
          append = TRUE
        )
        # })
      } else {
        # Create a new table
        DBI::dbWriteTable(stock_db$connection,
          name = target_table,
          value = raw_data_frame
        )
      }
    }, error = function(e) e)
    if (inherits(result, "error")) {
      error_msg <- conditionMessage(result)
      success <- FALSE
    } else {
      success <- result
    }

    if (success) {
      msg <- sprintf("import %s successfully", input_file)
      rlang::inform(msg)
    } else {
      msg <- sprintf("fail to import %s, error: %s", input_file, error_msg)
      rlang::warn(msg)
    }

  }

  return(invisible(success))
}

# Method definition for s4 generic
#' @describeIn import_table import raw data into table in a database of gta_db class
#' @export
setMethod(
  "import_table",
  signature(stock_db = "gta_db"),
  function(stock_db, input_file, input_type,
             input_dir, start_index,
             target_table, ignore_problems,
             log_dir,
             ...) {
    import_table.gta_db(
      stock_db, input_file, input_type,
      input_dir, start_index,
      target_table, ignore_problems,
      log_dir
    )
  }
)

# Process importing files in stock database
# Method definition for s3/s4 generic
# @describeIn process_files process importing files in a database of gta_db class
#' @export
process_files.gta_db <- function(stock_db,
                                 data_source = get_datasource(stock_db),
                                 retry_log = NULL,
                                 log_file_prefix = "update_db_log",
                                 log_dir = "./log") {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_not_null(data_source)
  assertive::assert_is_data.frame(data_source)
  assertive::assert_is_character(log_file_prefix)
  assertive::assert_is_character(log_dir)


  # filter datasource files to be process
  data_source_process <- data_source %>%
    dplyr::filter(!is.na(process)) %>%
    dplyr::left_join(data_source,
      by = c("process_source" = "input_file"),
      suffix = c("", ".source")
    )

  # get target database info
  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname
  assertive::assert_is_not_null(target_database)

  # setup log info params
  log_file_prefix <- sprintf(
    "%s_%s",
    log_file_prefix,
    target_database
  )
  log_file_path <- NULL
  ds_log <- data_source_process %>%
    dplyr::select(input_file = input_file,
                  source_file = process_source,
                  process) %>%
    dplyr::mutate(success = FALSE)

  # collect failed tables from log file
  failed_files <- NULL
  finished_files <- NULL
  if (!is.null(retry_log)) {
    ds_retry_log <- read_log(retry_log, log_dir = log_dir)

    if (!is.null(ds_retry_log)) {

      # find out failed files
      ds_faild_files <- ds_retry_log %>%
        dplyr::filter(success == FALSE)
      if (nrow(ds_faild_files) > 0) {
        failed_files <- ds_faild_files$input_file
      }

      # find out finished files
      ds_finished_files <- ds_retry_log %>%
        dplyr::filter(success == TRUE)
      if (nrow(ds_finished_files) > 0) {
        finished_files <- ds_finished_files$input_file
      }
    } else {
      msg <- sprintf(
        "Since there is no a retry log(%s), \n all input files will be processed.",
        retry_log
      )
      rlang::warn(msg)
    }
  }

  # process importing files from data source
  ds_data_source <- data_source_process
  for (i in seq_len(nrow(ds_data_source))) {
    data_source_info <- ds_data_source[i, ]

    # process for all input_files or input_files recording failed in retry log
    if (is.null(failed_files) || data_source_info$input_file %in% failed_files) {
      # processing for all input_files or input_files not recording successful in retry log
      if (is.null(finished_files) || !(data_source_info$input_file %in% finished_files)) {
        msg <- sprintf(
          "\nProcess %s into %s by %s...\n",
          data_source_info$process_source,
          data_source_info$input_file,
          data_source_info$process
        )
        rlang:::inform(msg)

        # build process function
        process_fun <- NULL
        if (!is.na(data_source_info$process)) {
          process <- data_source_info$process
          switch(process,
            "TTM" = {
              process_fun <- purrr::partial(ttm_financial_report,
                date_index_field = "Accper",
                key_fields = c("Stkcd", "Typrep"),
                parallel = TRUE
              )
            }
          )
        }

        # convert file through process_fun
        result <- convert_import_file(
          input_file = data_source_info$process_source,
          input_type = data_source_info$input_type.source,
          input_dir = data_source_info$input_dir.source,
          start_index = as.integer(data_source_info$start_index.source),
          ignore_problems = TRUE,
          log_dir = log_dir,
          process_fun = process_fun,
          output_file = data_source_info$input_file,
          output_type = data_source_info$input_type,
          output_dir = data_source_info$input_dir
        )

        ds_log$success[i] <- result

        if (!result) {
          msg <- sprintf(
            "Fail to process %s into %s by %s !!\n",
            data_source_info$process_source,
            data_source_info$input_file,
            data_source_info$process
          )

          rlang:::inform(msg)
        }
      } else {
        ds_log$success[i] <- TRUE
      }
    } else {
      ds_log$success[i] <- TRUE
    }

    # write log for update operation
    log_file_path <- save_log(ds_log,
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    )
  }

  # Notify user log file info
  if (!is.null(log_file_path)) {
    msg <- sprintf(
      "\nFor more info about process files, plese check %s for detail.\n",
      log_file_path
    )
    rlang::inform(msg)
  }

  return(invisible(NULL))

}

# Method definition for s4 generic
#' @describeIn process_files process importing files in a database of gta_db class
#' @export
setMethod(
  "process_files",
  signature(stock_db = "gta_db"),
  function(stock_db, data_source, retry_log,
             log_file_prefix, log_dir, ...) {
    process_files.gta_db(
      stock_db, data_source, retry_log,
      log_file_prefix, log_dir
    )
  }
)
