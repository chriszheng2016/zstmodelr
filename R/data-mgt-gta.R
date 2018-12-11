#' @include data-mgt.R
#' @include stock-db-gta.R


# Generic functions implemetation by gta_db class ------------------------

# Get data source info for importing raw data
# Method definition for s3 generic
#' @describeIn get_datasource get data source for importing raw data into
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
      dplyr::mutate(input_dir = input_dir) %>%
      dplyr::select(
        target_table = target_table,
        input_file = input_file,
        input_type = input_type,
        input_dir = input_dir,
        start_index = start_index
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
#' @describeIn clear_tables clear data in all tables in a database of gta_db class
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
#' @describeIn update_db update tables in a database of gta_db class
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

  success <- TRUE

  # get target database info
  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname
  assertive::assert_is_not_null(target_database)

  # setup log info params
  log_file_prefix <- sprintf("%s_%s",
                             log_file_prefix,
                             target_database)
  log_file_path <- NULL
  ds_log <- data_source %>%
    dplyr::select(target_table, input_file) %>%
    dplyr::mutate(success = FALSE)

  # collect failed tables from log file
  faild_tables <- NULL
  finished_tables <- NULL
  if (!is.null(retry_log)) {

    ds_retry_log <- read_log(retry_log, log_dir = log_dir)

    if (!is.null(ds_retry_log)) {

      # find out failed tables
      ds_faild_tables <- ds_retry_log %>%
        dplyr::filter(success == FALSE)
      if (nrow(ds_faild_tables) > 0) {
        faild_tables <- ds_faild_tables$target_table
      }

      # find out finished tables
      ds_finished_tables <- ds_retry_log %>%
          dplyr::filter(success == TRUE)
        if (nrow(ds_finished_tables) > 0) {
          finished_tables <- ds_finished_tables$target_table
      }
    } else {
      msg <- sprintf("Since there is no a retry log(%s), \n all tables will be updated.",
                     retry_log)
      rlang::warn(msg)
    }
  }

  # import data from data source
  ds_data_source <- data_source
  need_clear_dblog <- FALSE
  for (i in seq_len(nrow(ds_data_source))) {
    data_source_info <- ds_data_source[i, ]

    # conduct update for all tables or tables recording failed in retry log
    if (is.null(faild_tables) || data_source_info$target_table %in% faild_tables) {
      # conduct update for all tables or tables not recording successful in retry log
      if (is.null(finished_tables) || !(data_source_info$target_table %in% finished_tables)) {

        msg <- sprintf("Import data into %s ...\n", data_source_info$target_table)
        rlang:::inform(msg)

        success <- import_table(stock_db,
                                input_file = data_source_info$input_file,
                                input_type = data_source_info$input_type,
                                input_dir = data_source_info$input_dir,
                                start_index = data_source_info$start_index,
                                target_table = data_source_info$target_table,
                                ignore_problems = TRUE,
                                log_dir = log_dir
        )

        need_clear_dblog <- TRUE
        ds_log$success[i] <- success

        if (!success) {
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
    log_file_path <- save_log(ds_log, log_file_prefix = log_file_prefix,
                              log_dir = log_dir)


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

}
# Method definition for s4 generic
#' @describeIn update_db update tables in a database of gta_db class
#' @export
setMethod(
  "update_db",
  signature(stock_db = "gta_db"),
  function(stock_db, data_source, retry_log,
           log_file_prefix, log_dir, ...) {
    update_db.gta_db(stock_db, data_source, retry_log,
                     log_file_prefix, log_dir)
  }
)

# Import a raw data file into table in stock database
# Method definition for s3 generic
#' @describeIn import_table import raw data into table in a database of gta_db class
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
  assertive::assert_is_a_number(start_index)
  assertive::assert_all_are_greater_than_or_equal_to(start_index, 1)
  assertive::assert_is_logical(ignore_problems)

  success <- TRUE

  # get full path of input file
  if (!is.null(input_dir)) {
    input_file <- paste0(input_dir, "/", basename(input_file))
  } else {
    input_file <- normalizePath(input_file, winslash = "/")
  }

  # get data object name
  object_name <- basename(input_file)
  object_name <- stringr::str_split(object_name, "\\.",
    simplify = TRUE
  )[1, 1]

  # read data from raw file into dataframe
  input_type <- match.arg(input_type)
  # Get colnames of rawdata header
  suppressMessages(
    raw_data_header <- switch(input_type,
      txt = {
        readr::read_tsv(input_file,
          locale = readr::locale(encoding = "CP936"),
          n_max = 0
        )
      },
      csv = {
        readr::read_csv(input_file,
          locale = readr::locale(encoding = "CP936"),
          n_max = 0
        )
      }
    )
  )
  raw_data_colnames <- names(raw_data_header)
  if (nrow(raw_data_header) != 0) {
    success <- FALSE

    msg <- sprintf("Errors in getting header from %s", input_file)
    stop(msg)
  }

  # read raw data into dataframe
  suppressMessages({
    skip <- start_index - 1L

    raw_data_frame <- switch(input_type,
      txt = {
        readr::read_tsv(input_file,
          locale = readr::locale(encoding = "CP936"),
          col_names = raw_data_colnames,
          skip = skip,
          guess_max = 50000
        )
      },
      csv = {
        readr::read_csv(input_file,
          locale = readr::locale(encoding = "CP936"),
          col_names = raw_data_colnames,
          skip = skip,
          guess_max = 50000
        )
      }
    )
  })
  problems_data <- readr::problems(raw_data_frame)
  if ((nrow(raw_data_frame) == 0) || (nrow(problems_data) != 0)) {
    if (is.null(log_dir)) {
      problems_file <- sprintf(
        "%s/%s(%s).csv",
        dirname(input_file),
        object_name,
        format(Sys.Date(), "%Y-%m-%d")
      )
    } else {
      if (!file.exists(log_dir)) {
        dir.create(log_dir)
      }

      problems_file <- sprintf(
        "%s/%s(%s).csv",
        log_dir,
        object_name,
        format(Sys.Date(), "%Y-%m-%d")
      )
    }

    readr::write_excel_csv(problems_data, problems_file)

    msg <- sprintf(
      "Converting problems in %s, plese check %s for detail.",
      input_file,
      problems_file
    )
    warning(msg)

    if (!ignore_problems) {
      success <- FALSE
    }
  }

  # transfer raw data dataframe to table in stcok database
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
    } else {
      msg <- sprintf("fail to import %s, error: %s", input_file, error_msg)
    }
    message(msg)
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
