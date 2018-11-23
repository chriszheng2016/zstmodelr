#' @include data-mgt.R
#' @include stock-db-gta.R


# Generic functions implemetation by gta_db class ------------------------

# Get data source info for importing raw data
# Method definition for s3 generic
#' @describeIn get_datasource get data source for importing raw data into
#'   database of gta_db class
#' @export
get_datasource.gta_db <- function(stock_db) {

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # Get file table name mapping for referece
  gta_profile_name <- system.file(.GTA_RPROFILE_DIR,
    .GTA_PROFILE_FILE,
    package = .PACKAGE_NAME
  )
  if (gta_profile_name == "") {
    msg <- sprintf(
      "No file of % exisits in % for %",
      .GTA_PROFILE_FILE,
      .GTA_RPROFILE_DIR,
      .PACKAGE_NAME
    )
    stop(msg)
    success <- FALSE
  }

  # Get data source info from setting
  ds_datasource <- NULL
  if (success) {
    ds_datasource_files <- .get_db_datasource_files(gta_profile_name)

    # build specified result of datasource
    ds_datasource <- ds_datasource_files %>%
      dplyr::filter(is_valid == TRUE) %>%
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
#' @describeIn get_factors_info get factors info from a database of gta_db class
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

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # Get target database info
  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname
  assertive::assert_is_not_null(target_database)

  # Clear old data in all tables
  sql_cmd <- "EXEC sp_MSforeachtable \"truncate table ?\""
  DBI::dbExecute(stock_db$connection, sql_cmd)

  # Shrink database log
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
                             log_dir = "./log") {

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_not_null(data_source)
  assertive::assert_is_data.frame(data_source)

  success <- TRUE

  # Get target database info
  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname
  assertive::assert_is_not_null(target_database)


  # Build target tables info
  ds_data_source <- data_source
  ds_log <- data_source %>%
    dplyr::select(target_table, input_file) %>%
    dplyr::mutate(success = FALSE)

  # Collect failed tables from log file
  faild_tables <- NULL
  retry_log_path <- NULL
  if (!is.null(retry_log)) {
    if ((basename(retry_log) == retry_log)) {
      if (!is.null(log_dir)) {
        retry_log_path <- sprintf("%s/%s", log_dir, basename(retry_log))
      } else {
        retry_log_path <- sprintf("%s/%s", ".", basename(retry_log))
      }
    } else {
      retry_log_path <- retry_log
    }

    if (file.exists(retry_log_path)) {
      ds_retry_log <- readr::read_csv(retry_log_path)
      ds_faild_tables <- ds_retry_log %>%
        dplyr::filter(success == FALSE)
      if (nrow(ds_faild_tables) > 0) {
        faild_tables <- ds_faild_tables$target_table
      }
    } else {
      msg <- sprintf("there isn't retry log file:%s", retry_log)
      warning(msg)
    }
  }

  # Import data from data source
  need_clear_dblog <- FALSE
  for (i in seq_len(nrow(ds_data_source))) {
    data_source <- ds_data_source[i, ]

    # Conduct update for all tables or faied tables recording in retry log
    if (is.null(faild_tables) || data_source$target_table %in% faild_tables) {
      msg <- sprintf("Import data into %s ...\n", data_source$target_table)
      message(msg)

      success <- import_table.gta_db(stock_db,
        input_file = data_source$input_file,
        input_type = data_source$input_type,
        input_dir = data_source$input_dir,
        start_index = data_source$start_index,
        target_table = data_source$target_table,
        ignore_problems = TRUE,
        log_dir = log_dir
      )

      need_clear_dblog <- TRUE
      ds_log$success[i] <- success

      if (!success) {
        msg <- sprintf("fail to update %s !!\n", data_source$target_table)
        message(msg)
      }
    } else {
      ds_log$success[i] <- TRUE
    }
  }

  # Shrink database log if needed
  if (need_clear_dblog) {
    # sql_cmd <- sprintf("DBCC ShrinkDatabase(%s, 0)", target_database )
    sql_cmd <- sprintf(
      "DBCC ShrinkFile(%s_log, 1, TRUNCATEONLY)",
      target_database
    )
    DBI::dbExecute(stock_db$connection, sql_cmd)
  }


  # Write log for update operation
  if (!is.null(log_dir)) {
    if (!file.exists(log_dir)) {
      dir.create(log_dir)
    }

    log_file <- sprintf(
      "%s/update_log_%s(current).csv",
      log_dir,
      target_database
    )

    # Back up old log file by change its names
    # as "update_db_log(YYYY-MM-DD).csv"
    if (file.exists(log_file)) {
      file_info <- file.info(log_file)
      last_modified_time <- file_info$mtime
      backup_file <- sprintf(
        "%s/update_log_%s(%s).csv",
        log_dir,
        target_database,
        format(last_modified_time, "%Y-%m-%d")
      )
      file.copy(log_file,
        to = backup_file,
        copy.date = TRUE,
        overwrite = TRUE
      )
    }

    # Write a new log file
    readr::write_excel_csv(ds_log, log_file)

    msg <- sprintf(
      "For more info about update db, plese check %s for detail.",
      log_file
    )
    message(msg)
  }
}
# Method definition for s4 generic
#' @describeIn update_db update tables in a database of gta_db class
#' @export
setMethod(
  "update_db",
  signature(stock_db = "gta_db"),
  function(stock_db, data_source, retry_log, log_dir, ...) {
    update_db.gta_db(stock_db, data_source, retry_log, log_dir)
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

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_not_null(input_file)
  assertive::assert_is_a_number(start_index)
  assertive::assert_all_are_greater_than_or_equal_to(start_index, 1)
  assertive::assert_is_logical(ignore_problems)

  success <- TRUE

  # Get full path of input file
  if (!is.null(input_dir)) {
    input_file <- paste0(input_dir, "/", basename(input_file))
  } else {
    input_file <- normalizePath(input_file, winslash = "/")
  }

  # Get data object name
  object_name <- basename(input_file)
  object_name <- stringr::str_split(object_name, "\\.",
    simplify = TRUE
  )[1, 1]

  # Read data from raw file into dataframe
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

  # Read raw data into dataframe
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

  # Transfer raw data dataframe to table in stcok database
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
