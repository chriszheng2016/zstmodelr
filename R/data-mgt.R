# @include stock-db.R
#  Problems: including stock-db.R will lead to not to produce get_datasource.rd.
#  it seems to be ok if without including stock-db.R

# Generic functions for data mangement operation ------------------------------

#' Get data source info for importing raw data
#'
#' Generic function to get data source info for importing raw data into stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family data management
#'
#' @return A dataframe of info about importing raw data if succeed,
#'   otherwise NULL.
#'
#' @export
# S3 generic definition
# get_datasource <- function(stock_db, ...){
#   UseMethod("get_datasource")
# }
# S4 generic definition
setGeneric(
  name = "get_datasource",
  signature = c("stock_db"),
  def = get_datasource <- function(stock_db, ...) {
    standardGeneric("get_datasource")
  }
)

#' Clear data in all tables in stock database
#'
#' Generic function to Clear existed data in all tables in stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family data management
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
# S3 generic definition
# clear_tables <- function(stock_db, ...){
#   UseMethod("clear_tables")
# }
# S4 generic definition
setGeneric(
  name = "clear_tables",
  signature = c("stock_db"),
  def = clear_tables <- function(stock_db, ...) {
    standardGeneric("clear_tables")
  }
)


#' Update tables in stock database with new data
#'
#' Generic function to update tables in stock database by importing new data
#'
#' By combining information from data_source and re-try log file , it update all
#' tables from raw data of datasource in following steps:
#'
#' \enumerate{
#'    \item Build target tables info from all tables in datasource or some
#'    tables with recording failure in retry log file;
#'    \item Update target tables by importing data from datasource;
#'    \item Save update log, etc.
#' }
#'
#' Update log is saved in log dir, like "update_log_XXXX(current).csv"
#'
#'
#' @param stock_db  A stock database object to operate.
#' @param data_source  Data source info produced by get_datasource(stock_db)
#' @param retry_log Log file for re-importing tables with recording failure
#'   in log file. If NULL, it will update all tables in data_source, otherwise
#'   only update on these tables with recording failure in log file.
#'   By default NULL.
#' @param log_file_prefix  A character of log file prefix to name log file.
#'   Log file is named as format of "log_file_prefix_XXXX_(current).csv"
#'   Default is "update_db_log".
#' @param log_dir Path to save updating log file. NULL means to use "./" as
#'   log dir. Default "./log".
#' @param ... Extra arguments to be passed to methods.
#'
#' @family data management
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
# S3 generic definition
# update_db <- function(stock_db,
#                          ...) {
#   UseMethod("update_db")
# }

# S4 generic definition
setGeneric(
  name = "update_db",
  signature = c("stock_db"),
  def = update_db <- function(stock_db,
                              data_source = get_datasource(stock_db),
                              retry_log = NULL,
                              log_file_prefix = "update_db_log",
                              log_dir = "./log",
                              ...) {
    standardGeneric("update_db")
  }
)

#' Import a raw data file into table in stock database
#'
#' Generic function to import a raw data into target table in stock database.
#'
#' Import process include two phrases:
#' \enumerate{
#'    \item Convert raw data into R data type.
#'    \item Transfer R data into table in database.
#' }
#'
#' Any problem in converting raw data will be logged in log file
#'  in log dir, e.g. "input_file(YYYY-MM-DD).csv"
#'
#' @param stock_db  A stock database object to operate.
#' @param input_file  A name or a path of input data file.
#' @param input_type  Format of input file, e.g. "txt", "csv".
#' @param input_dir Working dir of input file, if NULL, use dir of input file as
#'   working dir, by default NULL,
#' @param start_index Start index of first line of actual records in data file,
#'   by default 2L, which means first line is header and actual data starts from
#'   second lines.
#' @param target_table  Name of target table in stock db, if NULL, use basename
#'   input_file as target table ,  by default NULL,
#' @param ignore_problems Whether to ignore problems when covert data,
#'   if TRUE, continue to import data but log problems into log file,
#'   otherwise abort importing process.  By default TRUE.
#' @param log_dir   Path of log dir for saving problem log file,
#'   by default"./log", if the log path doesn't existed, it  will be created.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family data management
#'
#' @return TRUE if success, else FALSE.
#'
#' @export
# S3 generic definition
# import_table <- function(stock_db,
#                          input_file,
#                          input_type = c("csv", "txt"),
#                          input_dir = NULL,
#                          start_index = 2L
#                          target_table = NULL,
#                          ignore_problems = TRUE,
#                          log_dir = "./log",
#                          ...) {
#   UseMethod("import_table")
# }


# S4 generic definition
setGeneric(
  name = "import_table",
  signature = c("stock_db"),
  def = import_table <- function(stock_db,
                                 input_file,
                                 input_type = c("csv", "txt"),
                                 input_dir = NULL,
                                 start_index = 2L,
                                 target_table = NULL,
                                 ignore_problems = TRUE,
                                 log_dir = "./log",
                                 ...) {
    standardGeneric("import_table")
  }
)

#' Process importing files in stock database
#'
#' Generic function to process importing files in stock database.
#'
#' By combining information from data_source and re-try log file , it process
#' input_files of datasource in following steps:
#'
#' \enumerate{
#'    \item Build input_files info from all input_files marked with process
#'    in datasource or some input_files with recording failure in retry log file;
#'    \item Read input_files into original dataframe, process original
#'    dataframe by using process_fun specified by datasource, and save
#'    processed dataframe into files;
#'    \item Save update log, etc.
#' }
#'
#' Process log is saved in log dir, like "process_files_log_XXXX(current).csv"
#'
#'
#' @param stock_db  A stock database object to operate.
#' @param data_source  Data source info produced by get_datasource(stock_db)
#' @param retry_log Log file for re-processing tables with recording failure
#'   in log file. If NULL, it will process all input_files in data_source, otherwise
#'   only process on these input_files with recording failure in log file.
#'   By default NULL.
#' @param log_file_prefix  A character of log file prefix to name log file.
#'   Log file is named as format of "log_file_prefix_XXXX_(current).csv"
#'   Default is "update_db_log".
#' @param log_dir Path to save process log file. NULL means to use "./"
#'   as log dir. Default "./log".
#' @param ... Extra arguments to be passed to methods.
#'
#' @family data management
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
# S3 generic definition
# process_files <- function(stock_db,
#                          ...) {
#   UseMethod("process_files")
# }
# S4 generic definition
setGeneric(
  name = "process_files",
  signature = c("stock_db"),
  def = process_files <- function(stock_db,
                                  data_source = get_datasource(stock_db),
                                  retry_log = NULL,
                                  log_file_prefix = "process_files_log",
                                  log_dir = "./log",
                                  ...) {
    standardGeneric("process_files")
  }
)


# Non-generic functions for stock db operation ---------------------------------

#' Read a raw file for importing to stock db
#'
#' Read a raw file into a dataframe for processing or importing.
#'
#' @param input_file  A name or a path of input data file, which could use
#'   regular expression to read multiple files into a combined dataframe, e.g.
#'   "test[0-9]*.txt" will read files like test.txt, test1.txt,...,
#'   test9.txt..., etc., the files ware queued in ascending alphabet order.
#' @param input_type  A character of input file format, e.g. "txt", "csv".
#' @param input_dir   A path of working dir of input file, if NULL, use dir of
#'   input file as working dir, default NULL.
#' @param start_index A integer of start index of first line of actual records
#'   in data file, by default 2L, which means first line is header and actual
#'   data starts from second line.
#' @param ignore_problems A logic flag to determine whether to ignore problems
#'   when covert data, if TRUE, continue to import data but log problems into
#'   log file, otherwise abort reading process.  By default TRUE.
#' @param log_dir   Path of log dir for saving problem log file, by
#'   default"./log", if the log path doesn't existed, it  will be created.
#'
#' @family data management
#'
#' @return A dataframe of raw data for importing if succeed, otherwise NULL.
#'
#' @export
read_import_file <- function(input_file,
                             input_type = c("csv", "txt"),
                             input_dir = NULL,
                             start_index = 2L,
                             ignore_problems = TRUE,
                             log_dir = "./log") {
  # Define method to compute single file
  .read_single_file <- function(input_file,
                                input_type,
                                input_dir,
                                start_index,
                                ignore_problems,
                                log_dir,
                                file_encoding = "UTF-8",
                                col_names = TRUE,
                                col_types = NULL) {
    # validate params

    # get full path of input file
    if (!is.null(input_dir)) {
      input_file_path <- paste0(input_dir, "/", basename(input_file))
    } else {
      input_file_path <- normalizePath(input_file, winslash = "/")
    }

    # get data file name
    file_name <- basename(input_file_path)
    file_name <- tools::file_path_sans_ext(file_name)


    # read raw data into dataframe
    import_data_frame <- NULL
    suppressMessages({
      skip <- start_index - 1L

      import_data_frame <- switch(input_type,
        txt = {
          readr::read_tsv(
            input_file_path,
            locale = readr::locale(encoding = file_encoding),
            col_names = col_names,
            col_types = col_types,
            skip = skip
          )
        },
        csv = {
          readr::read_csv(
            input_file_path,
            locale = readr::locale(encoding = file_encoding),
            col_names = col_names,
            col_types = col_types,
            skip = skip
          )
        }
      )
    })

    # log problems in reading data
    problems_data <- readr::problems(import_data_frame)
    if ((NROW(import_data_frame) == 0) || (NROW(problems_data) != 0)) {
      if (is.null(log_dir)) {
        problems_file <- sprintf(
          "%s/%s(%s).csv",
          dirname(input_file_path),
          file_name,
          format(Sys.Date(), "%Y-%m-%d")
        )
      } else {
        if (!file.exists(log_dir)) {
          dir.create(log_dir)
        }

        problems_file <- sprintf(
          "%s/%s(%s).csv",
          log_dir,
          file_name,
          format(Sys.Date(), "%Y-%m-%d")
        )
      }

      readr::write_excel_csv(problems_data, problems_file)

      msg <- sprintf(
        "Converting problems in %s, plese check %s for detail.",
        input_file_path,
        problems_file
      )

      if (ignore_problems) {
        rlang::warn(msg)
      } else {
        rlang::abort(msg)
      }
    }

    # return NULL if no data was read
    if (NROW(import_data_frame) == 0) {
      import_data_frame <- NULL
    }

    return(import_data_frame)
  }

  # -- Main Function --
  # validate params
  #   assertive::assert_is_not_null(input_file)
  assertive::assert_is_integer(start_index)
  assertive::assert_all_are_greater_than_or_equal_to(start_index, 1L)
  assertive::assert_is_logical(ignore_problems)

  # Get traget files for importing
  if (is.null(input_dir)) {
    input_dir <- dirname(input_file)
  } else {
    input_dir <- file.path(input_dir, dirname(input_file))
  }
  input_dir <- normalizePath(input_dir, winslash = "/")
  target_files <- list.files(input_dir,
    pattern = stringr::regex(
      basename(input_file),
      ignore_case = TRUE,
      dotall = TRUE
    )
  )
  target_files <- stringr::str_sort(target_files, decreasing = FALSE)

  # Process multiple target files
  ds_import_data <- NULL
  if (length(target_files) > 0) {

    # use first file configration(col_name, col_type) as configuration for all files
    first_target_file <- target_files[1]

    # get full path of input file
    if (!is.null(input_dir)) {
      input_file_path <-
        paste0(input_dir, "/", basename(first_target_file))
    } else {
      input_file_path <- normalizePath(first_target_file, winslash = "/")
    }

    # guess file encoding
    file_encoding_info <- readr::guess_encoding(input_file_path)
    if (NROW(file_encoding_info) > 0) {
      file_encoding <- file_encoding_info$encoding[1]
    } else {
      file_encoding <- "GB18030"
    }

    input_type <- match.arg(input_type)

    # get colnames of import data
    suppressMessages(
      import_data_header <- switch(input_type,
        txt = {
          readr::read_tsv(input_file_path,
            locale = readr::locale(encoding = file_encoding),
            n_max = 0
          )
        },
        csv = {
          readr::read_csv(input_file_path,
            locale = readr::locale(encoding = file_encoding),
            n_max = 0
          )
        }
      )
    )
    col_names <- names(import_data_header)
    if (nrow(import_data_header) != 0) {
      msg <- sprintf("Error in getting header from %s", input_file_path)
      rlang::abort(msg)
    }

    # get column types
    suppressMessages({
      skip <- start_index - 1L
      col_spec <- switch(input_type,
        txt = {
          readr::spec_tsv(
            input_file_path,
            col_names = col_names,
            locale = readr::locale(encoding = file_encoding),
            skip = skip,
            guess_max = getOption("zstmodelr.data_mgt.guess_max", 50000)
          )
        },
        csv = {
          readr::spec_csv(
            input_file_path,
            col_names = col_names,
            locale = readr::locale(encoding = file_encoding),
            skip = skip,
            guess_max = getOption("zstmodelr.data_mgt.guess_max", 50000)
          )
        }
      )
    })
    col_types <- col_spec

    # set configuration for target files
    ds_target_files <- tibble::tibble(
      input_file = target_files,
      input_dir = input_dir,
      file_encoding = file_encoding,
      col_names = list(col_names),
      col_types = list(col_types)
    )

    # read target files
    ds_target_files <- ds_target_files %>%
      dplyr::mutate(
        import_data = purrr::pmap(
          .,
          .f = .read_single_file,
          input_type = input_type,
          start_index = start_index,
          ignore_problems = ignore_problems,
          log_dir = log_dir
        )
      )

    # consolidate imported data into a data.frame
    ds_import_data <- purrr::map_dfr(ds_target_files$import_data, .f = ~.)
  }

  ds_import_data
}

#' Write a raw file for importing to stock db
#'
#' Write a dataframe as import file. If output file existed, original file will
#' be backed up.
#'
#' @param ds_output  A name or a path of input data file.
#' @param output_file  A name or a path of input data file.
#' @param output_type  A character of output file format, e.g. "txt", "csv".
#' @param output_dir A path of working dir of output file, if NULL, use dir
#'   of output file as working dir, by default NULL,
#'
#' @family data management
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
write_import_file <- function(ds_output,
                              output_file,
                              output_type = c("csv", "txt"),
                              output_dir = NULL) {

  # validate params
  assertive::assert_is_data.frame(ds_output)
  assertive::assert_is_not_null(output_file)

  # get full path of output file
  if (!is.null(output_dir)) {
    output_file_path <- paste0(output_dir, "/", basename(output_file))
  } else {
    output_file_path <- normalizePath(output_file,
      winslash = "/",
      mustWork = FALSE
    )
  }

  # get data file name
  file_name <- basename(output_file_path)
  file_name <- tools::file_path_sans_ext(file_name)
  file_ext <- tools::file_ext(output_file_path)

  # backup exisited file
  if (file.exists(output_file_path)) {
    file_info <- file.info(output_file_path)
    last_modified_time <- file_info$mtime
    backup_file_path <- sprintf(
      "%s/%s(%s).%s",
      dirname(output_file_path),
      file_name,
      format(last_modified_time, "%Y-%m-%d"),
      file_ext
    )
    file.copy(output_file_path,
      to = backup_file_path,
      copy.date = TRUE,
      overwrite = TRUE
    )
  }

  # write file by type
  output_type <- match.arg(output_type)
  suppressMessages(
    switch(output_type,
      txt = {
        readr::write_tsv(ds_output, path = output_file_path)
      },
      csv = {
        readr::write_csv(ds_output, path = output_file_path)
      }
    )
  )

  return(invisible(NULL))
}


#' Convert a raw importing file through process_fun
#'
#' convert_import_file is the working horse backing process_files.
#'
#' Convert a raw importing file into a processed importing file
#' though three steps:
#' \enumerate{
#'    \item Read import file into original dataframe by using read_import_file;
#'    \item Process original dataframe into processed dataframe by
#'    using process_fun, like ttm_financial_report;
#'    \item Write processed dataframe into file by using write_import_file.
#' }
#'
#' @inheritParams read_import_file
#'
#' @param process_fun A function to process input file into output file.
#' @param ...     Params to process_fun.
#'
#' @inheritParams write_import_file
#'
#' @family data management
#'
#' @return TRUE if success, else FALSE.
#'
#' @export
convert_import_file <- function(input_file,
                                input_type = c("csv", "txt"),
                                input_dir = NULL,
                                start_index = 2L,
                                ignore_problems = TRUE,
                                log_dir = "./log",
                                process_fun = NULL,
                                ...,
                                output_file = input_file,
                                output_type = input_type,
                                output_dir = input_dir) {

  # validate params
  if (!is.null(process_fun)) {
    assertive::assert_is_function(process_fun)
  }

  success <- TRUE

  # read original data
  result <- tryCatch(
    {
      read_import_file(
        input_file,
        input_type = input_type,
        input_dir = input_dir,
        start_index = start_index,
        ignore_problems = ignore_problems,
        log_dir = log_dir
      )
    },
    error = function(e) e
  )
  if (inherits(result, "error")) {
    error_msg <- conditionMessage(result)
    ds_origin_data <- NULL
  } else {
    ds_origin_data <- result
  }
  if (is.null(ds_origin_data)) success <- FALSE

  # process orgiginal data
  ds_processed_data <- NULL
  if (success) {
    result <- tryCatch(
      {
        if (!is.null(process_fun)) {
          process_fun(ds_origin_data, ...)
        } else {
          ds_origin_data
        }
      },
      error = function(e) e
    )
    if (inherits(result, "error")) {
      error_msg <- conditionMessage(result)
      success <- FALSE
    } else {
      ds_processed_data <- result
    }
  }

  # write process data
  if (success) {
    result <- tryCatch(
      {
        write_import_file(ds_processed_data,
          output_file = output_file,
          output_type = output_type,
          output_dir = output_dir
        )
      },
      error = function(e) e
    )
    if (inherits(result, "error")) {
      error_msg <- conditionMessage(result)
      success <- FALSE
    }
  }

  if (success) {
    msg <- sprintf(
      "\nConvert %s to %s successfully",
      input_file,
      output_file
    )
    rlang::inform(msg)
  } else {
    msg <- sprintf(
      "\nFail to convert %s to %s, error: %s",
      input_file,
      output_file,
      error_msg
    )
    rlang::warn(msg)
  }

  return(invisible(success))
}


#' Convert financial report to TTM format
#'
#' Convert financial report from normal format(quarterly accumulated format)
#' to TTM format(Trail Twelve Months).
#'
#' @param ds_financial_report   A data.frame of financial report to convert.
#' @param date_index_field  A character name of date index field of dataframe of
#'   financial report, default 'Accper'.
#' @param key_fields    A character vector of key fields, which identify unique
#'   observation in each date, default c("Stkcd", "Typrep").
#' @param field_suffix A character suffix for result fields, default is "ttm".
#' @param parallel   A logic to determine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#'
#' @family data management
#'
#' @return A dataframe of financial report of TTM format if succeed,
#'   otherwise NULL.
#'
#' @export
ttm_financial_report <- function(ds_financial_report,
                                 date_index_field = "Accper",
                                 key_fields = c("Stkcd", "Typrep"),
                                 field_suffix = "ttm",
                                 parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # function to trial data in a group with unique value of key_fields
  .trail_fun <- function(ds_financial_report,
                         date_index_field,
                         key_fields,
                         field_suffix = "ttm",
                         ...) {

    # predicate period of dates
    origin_dates <- ds_financial_report[[date_index_field]]

    period <- guess_dates_period(origin_dates, regular = TRUE)

    # trail data by periodic dates
    if (period != "unknown") {
      # trail regular periodic series

      # split original dataset into datasets of value fields
      # and non_value fields

      all_fields <- names(ds_financial_report)
      # value_fields include numeric fields, na_fields, but exclude
      # key_fields and date_index_field
      pattern_retain_fields <- stringr::str_c(
        stringr::str_c(key_fields, collapse = "|"),
        date_index_field,
        sep = "|"
      )
      pattern_non_retain_fields <- sprintf("[^%s]", pattern_retain_fields)

      numeric_fields <- expect_type_fields(ds_financial_report,
        expect_type = "numeric"
      )
      na_fields <- expect_type_fields(ds_financial_report,
        expect_type = "NA"
      )
      value_fields <- c(numeric_fields, na_fields)
      value_fields <- stringr::str_subset(value_fields,
        pattern = pattern_non_retain_fields
      )

      ds_origin_value <- ds_financial_report %>%
        dplyr::select(!!value_fields)

      # non_value_fields include other fields except value fields
      non_value_fields <- all_fields[!(all_fields %in% value_fields)]
      ds_origin_non_value <- ds_financial_report %>%
        dplyr::select(!!non_value_fields)

      # trail dataset of value_fields
      ds_trail_value <- trail_periodic_series(origin_dates,
        data_series = ds_origin_value,
        period = period,
        trailing_month = 12L,
        agg_fun = sum,
        na.rm = TRUE
      )

      # change value fields with suffix (*_ttm)
      if (!is.null(field_suffix) && (field_suffix != "")) {
        origin_value_fields <- names(ds_trail_value)
        new_value_fields <- paste0(origin_value_fields, "_", field_suffix)
        names(ds_trail_value) <- new_value_fields
      }

      # combine dataset of non_value fields and dataset of trail_value fields
      if (NROW(ds_trail_value) == NROW(ds_origin_non_value)) {
        ds_trail <- ds_origin_non_value %>%
          dplyr::bind_cols(ds_trail_value)
      } else {
        msg <- sprintf(
          "Fail to trail value of fields, because length of trail result(%d)
          is deferent from length of original data(%d)",
          NROW(ds_trail_value),
          NROW(ds_origin_non_value)
        )
        rlang::abort(msg)
      }
    } else {
      msg <- sprintf(
        "Can't trail series(%s) with irregular periodic date(%s).",
        date_index_field
      )
      rlang::abort(msg)
    }
  }

  # main body of function

  # validate params
  assertive::assert_is_data.frame(ds_financial_report)
  assertive::assert_is_character(date_index_field)
  assertive::assert_is_vector(key_fields)
  assertive::assert_is_logical(parallel)


  # get quarter dataset
  date_expr <- rlang::parse_expr(date_index_field)
  ds_quarter_financial_report <- ds_financial_report %>%
    dplyr::filter(lubridate::month(!!date_expr) %in% c(3, 6, 9, 12)) %>%
    tibble::as_tibble()

  # make regular quarter dataset
  ds_quarter_financial_report <- ts_asfreq(ds_quarter_financial_report,
    freq_rule = "quarter",
    fillna_method = "nfill",
    date_index_field = date_index_field,
    key_fields = key_fields,
    parallel = parallel
  )


  # compute result
  ds_trail_financial_report <- compute_indicator(ds_quarter_financial_report,
    compute_fun = .trail_fun,
    date_index_field = date_index_field,
    key_fields = key_fields,
    field_suffix = field_suffix,
    parallel = parallel
  )

  return(ds_trail_financial_report)
}
