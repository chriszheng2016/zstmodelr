# Utility functions - logfile

#' Utility functions of log file
#'
#' Utility functions to handle log file, which contains log info of operating
#' in stock database.
#'
#' The log file is saved and read as "csv" file.
#'
#'
#' @name utils_logfile
NULL


# Save log info into log file.
#' @param ds_log_info  A dataframe of log info to save.
#' @param log_file_prefix  A prefix of log file, which define log file as
#'   in format of "log_file_prefix(current).csv"
#' @param log_dir  A character of log dir where log file is located.
#'   Default "./".
#' @return \strong{save_log()}: full path of save log file.
#' @describeIn utils_logfile save log info into log file.
#' @export
save_log <- function(ds_log_info,
                     log_file_prefix,
                     log_dir = "./log") {

  # validate params
  assertive::assert_is_data.frame(ds_log_info)
  assertive::assert_is_character(log_file_prefix)

  # make sure log dir exist
  if (!is.null(log_dir)) {
    # create new log dir
    if (!file.exists(log_dir)) {
      dir.create(log_dir)
    }
  } else {
    # use current dir if log_dir is null
    log_dir <- "./"
  }

  # build path of log file as "log_file_prefix(current).csv"

  log_file_path <- sprintf(
    "%s/%s(current).csv",
    log_dir,
    log_file_prefix
  )

  # Back up old log file by rename it as "log_file_prefix(YYYY-MM-DD).csv"
  if (file.exists(log_file_path)) {
    file_info <- file.info(log_file_path)
    last_modified_time <- file_info$mtime

    # Only backup old log file created before Current Date.
    if (as.Date(last_modified_time) < Sys.Date()) {
      backup_file <- sprintf(
        "%s/%s(%s).csv",
        log_dir,
        log_file_prefix,
        format(last_modified_time, "%Y-%m-%d")
      )
      file.copy(log_file_path,
        to = backup_file,
        copy.date = TRUE,
        overwrite = TRUE
      )
    }
  }

  # write a new log file
  readr::write_excel_csv(ds_log_info, file = log_file_path)

  # return path of log file
  return(log_file_path)
}

# Read log info from log file.
#' @param log_file_name  A character name of log file to read.
#' @return \strong{read_log}: A dataframe of log info if succeed, otherwise NULL.
#' @describeIn utils_logfile read log info from log file.
#' @export
read_log <- function(log_file_name,
                     log_dir = "./log") {

  # validate params
  assertive::assert_is_character(log_file_name)

  if (is.null(log_dir)) {
    # use current dir if log_dir is null
    log_dir <- "./"
  }

  ds_log_info <- NULL
  log_log_file_path <- file.path(log_dir, log_file_name)
  if (file.exists(log_log_file_path)) {
    file_ext <- tools::file_ext(log_log_file_path)
    if (file_ext == "csv") {
      suppressMessages(
        ds_log_info <- readr::read_csv(log_log_file_path)
      )
    }
  }

  return(ds_log_info)
}
