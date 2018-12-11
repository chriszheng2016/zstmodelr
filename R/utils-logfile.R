# save log info into log file.
save_log <- function(ds_log_info,
                     log_file_prefix,
                     log_dir = "./log") {

  # validate params
  assertive::assert_is_data.frame(ds_log_info)
  assertive::assert_is_character(log_file_prefix)

  # make sure log dir exsit
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

  # write a new log file
  readr::write_excel_csv(ds_log_info, path = log_file_path)

  # return path of log file
  return(log_file_path)
}

# read log info from log file.
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

