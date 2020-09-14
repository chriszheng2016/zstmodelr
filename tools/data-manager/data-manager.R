# Tools for updating database periodically

library(zstmodelr)
options(zstmodelr.data_mgt.guess_max = 300000)
# enable parallel process
enable_parallel()

# Process stock database
process_stock_db <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                             retry_error = FALSE,
                             ...) {

  # connect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)

  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname

  # whether to retry input_files with errors
  retry_log_file <- NULL
  log_file_prefix <- "process_files_log"
  if (!retry_error) {
    # update all tables
    msg <- sprintf("\nProcess all input_files in stock db...\n")
    message(msg)
  } else {
    # update only failed input_files recored in the log file
    retry_log_file <- sprintf(
      "%s_%s(current).csv",
      log_file_prefix,
      target_database
    )
    msg <- sprintf(
      "\nRetry to process input files with errors logged in %s...\n",
      retry_log_file
    )
    message(msg)
  }

  # process input_files for importing
  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")
  process_files(stock_db,
    retry_log = retry_log_file,
    log_dir = log_dir
  )

  # close stock db
  close_stock_db(stock_db)
}

# Update stock database
update_stock_db <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                            retry_error = FALSE,
                            ...) {

  # connect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)

  db_info <- DBI::dbGetInfo(stock_db$connection)
  target_database <- db_info$dbname

  # whether to retry tables with errors
  retry_log_file <- NULL
  log_file_prefix <- "update_db_log"
  if (!retry_error) {
    # update all tables
    msg <- sprintf("\nUpdate all tables in stock db...\n")
    message(msg)
  } else {
    # update only failed tables recored in the log file
    retry_log_file <- sprintf(
      "%s_%s(current).csv",
      log_file_prefix,
      target_database
    )
    msg <- sprintf(
      "\nRetry to update tables with errors logged in %s...\n",
      retry_log_file
    )
    message(msg)
  }

  # update tables
  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")
  update_db(stock_db,
    retry_log = retry_log_file,
    log_dir = log_dir
  )

  # close stock db
  close_stock_db(stock_db)
}

# Clear stock database
clear_stock_db <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                           force = FALSE, ...) {
  continue <- FALSE

  # force to execute without confirmation
  if (force) continue <- TRUE

  # ask user's confirmation to execute
  if (!continue) {
    answer <- readline("Do you want to clear all tables(yes or no)!!? ")
    if (substr(answer, 1, 1) == "y") {
      continue <- TRUE
    }
  }

  # carry out excution
  if (continue) {

    # connect to target stock db
    dsn <- match.arg(dsn)
    stock_db <- stock_db(gta_db, dsn)
    open_stock_db(stock_db)

    msg <- sprintf("\nAll tables will be cleared...\n")
    message(msg)

    # clears all tables
    clear_tables(stock_db)

    # close stock db
    close_stock_db(stock_db)
  }
}

# Fetch action fun by name
.action_fun <- function(action = c("process", "update", "clear")) {
  action <- match.arg(action)
  action_fun <- switch(
    action,
    "process" = {
      rlang::quo(process_stock_db)
    },
    "update" = {
      rlang::quo(update_stock_db)
    },
    "clear" = {
      rlang::quo(clear_stock_db)
    }
  )

  action_fun
}

# Main function to conduct data management
data_manager <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                         action = c("process", "update", "clear"),
                         ...,
                         help = FALSE) {
  if (help) {
    help_usage()
  } else {
    dsn <- match.arg(dsn)
    action <- match.arg(action)
    action_fun <- rlang::eval_tidy(.action_fun(action))
    action_fun(dsn, ...)
  }
}

# Interactive UI of main function to conduct data management
data_manager_ui <- function(debug = FALSE) {
  # prompt use select action function
  action_fun <- interactive_call_(rlang::quo(.action_fun), quiet = TRUE)

  # run action interactively
  interactive_call_(action_fun, debug = debug)
}

help_usage <- function() {

  # usage of arguments
  argment_desc <- c("
  * dsn: name of data source of database.

  * action: action to perform.
    - process: process files for importing.
    - update: update tables in dabase by importing files.
    - clear: clear all tables in database.

  * ...: arguments to action function.
    - retry_error: use logged file to retry actions with error, valid for process and update.
    - force: clear all table without comfirmation, valid for clear.

  * help: diplay usage or not.

  ")

  # usage of examples
  examples <- c('
  # Update all tables by default
  data_manager(dsn = "GTA_SQLData", action = "update")

  # Update tables with logged errors
  data_manager(dsn = "GTA_SQLData", action = "update", retry_error = TRUE)

  # Process all input_files for importing by default
  data_manager(dsn = "GTA_SQLData", action = "process")

  # Process input_files with logged errors
  data_manager(dsn = "GTA_SQLData", action = "process", retry_error = TRUE)

  # Clear all tables by default
  data_manager(dsn = "GTA_SQLData", action = "clear")

  # Clear all tables forcefully
  data_manager(dsn = "GTA_SQLData", action = "clear", force = TRUE)
  ')

  help_fun(data_manager,
    argument_desc = argment_desc,
    examples = examples
  )
}



# Run data_manager in direct mode
# data_manager(help = TRUE)
#
# Run data_manager in interactive mode
# data_manager_ui()
