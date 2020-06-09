# Tools for updating database periodically

library(zstmodelr)
options(zstmodelr.data_mgt.guess_max = 300000)
# enable parallel process
enable_parallel()

# process stock database
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

# Main function to conduct data management
data_manager <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                         action = c("process", "update", "clear"),
                         help = FALSE,
                         ...) {
  if (help) {
    help_usage()
  } else {
    dsn <- match.arg(dsn)
    action <- match.arg(action)
    switch(
      action,
      "process" = {
        process_stock_db(dsn, ...)
      },
      "update" = {
        update_stock_db(dsn, ...)
      },
      "clear" = {
        clear_stock_db(dsn, ...)
      }
    )
  }
}

# Interactive UI of main function to conduct data management
data_manager_ui <- function(fun = data_manager, debug = FALSE) {

  # Prompt use to choose value for a argument
  .prompt_arg_value <- function(arg, choices, fun = NULL) {
    # Use default value as candidate of argument value
    # code borrowed from match.arg()
    if (missing(fun) || is.null(fun)) {
      fun <- sys.function(sysP <- sys.parent())
    }
    if (missing(choices)) {
      formal.args <- formals(fun)
      choices <- eval(formal.args[[as.character(substitute(arg))]],
        envir = sys.frame(sysP <- sys.parent())
      )
      if (is.logical(choices)) {
        choices <- c(FALSE, TRUE)
      }
    }

    # Prompt user to choose a value for argument
    arg_name <- as.character(substitute(arg))
    if (length(choices) > 1) {
      cli::cli_rule(center = " * select value for {arg_name} * ")
      arg_value <- choices[utils::menu(choices)]
    } else {
      arg_value <- choices[1]
    }

    if (length(arg_value) > 0) {
      cli::cli_alert_success("selected {arg_name}: {.strong {arg_value}}.\n")
    } else {
      cli::cli_alert_warning("please see {.strong usage} carefully.")
      help_usage()
      rlang::abort("Abort without argument.\n")
    }

    # Return result as "arg = value"
    arg_value_str <- NULL
    if (is.character(arg_value) && (length(arg_value) > 0)) {
      arg_value_str <- glue::glue("{arg_name} = '{arg_value}'")
    } else {
      arg_value_str <- glue::glue("{arg_name} = {arg_value}")
    }

    arg_value_str
  }

  # Main function

  # Display function logo
  fun_name <- as.character(substitute(fun))
  cli::cli_rule(center = "{.emph {fun_name}}")

  # Choose target data source
  dsn_arg <- .prompt_arg_value(dsn, fun = fun)

  # Choose action for db
  action_arg <- .prompt_arg_value(action, fun = fun)

  # Choose other arguments for action
  select_action <-
    stringr::str_extract(action_arg, pattern = "(?<=').*?(?=')")
  switch(
    select_action,
    "process" = {
      other_args <-
        .prompt_arg_value(arg = retry_error, fun = process_stock_db)
    },
    "update" = {
      other_args <-
        .prompt_arg_value(arg = retry_error, fun = update_stock_db)
    },
    "clear" = {
      other_args <- .prompt_arg_value(arg = force, fun = clear_stock_db)
    }
  )

  # Perform action
  actions_template <-
    "{substitute(fun)}({dsn_arg}, {action_arg}, {other_args})"
  action <- rlang::parse_expr(glue::glue(actions_template))
  cat("Action might take very long time, do you want to continue?\n")
  cli::cli_code(format(action))
  if (utils::menu(c("Yes", "No")) == 1) {
    if (!debug) {
      rlang::eval_tidy(action)
    }
  } else {
    cli::cli_alert_warning("Action is aborted.")
  }
}

# Display usage
help_usage <- function(fun = data_manager) {
  fun_name <- as.character(substitute(fun))
  cli::cli_rule(center = "{fun_name} Help")
  cli::cli_code(args(fun))

  # usage of arguments
  cli::cli_rule(center = "Arguments")
  cli::cli_ul()
  cli::cli_li("dsn: name of data source of database")
  cli::cli_li("action: action to perform")
  ulid <- cli::cli_ul()
  cli::cli_li("process: process files for importing")
  cli::cli_li("update: update tables in dabase by importing files")
  cli::cli_li("clear: clear all tables in database")
  cli::cli_end(ulid)
  cli::cli_li("help: diplay usage or not")
  cli::cli_end(ulid)
  cli::cli_li("...: arguments to action function")
  ulid <- cli::cli_ul()
  cli::cli_li("retry_error: use logged file to retry actions with error, valid for process and update")
  cli::cli_li("force: clear all table without comfirmation, valid for clear")
  cli::cli_end(ulid)

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

  cli::cli_text("")
  cli::cli_rule(center = "Examples")
  cat(examples)
}

# Run data_manage_ui on interactive mode
if (interactive()) {
  data_manager_ui()
}

