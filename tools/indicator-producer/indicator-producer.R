# Tools for producing customized indicators periodically
library(zstmodelr)
# enable parallel process
enable_parallel()

# Produce indicators
produce_indicators <- function(dsn = c("GTA_SQLData"),
                               indicator_codes = NULL,
                               validate_def = FALSE,
                               validate_stkcds = c("600031", "000157", "600066", "000550"),
                               parallel = getOption("zstmodelr.common.parallel", TRUE)) {
  # validate params
  assertive::assert_is_a_non_empty_string(dsn)
  if (!is.null(indicator_codes)) {
    assertive::assert_is_character(indicator_codes)
  }
  assertive::assert_is_logical(validate_def)
  if (validate_def) {
    assertive::assert_is_character(validate_stkcds)
  }
  assertive::assert_is_logical(parallel)

  # connect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  # build indicator definition list
  ds_indicator_defs_origin <- get_indicator_defs(stock_db)

  # get related indicator defs of target indicators
  if (!is.null(indicator_codes)) {
    ds_indicator_defs_origin <-
      related_indicator_defs(ds_indicator_defs_origin,
        indicator_codes = indicator_codes
      )
    msg <- sprintf(
      "\nProduce related indicators: %s...",
      paste0(ds_indicator_defs_origin$ind_code, collapse = ",")
    )
  } else {
    msg <- sprintf("\nProduce all indicators...")
  }

  # Notice indicators to be produced
  rlang::inform(msg)

  # prioritize indicator_defs by dependency among indicators
  ds_indicator_defs_priority <-
    prioritize_indicator_defs(ds_indicator_defs_origin)

  # produce indicators by order of priority
  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")
  for (i in seq_len(NROW(ds_indicator_defs_priority))) {
    ds_indicator_defs <-
      ds_indicator_defs_priority$ds_indicator_defs[[i]]

    # produce indicators in batch mode
    log_file_prefix <-
      sprintf("generate_indicator_log(batch#%d)", i)
    generate_indicators(
      stock_db,
      ds_indicator_defs = ds_indicator_defs,
      validate_def = validate_def,
      validate_stkcds <- validate_stkcds,
      parallel = parallel,
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    )
  }

  close_stock_db(stock_db)
}

# archive indicators
archive_indicators <- function(dsn = c("GTA_SQLData"),
                               indicator_codes = NULL,
                               ...) {

  # connect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  msg <- sprintf("\nIndicators files will be archived...\n")
  message(msg)

  # build indicator definition list
  suppressMessages(
    ds_indicator_defs <- get_indicator_defs(stock_db)
  )

  # filter ind_defs by specified indicator codes
  if (!is.null(indicator_codes)) {
    # check specified indicators existed in defs
    indicator_exsited <- indicator_codes %in% ds_indicator_defs$ind_code
    if (any(!indicator_exsited)) {
      msg <- sprintf(
        "There is no indicator definition for %s.",
        paste0(indicator_codes[!indicator_exsited], collapse = ",")
      )
      rlang::abort(msg)
    }

    ds_indicator_defs <- ds_indicator_defs %>%
      dplyr::filter(ind_code %in% indicator_codes)
  }

  # backup indicators files
  path_archive_dir <- backup_indicators(stock_db, ds_indicator_defs,
    backup_dir = "backup"
  )

  msg <- sprintf(
    "Indicator files arachived in %s.",
    path_archive_dir
  )
  rlang::inform(msg)

  # close stock db
  close_stock_db(stock_db)
}

# Clear indicators
clear_indicators <- function(dsn = c("GTA_SQLData"),
                             indicator_codes = NULL,
                             force = FALSE, ...) {
  continue <- FALSE

  # force to excute without comfirmation
  if (force) continue <- TRUE

  # ask user's comfirmation to excute
  if (!continue) {
    answer <- readline("Do you want to clear indicators files(yes or no)!!? ")
    if (substr(answer, 1, 1) == "y") {
      continue <- TRUE
    }
  }

  # ask user whether to backup files firstly
  if (continue) {
    answer <- readline("Do you want to archive indicators files(yes or no)!!? ")
    if (substr(answer, 1, 1) == "y") {
      archive_indicators(dsn, indicator_codes = indicator_codes, ...)
    }
  }

  # carry out excution
  if (continue) {

    # conect to target stock db
    dsn <- match.arg(dsn)
    stock_db <- stock_db(gta_db, dsn)
    open_stock_db(stock_db)
    init_stock_db(stock_db)

    msg <- sprintf("\nIndicators files will be cleared...\n")
    message(msg)

    # build indicator definition list
    suppressMessages(
      ds_indicator_defs <- get_indicator_defs(stock_db)
    )

    # filter ind_defs by specified indicator codes
    if (!is.null(indicator_codes)) {
      # check specifed indicators existed in defs
      indicator_exsited <- indicator_codes %in% ds_indicator_defs$ind_code
      if (any(!indicator_exsited)) {
        msg <- sprintf(
          "There is no indicator definition for %s.",
          paste0(indicator_codes[!indicator_exsited], collapse = ",")
        )
        rlang::abort(msg)
      }

      ds_indicator_defs <- ds_indicator_defs %>%
        dplyr::filter(ind_code %in% indicator_codes)
    }

    # clears indicators files
    delete_indicators(stock_db, ds_indicator_defs)

    # close stock db
    close_stock_db(stock_db)
  }
}



# Main function to indicator producers
indicator_producer <- function(dsn = c("GTA_SQLData"),
                               fun = c("produce", "archive", "clear"),
                               help = FALSE,
                               ...) {
  if (help) {
    help_usage()
  } else {
    dsn <- match.arg(dsn)
    fun <- match.arg(fun)
    switch(fun,
           "produce" = {
             produce_indicators(dsn, ...)
           },
           "archive" = {
             archive_indicators(dsn, ...)
           },
           "clear" = {
             clear_indicators(dsn, ...)
           }
    )
  }
}
# Interactive UI of main function to conduct data management
data_manager_ui <- function(fun = indicator_producer, debug = FALSE) {

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
    "produce" = {
      other_args <-
        .prompt_arg_value(arg = retry_error, fun = process_stock_db)
    },
    "archive" = {
      other_args <-
        .prompt_arg_value(arg = retry_error, fun = update_stock_db)
    },
    "clear" = {
      other_args <- .prompt_arg_value(arg = force, fun = clear_indicators)
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
help_usage <- function(fun = indicator_producer) {
  fun_name <- as.character(substitute(fun))
  cli::cli_rule(center = "{fun_name} Help")
  cli::cli_code(args(fun))

  # usage of arguments
  cli::cli_rule(center = "Arguments")
  cli::cli_ul()
  cli::cli_li("dsn: name of data source of database")
  cli::cli_li("action: action to perform")
  ulid <- cli::cli_ul()
  cli::cli_li("produce: producing indicators files")
  cli::cli_li("archive: save historical indicators files")
  cli::cli_li("clear: clear all indicators files")
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
  # Produce all indicators in parallel process(Production)
    indicator_producer(fun = "produce", parallel = TRUE)
  # Produce all indicators in non-parallel process(Debug)
    indicator_producer(fun = "produce", parallel = FALSE)
  # Produce some indicators in parallel process(Production)
    indicator_producer(fun = "produce", indicator_codes = c("m_nop_ttm"), parallel = TRUE)
  # Validate all indicator definition
    indicator_producer(fun = "produce", validate_def = TRUE, parallel = FALSE)
  # Validate some indicator definition
    indicator_producer(fun = "produce", indicator_codes = c("m_nop_ttm"),
                      validate_def = TRUE, parallel = FALSE)
  # Archive all indicators files by default
    indicator_producer(fun = "archive")
  # Archive some indicator files by default
    indicator_producer(fun = "archive", indicator_codes = c("m_nop_ttm"))
  # Clear all indicators files by default
    indicator_producer(fun = "clear")
  # Clear all indicators files forcefully
    indicator_producer(fun = "clear", force = TRUE)
  # Clear some indicators files
    indicator_producer(fun = "clear", indicator_codes = c("m_nop_ttm"))
  ')

  cli::cli_text("")
  cli::cli_rule(center = "Examples")
  cat(examples)
}

# Run indicators producer
#
# Produce all indicators in parallel process(Production)
# indicator_producer(fun = "produce", parallel = TRUE)
#
# Produce all indicators in non-parallel process(Debug)
# indicator_producer(fun = "produce", parallel = FALSE)
#
# Produce some indicators in parallel process(Production)
# indicator_producer(fun = "produce", indicator_codes = c("m_nop_ttm"), parallel = TRUE)
#
# Validate all indicator definition
# indicator_producer(fun = "produce", validate_def = TRUE, parallel = FALSE)
#
# Validate some indicator definition
# indicator_producer(fun = "produce", indicator_codes = c("m_nop_ttm"),
#                    validate_def = TRUE, parallel = FALSE)
# Validate some indicator definition for some stkcds
# indicator_producer(fun = "produce", indicator_codes = c("m_ev"),
#      validate_def = TRUE, validate_stkcds = c("000015"), parallel = FALSE)
#
# Archive all indicators files by default
# indicator_producer(fun = "archive")
#
# Archive some indicator files by default
# indicator_producer(fun = "archive", indicator_codes = c("m_nop_ttm"))
#
# Clear all indicators files by default
# indicator_producer(fun = "clear")
#
# Clear all indicators files forcefully
# indicator_producer(fun = "clear", force = TRUE)
#
# Clear some indicators files
# indicator_producer(fun = "clear", indicator_codes = c("m_nop_ttm"))


