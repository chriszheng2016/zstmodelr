# Tools for producing customized indicators periodically
library(zstmodelr)
# enable parallel process
enable_parallel()

# Produce indicators
produce_indicators <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
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

# Archive indicators
archive_indicators <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
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
clear_indicators <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
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

# Fetch action fun by name
.action_fun <- function(action = c("produce", "archive", "clear")){

  action <- match.arg(action)
  action_fun <- switch(
    action,
    "produce" = {
      rlang::quo(produce_indicators)
    },
    "archive" = {
      rlang::quo(archive_indicators)
    },
    "clear" = {
      rlang::quo(clear_indicators)
    }
  )

  action_fun
}

# Main function to indicator producers
indicator_producer <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                               action = c("produce", "archive", "clear"),
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

# Interactive UI of main function to indicator producers
indicator_producer_ui <- function(debug = FALSE) {

  # prompt use select action function
  action_fun <- interactive_call_(rlang::quo(.action_fun), quiet = TRUE)

  # run action interactively
  interactive_call_(action_fun, debug = debug)
}

help_usage <- function() {

  # usage of arguments
  argment_desc <- c('
  * dsn: name of data source of database.

  * action: action to perform.
    - produce: produce indicators files.
    - archive: save historical indicators files.
    - clear: clear all indicators files.

  * ...: arguments to action function.
    - indicator_codes: a character vector of indicator code to perform.
    - validate_def: a logical on whether conduct validation or not.
    - validate_stkcds: a charactor vector of stkcd to perform.
    - parallel: a logical on whether compute by parallel process.
    - force: clear all files without comfirmation.

  * help: diplay usage or not

  ')

  # usage of examples
  examples <- c('
  # Produce all indicators in parallel process(Production)
  indicator_producer(action = "produce", parallel = TRUE)

  # Produce all indicators in non-parallel process(Debug)
  indicator_producer(action = "produce", parallel = FALSE)

  # Produce some indicators in parallel process(Production)
  indicator_producer(action = "produce", indicator_codes = c("m_nop_ttm"), parallel = TRUE)

  # Validate all indicator definition
  indicator_producer(action = "produce", validate_def = TRUE, parallel = FALSE)

  # Validate some indicator definition
  indicator_producer(action = "produce", indicator_codes = c("m_nop_ttm"),
                      validate_def = TRUE, parallel = FALSE)

  # Validate some indicator definition for some stkcds
  indicator_producer(action = "produce", indicator_codes = c("m_ev"),
        validate_def = TRUE, validate_stkcds = c("000015"), parallel = FALSE)

  # Archive all indicators files by default
  indicator_producer(action = "archive")

  # Archive some indicator files by default
  indicator_producer(action = "archive", indicator_codes = c("m_nop_ttm"))

  # Clear all indicators files by default
  indicator_producer(action = "clear")

  # Clear all indicators files forcefully
  indicator_producer(action = "clear", force = TRUE)

  # Clear some indicators files
  indicator_producer(action = "clear", indicator_codes = c("m_nop_ttm"))
  ')

  help_fun(indicator_producer, argument_desc = argment_desc,
           examples = examples)
}


# Run indicator_producer in direct mode
# indicator_producer(help = TRUE)
#
# Run indicator_producer in interactive mode
# indicator_producer_ui()
#



