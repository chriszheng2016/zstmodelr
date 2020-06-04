# Tools for producing customized indicators periodically
library(zstmodelr)

# Procduce indicators
produce_indicators <- function(dsn = c("GTA_SQLData"),
                               indicator_codes = NULL,
                               validate_def = FALSE,
                               parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # validate params
  if (!is.null(indicator_codes)) assertive::assert_is_character(indicator_codes)

  # conect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  # build indicator definition list
  ds_indicator_defs_origin <- get_indicator_defs(stock_db)

  # get related indicator defs of target indicators
  if (!is.null(indicator_codes)) {
    ds_indicator_defs_origin <- related_indicator_defs(ds_indicator_defs_origin,
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
  ds_indicator_defs_priority <- prioritize_indicator_defs(ds_indicator_defs_origin)

  # produce indicators by order of priority
  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")
  validate_stkcds <- c("600031", "000157", "600066", "000550")
  for (i in seq_len(NROW(ds_indicator_defs_priority))) {
    ds_indicator_defs <- ds_indicator_defs_priority$ds_indicator_defs[[i]]

    # produce indicators in batch mode
    log_file_prefix <- sprintf("generate_indicator_log(batch#%d)", i)
    generate_indicators(stock_db,
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

  # conect to target stock db
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
                               ...) {
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
#
# Archive all indicators files by default
# indicator_producer(fun = "archive")
#
# Archive all indicators files by default
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
