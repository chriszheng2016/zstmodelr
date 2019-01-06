# Tools for producing customized indicators periodically
library(zstmodelr)

# Procduce indicators
produce_indicators <- function(dsn = c("GTA_SQLData"),
                              validate_def = FALSE,
                              parallel = TRUE) {

  # conect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  # build indicator definition list
  ds_indicator_defs_origin <- get_indicator_defs(stock_db)

  # prioritize indicator_defs by dependency among indicators
  ds_indicator_defs_priority <- prioritize_indicator_defs(ds_indicator_defs_origin)

  # produce indicators by order of priority
  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")
  for (i in seq_len(NROW(ds_indicator_defs_priority))) {
    ds_indicator_defs <- ds_indicator_defs_priority$ds_indicator_defs[[i]]

    # produce indicators in batch mode
    log_file_prefix <- sprintf("generate_indicator_log(batch#%d)", i)
    generate_indicators(stock_db,
      ds_indicator_defs = ds_indicator_defs,
      validate_def = validate_def,
      parallel = parallel,
      log_file_prefix = log_file_prefix,
      log_dir = log_dir
    )
  }

  close_stock_db(stock_db)
}

# Clear indicators
clear_indicators <- function(dsn = c("GTA_SQLData"),
                             force = FALSE, ...) {
  continue <- FALSE

  # force to excute without comfirmation
  if (force) continue <- TRUE

  # ask user's comfirmation to excute
  if (!continue) {
    answer <- readline("Do you want to clear all indicators files(yes or no)!!? ")
    if (substr(answer, 1, 1) == "y") {
      continue <- TRUE
    }
  }

  # carry out excution
  if (continue) {

    # conect to target stock db
    dsn <- match.arg(dsn)
    stock_db <- stock_db(gta_db, dsn)
    open_stock_db(stock_db)
    init_stock_db(stock_db)

    msg <- sprintf("\nAll indicators files will be cleared...\n")
    message(msg)

    # build indicator definition list
    ds_indicator_defs <- get_indicator_defs(stock_db)

    # clears all indicators files
    delete_indicators(stock_db, ds_indicator_defs)

    # close stock db
    close_stock_db(stock_db)
  }
}

# Main function to indicator producers
indicator_producer <- function(dsn = c("GTA_SQLData"),
                              fun = c("produce", "clear"),
                              ...) {
  dsn <- match.arg(dsn)
  fun <- match.arg(fun)
  switch(fun,
    "produce" = {
      produce_indicators(dsn, ...)
    },
    "clear" = {
      clear_indicators(dsn, ...)
    }
  )
}

# Run indicators producer
#
# Produce indicator in parallel process(Production)
# indicator_producer(fun = "produce", parallel = TRUE)
#
# Produce indicator in non-parallel process(Debug)
# indicator_producer(fun = "produce", parallel = FALSE)
#
# Validate indicator definition
# indicator_producer(fun = "produce", validate_def = TRUE, parallel = FALSE)
#
# Clear all indicators files by default
# indicator_producer(fun = "clear")
#
# Clear all indicators files forcefully
# indicator_producer(fun = "clear", force = TRUE)
