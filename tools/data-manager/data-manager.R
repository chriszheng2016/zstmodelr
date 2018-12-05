# Tools for updating database periodically

library(zstmodelr)

# Update stock database
update_stock_db <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                            retry_error = FALSE,
                            ...) {

  # connect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)

  # whether to retry tables with errors
  log_file <- NULL
  if (!retry_error) {
    # update all tables
    msg <- sprintf("\nUpdate all tables in stock db...\n")
    message(msg)
  } else {
    # update only faild tables recored in the log file
    log_file <- "update_log_GTA_SQLDATA(current).csv"
    msg <- sprintf(
      "\nRetry to update tables with errors logged in %s...\n",
      log_file
    )
    message(msg)
  }

  # update tables
  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")
  update_db(stock_db,
    retry_log = log_file,
    log_dir = log_dir
  )

  # close stock db
  close_stock_db(stock_db)
}

# Clear stock database
clear_stock_db <- function(dsn = c("GTA_SQLData", "GTA_SQLData_TEST"),
                           force = FALSE, ...) {
  continue <- FALSE

  # force to excute without comfirmation
  if (force) continue <- TRUE

  # ask user's comfirmation to excute
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
                         fun = c("update", "clear"),
                         ...) {
  dsn <- match.arg(dsn)
  fun <- match.arg(fun)
  switch(fun,
    "update" = {
      update_stock_db(dsn, ...)
    },
    "clear" = {
      clear_stock_db(dsn, ...)
    }
  )
}

# run data manager
#
# Udate all tables by default
# data_manager(dsn = "GTA_SQLData", fun = "update")
#
# Update tables with loged errors
# data_manager(dsn = "GTA_SQLData", fun = "update", retry_error = TRUE)
#
# Clear all tables by default
# data_manager(dsn = "GTA_SQLData", fun = "clear")
#
# # Clear all tables by default
# data_manager(dsn = "GTA_SQLData", fun = "clear", force = TRUE)
