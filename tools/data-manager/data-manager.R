# Tools for updating database periodically

library(zstmodelr)

# Main function to conduct data management
data_manager <- function() {

  # Conect to target stock db
  # stock_db <- stock_db(gta_db, "GTA_SQLData_TEST")
  stock_db <- stock_db(gta_db, "GTA_SQLData")
  open_stock_db(stock_db)

  log_dir <- dir_path_db(stock_db, dir_id = "DIR_DB_DATA_LOG")

  # Update all tables
  update_db(stock_db,
    retry_log = NULL,
    log_dir = log_dir
  )

  # Update only faild tables recored in th log file
  # update_db(stock_db,
  #           retry_log = "update_log_GTA_SQLDATA(current).csv",
  #           log_dir = log_dir)


  # Close stock db
  close_stock_db(stock_db)
}

# run data manager
data_manager()
