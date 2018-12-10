# Tools for producing customized indicators periodically
library(zstmodelr)

# Procduce indicators
produce_indictors <- function(dsn = c("GTA_SQLData"),
                              validate_def = FALSE,
                              parallel = TRUE) {

  # conect to target stock db
  dsn <- match.arg(dsn)
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  # build indicator definition list
  ds_indicator_defs <- get_indicator_defs(stock_db)

  # produce indicators in batch mode
  generate_indictors(stock_db,
                     ds_indicator_defs = ds_indicator_defs,
                     validate_def = validate_def,
                     parallel = parallel)

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
indictor_producer <- function(dsn = c("GTA_SQLData"),
                              fun = c("produce", "clear"),
                               ...){

  dsn <- match.arg(dsn)
  fun <- match.arg(fun)
  switch(fun,
         "produce" = {
           produce_indictors(dsn, ...)
         },
         "clear" = {
           clear_indicators(dsn, ...)
         }
  )

}

# Run indicators producer
#
# Produce indicator in parallel process(Production)
# indictor_producer(fun = "produce", parallel = TRUE)
#
# Produce indicator in non-parallel process(Debug)
# indictor_producer(fun = "produce", parallel = FALSE)
#
# Validate indicator definition
# indictor_producer(fun = "produce", validate_def = TRUE, parallel = FALSE)
#
## Clear all indicators files by default
# indictor_producer(fun = "clear")
#
# Clear all indicators files forcefully
# indictor_producer(fun = "clear", force = TRUE)
