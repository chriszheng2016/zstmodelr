# @include stock-db.R
#  Problems: including stock-db.R will lead to not to produce get_datasource.rd.
#  it seems to be ok if without including stock-db.R

# Generic functions for data mangement operation ------------------------------

#' Get data source info for importing raw data
#'
#' Generic function to get data source info for importing raw data into stock_db.
#'
#' @param stock_db         A stock database object to operate.
#'
#' @family data managment functions
#'
#' @return A dataframe of info about importing raw data if successfully,
#' otherwise NULL.
#'
#' @export
#'
#' @examples
# S3 generic definition
# get_datasource <- function(stock_db, ...){
#   UseMethod("get_datasource")
# }
# S4 generic definition
setGeneric(
  name = "get_datasource",
  signature = c("stock_db"),
  def = get_datasource <- function(stock_db, ...) {
    standardGeneric("get_datasource")
  }
)

#' Clear data in all tables in stock database
#'
#' Generic function to Clear existed data in all tables in stock_db.
#'
#' @param stock_db         A stock database object to operate.
#'
#' @family data managment functions
#'
#' @return
#'
#' @export
#'
#' @examples
# S3 generic definition
# clear_tables <- function(stock_db, ...){
#   UseMethod("clear_tables")
# }
# S4 generic definition
setGeneric(
  name = "clear_tables",
  signature = c("stock_db"),
  def = clear_tables <- function(stock_db, ...) {
    standardGeneric("clear_tables")
  }
)


#' Update tables in stock database with new data
#'
#' Generic function to update tables in stock database by importing new data
#'
#' By combining infomation from data_source and re-try log file , it update all
#' tables from raw data of datasource in following steps:
#'
#' \enumerate{
#'    \item build target tables info from all tables in datasource or some
#'    tables with recoding failure in retry log file.
#'    \item update target tables by importing data from datasoure.
#'    \item save update log, etc.
#' }
#'
#' Update log is saved in log dir, like "update_log_XXXX(current).csv"
#'
#'
#' @param stock_db  A stock database object to operate.
#' @param data_source  Data source info produced by get_datasource(stock_db)
#' @param retry_log Log file for re-importing tables with recording failure
#'   in log file. If NULL, it will upate all talbles in data_source, otherse
#'   only update on these tables with recording failure in log file.
#'   By default NULL.
#' @param log_dir Path to save updating log file. If NULL, don't save log file
#'   by default "./log".
#'
#'
#' @family data managment generics
#'
#' @return TRUE if success, else FALSE.
#' @export
#'
#' @examples

# S3 generic definition
# update_db <- function(stock_db,
#                          ...) {
#   UseMethod("update_db")
# }


# S4 generic definition
setGeneric(
  name = "update_db",
  signature = c("stock_db"),
  def = update_db <- function(stock_db,
                              data_source = get_datasource(stock_db),
                              retry_log = NULL,
                              log_dir = "./log",
                              ...) {
    standardGeneric("update_db")
  }
)

#' Import a raw data file into table in stock database
#'
#' Generic function to import a raw data into target table in stock database.
#'
#' Import porcess include two phrases:
#' \enumerate{
#'    \item Convert raw data into R data type.
#'    \item Transfer R data into table in database.
#' }
#'
#' Any problem in converting raw data will be logged in log file
#'  in log dir, e.g. "input_file(YYYY-MM-DD).csv"
#'
#' @param stock_db  A stock database object to operate.
#' @param input_file  A Name or a path of input data file.
#' @param input_type  Format of input file, e.g. "txt", "csv".
#' @param input_dir Working dir of input file, if NULL, use dir of input file as
#'   working dir, by default NULL,
#' @param start_index Start index of first line of actual records in data file,
#'   by default 2L, which means first line is header and actual data starts from
#'   second lines.
#' @param target_table  Name of target table in stock db, if NULL, use basename
#'   input_file as target table ,  by default NULL,
#' @param ignore_problems Whether to ignore problems when covert data,
#'   if TRUE, continue to import data but log problems into log file,
#'   otherwise abort importing process.  By default TRUE.
#' @param log_dir   Path of log dir for saving problem log file,
#'   by default"./log", if the log path doesn't exsited, it  will be created.
#'
#' @family data managment functions
#'
#' @return TRUE if success, else FALSE.
#' @export
#'
#' @examples

# S3 generic definition
# import_table <- function(stock_db,
#                          input_file,
#                          input_type = c("csv", "txt"),
#                          input_dir = NULL,
#                          start_index = 2L,
#                          target_table = NULL,
#                          ignore_problems = TRUE,
#                          log_dir = "./log",
#                          ...) {
#   UseMethod("import_table")
# }


# S4 generic definition
setGeneric(
  name = "import_table",
  signature = c("stock_db"),
  def = import_table <- function(stock_db,
                                   input_file,
                                   input_type = c("csv", "txt"),
                                   input_dir = NULL,
                                   start_index = 2L,
                                   target_table = NULL,
                                   ignore_problems = TRUE,
                                   log_dir = "./log",
                                   ...) {
    standardGeneric("import_table")
  }
)


# Non-generic functions for stock db operation ---------------------------------

#' Update tables in stock database with new data
#'
#' update tables in stock database by importing new data
#'
#' @param stock_db  A stock database object to operate.
#'
#' @family data managment functions
#'
#' @return
#' @export
#'
#' @examples


