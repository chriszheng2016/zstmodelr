# setClassUnion("optionalList", c("list", "NULL"))

# Class definition of stock_db class -------------------------------

# Abstract Class of StockDB
# setClassUnion("ANYOrNull", c("ANY", "NULL"))
setRefClass("stock_db",
  fields = list(
    dsn = "character",
    connection = "ANY"
  ),
  contains = "VIRTUAL"
)

#' subclass object factory of stock_db class
#'
#' factory for creating for subclass object of stock_db class
#'
#' @param stock_db_class   Specific creator of stock database class,
#'   e.g. gta_db
#' @param ... Addition params used by specific class of stock database
#'      (e.g. a dsn string "GTA_SQLData").
#'
#' @return A object of stock db.
#' @export
#'
#' @examples
#' \dontrun{
#' library(zstmodelr)
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
#' }
stock_db <- function(stock_db_class, ...) {

  # build class object
  class_object <- stock_db_class(...)

  return(class_object)
}

# Generic functions for stock_db operation ---------------------------------

#' Generic function for stock_db
#'
#' Generic function of operating stock database
#'
#' A group generic function to deal with a stock_db object
#'
#' @name stock_db_operation
#' @examples
#' \dontrun{
#' library(zstmodelr)
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
#' }
NULL


#' Open stock database
#'
#' Generic function to open a stock database
#'
#' @param stock_db A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#' @return TRUE if succeed otherwise FALSE
#' @export
#' @examples
#' \dontrun{
#' library(zstmodelr)
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
#' }
#'
#' # S3 generic definition
#' # open_stock_db <- function(stock_db, ...) {
#' #   UseMethod("open_stock_db")
#' # }
#'
#' # S4 generic definition
setGeneric(
  name = "open_stock_db",
  signature = c("stock_db"),
  def = open_stock_db <- function(stock_db, ...) {
    standardGeneric("open_stock_db")
  }
)


#' Close the stock database
#'
#' Generic function to close a stock database.
#'
#' @param stock_db A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return TRUE if succeed else FALSE.
#' @export
#'


# S3 generic definition
# close_stock_db <- function(stock_db, ...) {
#   UseMethod("close_stock_db")
# }

# S4 generic definition
setGeneric(
  name = "close_stock_db",
  signature = c("stock_db"),
  def = close_stock_db <- function(stock_db, ...) {
    standardGeneric("close_stock_db")
  }
)

#' Get profile of stock_db
#'
#' Generic function to get path of profile of stock_db.
#'
#' @param stock_db     A stock database object to operate.
#' @param profile_name  A character filename of profile.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return  full path of profile of stock db if succeed,
#' otherwise raise a error
#' @export
#'

# S3 generic definition
# get_profile <- function(stock_db, profile_name, ...){
#   UseMethod("get_profile")
# }
# S4 generic definition
setGeneric(
  name = "get_profile",
  signature = c("stock_db"),
  def = get_profile <- function(stock_db, profile_name, ...) {
    standardGeneric("get_profile")
  }
)

#' Init param of stock db
#'
#' Generic function to initiate params of a stock database.
#'
#' @param stock_db A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return TRUE if succeed else FALSE.
#'
#' @examples
#' \dontrun{
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' init_stock_db(stock_db)
#' }
#'
#' @export

# S3 generic definition
# init_stock_db <- function(stock_db, ...) {
#   UseMethod("init_stock_db")
# }

# S4 generic definition
setGeneric(
  name = "init_stock_db",
  signature = c("stock_db"),
  def = init_stock_db <- function(stock_db, ...) {
    standardGeneric("init_stock_db")
  }
)


#' List all tables of stock_db
#'
#' Generic function to list all tables of stock_db.
#'
#' @param stock_db  A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A vectors of characters of table names.
#'
#' @examples
#' \dontrun{
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' init_stock_db(stock_db)
#' list_stock_tables(stock_db)
#' }
#'
#' @export

# S3 generic definition
# list_stock_tables <- function(stock_db,...) {
#   UseMethod("list_stock_tables")
# }

# S4 generic definition
setGeneric(
  name = "list_stock_tables",
  signature = c("stock_db"),
  def = list_stock_tables <- function(stock_db, ...) {
    standardGeneric("list_stock_tables")
  }
)

#' Get dataset from a table in stock_db
#'
#' Generic function to get one dataset from stock_db.
#'
#' @param stock_db    A stock database object to operate.
#' @param table_name  Name of target table.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A data frame if succeed, otherwise NULL.
#'
#' @export
# S3 generic definition
# get_table_dataset <- function(stock_db, table_name,...) {
#   UseMethod("get_table_dataset")
# }
# S4 generic definition
setGeneric(
  name = "get_table_dataset",
  signature = c("stock_db"),
  def = get_table_dataset <- function(stock_db, table_name, ...) {
    standardGeneric("get_table_dataset")
  }
)

#' Get a dataset of a list of stock_cd from a table in stock_db
#'
#' Generic function to get a dataset of a list of stock_cd from table in stock.
#'
#' @param stock_db      A stock database object to operate.
#' @param table_name    Name of target table.
#' @param stock_cd_list A character vector of stock cd, default value of NULL means.
#'   all stock data will be returned.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A data frame on succeed otherwise NULL.
#'
#' @examples
#' \dontrun{
#' ds_trd_dalyr.df <- get_stock_dataset(stock_db,
#'   table_name = "TRD_Dalyr_鏃ヤ釜鑲″洖鎶ョ巼",
#'   stock_cd_list = c("600066", "000550")
#' )
#' }
#' @export

# S3 generic definition
# get_stock_dataset <- function(stock_db,
#                               table_name,
#                               stock_cd_list = NULL,
#                               ...) {
#   UseMethod("get_stock_dataset")
# }

# S4 generic definition
setGeneric(
  name = "get_stock_dataset",
  signature = c("stock_db"),
  def = get_stock_dataset <- function(stock_db,
                                      table_name,
                                      stock_cd_list = NULL,
                                      ...) {
    standardGeneric("get_stock_dataset")
  }
)


#' Fetch many datasets from stock_db
#'
#' Generic function to fetch many datasets from stock_db.
#'
#' @param stock_db   A stock database object to operate.
#' @param table_list A character vector of table names.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A list of names of table fetched if succeed.
#' @export
#'


# S3 generic definition
# fetch_table_dataset <- function(stock_db, table_list, ...) {
#   UseMethod("fetch_table_dataset")
# }

# S4 generic definition
setGeneric(
  name = "fetch_table_dataset",
  signature = c("stock_db"),
  def = fetch_table_dataset <- function(stock_db, table_list, ...) {
    standardGeneric("fetch_table_dataset")
  }
)

#' Get stock info from stock_db
#'
#' Generic function to get stock info from stock_db.
#'
#' @param stock_db    A stock database object to operate.
#' @param stock_cd_list A list of stock cd, default value of NULL means
#'  all stock data will return.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of stock info.
#' @export
#'
# S3 generic definition
# get_stock_info <- function(stock_db,
#                              stock_cd_list = NULL,
#                              ...) {
#   UseMethod("get_stock_info")
# }

# S4 generic definition
setGeneric(
  name = "get_stock_info",
  signature = c("stock_db"),
  def = get_stock_info <- function(stock_db,
                                   stock_cd_list = NULL,
                                   ...) {
    standardGeneric("get_stock_info")
  }
)

#' Get industry info from stock_db
#'
#' Generic function to get industry info from stock_db.
#'
#' @param stock_db    A stock database object to operate.
#' @param industry_codes A list of industry codes, default value of NULL means
#'  all industry data will return.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of stock info.
#' @export
#'
# S3 generic definition
# get_industry_info <- function(stock_db,
#                             industry_codes = NULL,
#                              ...) {
#   UseMethod("get_industry_info")
# }

# S4 generic definition
setGeneric(
  name = "get_industry_info",
  signature = c("stock_db"),
  def = get_industry_info <- function(stock_db,
                                   industry_codes = NULL,
                                   ...) {
    standardGeneric("get_industry_info")
  }
)

#' Get stock return timeseries from stock_db
#'
#' Generic function to get stock return timeseries from stock_db.
#'
#' @param stock_db    A stock database object to operate.
#' @param stock_cd_list A list of stock cd, default value of NULL means
#'   all stock data will return.
#' @param period_type Date period for time series, e.g. "day", "month",
#'   "year", by default "day".
#' @param period_date Choose start/end date of period as the date for
#'   observation
#' @param output_type Format of output result, e.g "timeSeries", "tibble".
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A timeseries of stock return.
#' @export
#'
# S3 generic definition
# get_stock_return <- function(stock_db,
#                              stock_cd_list = NULL,
#                              period_type = c("day", "month", "year"),
#                              period_date = c("start", "end"),
#                              output_type = c("timeSeries", "tibble"),
#                              ...) {
#   UseMethod("get_stock_return")
# }

# S4 generic definition
setGeneric(
  name = "get_stock_return",
  signature = c("stock_db"),
  def = get_stock_return <- function(stock_db,
                                     stock_cd_list = NULL,
                                     period_type = c("day", "month", "year"),
                                     period_date = c("start", "end"),
                                     output_type = c("timeSeries", "tibble"),
                                     ...) {
    standardGeneric("get_stock_return")
  }
)

#' Get market return timeseries from stock_db
#'
#' Generic function to get stock return timeseries from stock_db.
#'
#' @param stock_db    A stock database object to operate.
#' @param period_type Date period for time series, e.g. "day", "month",
#'   "year", by default value is  "day".
#' @param period_date Choose start/end date of period as the date for
#'   observation
#' @param output_type Format of output result, e.g "timeSeries", "tibble".
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A timeseries of market return.
#' @export
#'


# S3 generic definition
# get_market_return <- function(stock_db,
#                               period_type = c("day", "month", "year"),
#                               period_date = c("start", "end"),
#                               output_type = c("timeSeries", "tibble"),
#                               ...) {
#   UseMethod("get_market_return")
# }

# S4 generic definition
setGeneric(
  name = "get_market_return",
  signature = c("stock_db"),
  def = get_market_return <- function(stock_db,
                                      period_type = c("day", "month", "year"),
                                      period_date = c("start", "end"),
                                      output_type = c("timeSeries", "tibble"),
                                      ...) {
    standardGeneric("get_market_return")
  }
)

#' Get financial report timeseries from stock_db
#'
#' Generic function to get financial report timeseries from stock_db.
#'
#' @param stock_db    A stock database object to operate.
#'
#' @param stock_cd_list A character vector of stock cd, default value of NULL means.
#'   all stock data will be returned.
#'
#' @param statement   A string of statement type, i.e. "balance_sheet",
#' "income", "cashflow_direct","cashflow_indirect",
#' "income_ttm", "cashflow_direct_ttm", "cashflow_indirect_ttm".
#'
#' @param consolidated A logic indicate report is consolidated or not.
#'   Default TRUE means consolidated report, FALSE means parent company
#'   report.
#'
#' @param period_type Date period for time series, e.g. "quarter",
#'   "year", by default value is  "quarter".
#' @param period_date Choose start/end date of period as the date for
#'   observation
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A data frame of report timeseries if succeed, otherwise NULL.
#' @export
#'


# S3 generic definition
# get_financial_report <- function(stock_db,
#                               stock_cd_list = NULL,
#                               statement = c(
#                                 "balance_sheet",
#                                 "income",
#                                 "cashflow_direct",
#                                 "cashflow_indirect",
#                                 "income_ttm",
#                                 "cashflow_direct_ttm",
#                                 "cashflow_indirect_ttm"
#                               ),
#                               consolidated = TRUE,
#                               period_type = c("quarter", "year"),
#                               period_date = c("end","start"),
#                               ...) {
#   UseMethod("get_financial_report")
# }

# S4 generic definition
setGeneric(
  name = "get_financial_report",
  signature = c("stock_db"),
  def = get_financial_report <- function(stock_db,
                                         stock_cd_list = NULL,
                                         statement = c(
                                           "balance_sheet",
                                           "income",
                                           "cashflow_direct",
                                           "cashflow_indirect",
                                           "income_ttm",
                                           "cashflow_direct_ttm",
                                           "cashflow_indirect_ttm"
                                         ),
                                         consolidated = TRUE,
                                         period_type = c("quarter", "year"),
                                         period_date = c("end", "start"),
                                         ...) {
    standardGeneric("get_financial_report")
  }
)


#' Get indicators from specified source in stock_db
#'
#' Generic function to get indicator timeseries from specified source in stock_db.
#'
#' @param stock_db            A stock database object to operate.
#' @param indicator_source    A name of table or file(with extension)
#'   in which indicators are stored. For file, formats of rds,csv are supported.
#'
#' @param indicator_codes      A vector of indicator code. Default NULL return
#'   all indicators in the source.
#' @param ouput_format        Output format for data frame, i.e "long" and "wide":
#'   \itemize{
#'       \item in long format: code of indicator is stored in "ind_code",
#'       value of indicator is stored in "ind_value";
#'       \item in wide format: value of indicator is stored in
#'  field named with the name of indicator.
#'   }
#' @param ... Extra arguments to be passed to methods.
#'
#'
#' @family stock_db generics
#'
#' @return A data frame of indicator timeseries if succeed, otherwise NULL.
#' @export
#'

# S3 generic definition
# get_indicators_from_source <- function(stock_db, indicator_source,
#                                          indicator_codes = NULL,
#                                          ouput_format = c("long", "wide"),
#                                          ...)
#   UseMethod("get_indicators_from_source")
# }
# S4 generic definition
setGeneric(
  name = "get_indicators_from_source",
  signature = c("stock_db"),
  def = get_indicators_from_source <- function(stock_db, indicator_source,
                                               indicator_codes = NULL,
                                               ouput_format = c("long", "wide"),
                                               ...) {
    standardGeneric("get_indicators_from_source")
  }
)

#' Save indicators to specified source in stock_db
#'
#' Generic function to save indicator timeseries to specified source in stock_db.
#'
#' @param stock_db            A stock database object to operate.
#' @param indicator_source    A name of table or file(with extension)
#'   in which indicators are stored. For file, formats of rds,csv are supported.
#' @param ts_indicators       A dataframe of indicator timeseries
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#' @export
#'

# S3 generic definition
# save_indicators_to_source <- function(stock_db, indicator_source,
#                                       ts_indicators, ...)
#   UseMethod("save_indicators_to_source")
# }
# S4 generic definition
setGeneric(
  name = "save_indicators_to_source",
  signature = c("stock_db"),
  def = save_indicators_to_source <- function(stock_db, indicator_source,
                                              ts_indicators, ...) {
    standardGeneric("save_indicators_to_source")
  }
)



#' Get indicators from stock_db
#'
#' Generic function to get indicator timeseries from stock_db.
#'
#' @param stock_db            A stock database object to operate.
#' @param indicator_codes     A vector of indicator code.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of indicator timeseries if succeed, otherwise NULL.
#'   In order to store multi-indicators with different periods(day,
#'   month, quarter, year), returned data frame adapts longer format,
#'   which means code of indicator is stored in "ind_code", value of factor
#'   is stored in "ind_value".
#' @export
#'

# S3 generic definition
# get_indicators <- function(stock_db, indicator_codes, ...){
#   UseMethod("get_indicators")
# }
# S4 generic definition
setGeneric(
  name = "get_indicators",
  signature = c("stock_db"),
  def = get_indicators <- function(stock_db, indicator_codes, ...) {
    standardGeneric("get_indicators")
  }
)

#' Get indicators Info from stock_db
#'
#' Generic function to get indicator info from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param indicator_codes     A character vector of indicator groups.
#' Default NULL return all indicators.
##' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of matched indicators if succeed, otherwise NULL.
#' @export
#'

# S3 generic definition
# get_indicators_info <- function(stock_db, indicator_codes = NULL,
#                              ...){
#   UseMethod("get_indicators_info")
# }
# S4 generic definition
setGeneric(
  name = "get_indicators_info",
  signature = c("stock_db"),
  def = get_indicators_info <- function(stock_db, indicator_codes = NULL,
                                        ...) {
    standardGeneric("get_indicators_info")
  }
)


#' Get factors from stock_db
#'
#' Generic function to get factor timeseries from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param factor_codes    A vector of factor code.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of factor timeseries if succeed, otherwise NULL.
#'   In order to store multi-factors with different periods(day,
#'   month, quarter, year), returned data frame adapts longer format,
#'   which means name of factor is stored in "factor_name", value of factor
#'   is stored in "factor_value".
#' @export
#'

# S3 generic definition
# get_factors <- function(stock_db, factor_codes, ...){
#   UseMethod("get_factors")
# }
# S4 generic definition
setGeneric(
  name = "get_factors",
  signature = c("stock_db"),
  def = get_factors <- function(stock_db, factor_codes, ...) {
    standardGeneric("get_factors")
  }
)


#' Get factors Info from stock_db
#'
#' Generic function to get factors info from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param factor_codes     A character vector of factor groups. Default NULL return
#'   all factors.
#' @param factor_groups    A character vector of factor groups. Default NULL return
#'   all factors.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of matched factors if succeed, otherwise NULL.
#' @export
#'

# S3 generic definition
# get_factors_info <- function(stock_db, factor_codes = NULL,
#                              factor_group = NULL,...){
#   UseMethod("get_factors_info")
# }
# S4 generic definition
setGeneric(
  name = "get_factors_info",
  signature = c("stock_db"),
  def = get_factors_info <- function(stock_db, factor_codes = NULL,
                                     factor_groups = NULL, ...) {
    standardGeneric("get_factors_info")
  }
)

#' Get industry info of stocks from stock_db
#'
#' Generic function to get industry info timeseries from stock_db.
#'
#' @param stock_db     A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of industry timeseries of stocks if succeed,
#'   otherwise NULL.
#' @export
#'

# S3 generic definition
# get_stock_industry <- function(stock_db, ...){
#   UseMethod("get_stock_industry")
# }
# S4 generic definition
setGeneric(
  name = "get_stock_industry",
  signature = c("stock_db"),
  def = get_stock_industry <- function(stock_db, ...) {
    standardGeneric("get_stock_industry")
  }
)

#' Get stock info of special treatment from stock_db
#'
#' Generic function to stock info of special treatment from stock_db.
#'
#' @param stock_db     A stock database object to operate.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe stock info of special treatment if succeed, otherwise NULL.
#' @export
#'

# S3 generic definition
# get_spt_stocks <- function(stock_db, ...){
#   UseMethod("get_spt_stocks")
# }
# S4 generic definition
setGeneric(
  name = "get_spt_stocks",
  signature = c("stock_db"),
  def = get_spt_stocks <- function(stock_db, ...) {
    standardGeneric("get_spt_stocks")
  }
)


#' Get riskfree rate from stock_db
#'
#' Generic function to get riskfree rate timeseries from stock_db.
#'
#' @param stock_db     A stock database object to operate.
#' @param period       Date period of time series, e.g. "day", "month",
#'   "quarter", "year", default is "day".
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A dataframe of riskfree rate timeseries if succeed, otherwise NULL.
#' @export
#'

# S3 generic definition
# get_riskfree_rate <- function(stock_db, period = c("day", "month",
#                                                 "quarter", "year"), ...){
#   UseMethod("get_riskfree_rate")
# }
# S4 generic definition
setGeneric(
  name = "get_riskfree_rate",
  signature = c("stock_db"),
  def = get_riskfree_rate <- function(stock_db,
                                      period = c(
                                        "day", "month",
                                        "quarter", "year"
                                      ), ...) {
    standardGeneric("get_riskfree_rate")
  }
)


#' Get Path of Data Directory from stock_db
#'
#' Generic function to get path of data directory from stock_db.
#'
#'
#' @param stock_db         A stock database object to operate.
#' @param dir_id           A character id of directory.
#' \itemize{
#'       \item DIR_DB_DATA: dir of database of stock_db;
#'       \item DIR_DB_DATA_SOURCE : dir of source data to process into database;
#'       \item DIR_DB_DATA_ORIGIN:  dir of origin data to import into database;
#'       \item DIR_DB_DATA_LOG: dir of log of database operation;
#'       \item DIR_DB_DATA_INDICATOR: dir of customized indicators.
#'       }
#'
#' @param force            Whether return result if dir doesn't existed.
#'   Default TRUE, return result if dir doesn't exist.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return A full path of dir if succeed, otherwise NULL.
#'  If the path of dir doesn't exist and force = FALSE, it will raise a error.
#' @export
#'

# S3 generic definition
# dir_path_db <- function(stock_db,
#                      dir_id = c("DIR_DB_DATA",
#                                 "DIR_DB_DATA_SOURCE",
#                                 "DIR_DB_DATA_ORIGIN",
#                                 "DIR_DB_DATA_LOG",
#                                 "DIR_DB_DATA_INDICATOR"),
#                      force = TRUE,...)
#   UseMethod("dir_path_db")
# }
# S4 generic definition
setGeneric(
  name = "dir_path_db",
  signature = c("stock_db"),
  def = dir_path_db <- function(stock_db,
                                dir_id = c(
                                  "DIR_DB_DATA",
                                  "DIR_DB_DATA_SOURCE",
                                  "DIR_DB_DATA_ORIGIN",
                                  "DIR_DB_DATA_LOG",
                                  "DIR_DB_DATA_INDICATOR"
                                ),
                                force = TRUE, ...) {
    standardGeneric("dir_path_db")
  }
)


# Non-generic functions for stock db operation ---------------------------------

#' Get assets return from market and stocks return
#'
#' Get assets return timeseries by combining market return and stocks return
#'
#' @param benchmark_return   A timeseries of market index
#' @param stocks_return      A timeseries of a group stock stocks
#'
#'
#' @return A timeseries of assets return.
#' @export
get_assets_return <- function(benchmark_return, stocks_return) {
  stopifnot(
    timeSeries::is.timeSeries(benchmark_return),
    timeSeries::is.timeSeries(stocks_return)
  )


  start_date_market <- timeSeries::start(benchmark_return)
  end_date_market <- timeSeries::end(benchmark_return)

  start_date_stocks <- timeSeries::start(stocks_return)
  end_date_stocks <- timeSeries::end(stocks_return)

  # Used later start_date as start_date
  if (start_date_market >= start_date_stocks) {
    start_date <- start_date_market
  } else {
    start_date <- start_date_stocks
  }

  # Used early end_date as end_date
  if (end_date_market <= end_date_stocks) {
    end_date <- end_date_market
  } else {
    end_date <- end_date_stocks
  }

  # get data window between start_date and end_date
  benchmark_return <- timeSeries::window(benchmark_return, start_date, end_date)
  stocks_return <- timeSeries::window(stocks_return, start_date, end_date)

  # Combine benchmark and stocks return
  assets_return <- merge(benchmark_return, stocks_return)
  colnames(assets_return) <- stringr::str_replace(colnames(assets_return), "X", "")

  return(assets_return)
}

#' Get stocks excess return from stocks return and riskfree_rate
#'
#' Get excess return timeseries by combining stocks return and riskfree_rate
#'
#' @param ts_stocks_return   A timeseries of stocks_return
#' @param ts_riskfree_rate   A timeseries of riskfree_rate
#' @param period             A character of period, e.g. "day", "month",
#'   "quarter", "year". Default "day".
#' @param period_date        A character of period_date format, e.g. "start",
#'   "end", "start" format date as start of the period, "end" format date as end
#'   of period. Default "start".
#'
#'
#' @return A timeseries of excess return of stocks.
#' @export

stocks_excess_return <- function(ts_stocks_return,
                                 ts_riskfree_rate,
                                 period = c(
                                   "day", "month",
                                   "quarter", "year"
                                 ),
                                 period_date = c("start", "end")) {
  ts_stock_return_expr <- rlang::enexpr(ts_stocks_return)
  ts_riskfree_rate_expr <- rlang::enexpr(ts_riskfree_rate)

  # validate params
  assertive::assert_is_data.frame(ts_stocks_return)
  assertive::assert_is_data.frame(ts_riskfree_rate)

  # set date of timeseries
  period <- match.arg(period)
  period_date <- match.arg(period_date)
  if (is_periodic_dates(ts_stocks_return$date, freq_rule = period)) {
    ts_stocks_return$date <- as_period_date(ts_stocks_return$date,
      period = period,
      period_date = period_date
    )
  } else {
    msg <- sprintf(
      "The period of date of %s isn't %s",
      rlang::expr_text(ts_stock_return_expr),
      period
    )
    rlang::abort(msg)
  }

  if (is_periodic_dates(ts_riskfree_rate$date, freq_rule = period)) {
    ts_riskfree_rate$date <- as_period_date(ts_riskfree_rate$date,
      period = period,
      period_date = period_date
    )
  } else {
    msg <- sprintf(
      "The period of date of %s isn't %s",
      rlang::expr_text(ts_riskfree_rate_expr),
      period
    )
    rlang::abort(msg)
  }

  # connect riskfree rate with stkcds
  stkcds <- unique(ts_stocks_return$stkcd)
  ds_stkcds <- tibble::tibble(stkcd = stkcds)
  ts_riskfree_rate_with_stkcd <- ds_stkcds %>%
    dplyr::mutate(data = list(ts_riskfree_rate)) %>%
    tidyr::unnest(data)

  # combine stocks_return and riskfree_rate to compute excess return
  ts_stocks_excess_return <- ts_stocks_return %>%
    dplyr::inner_join(ts_riskfree_rate_with_stkcd, by = c("date", "stkcd")) %>%
    dplyr::mutate(excess_return = return - riskfree_return)

  return(ts_stocks_excess_return)
}

# Get a timeseries of stock data for specified stock from table datasets
get_stock_field_dataset <- function(ds_source.df,
                                    stock_cd,
                                    target_field,
                                    stkcd_field = "stkcd",
                                    date_field = "trdmnt",
                                    tseries_type = c("timeSeries", "xts"),
                                    debug = FALSE) {

  # Validate param
  if (is.null(ds_source.df) || missing(stock_cd)
  || missing(target_field) || missing(date_field)) {
    rlang::abort("ds_source.df, stock_cd, target_field, date_field  mustn't be null")
  }

  # Check whether the datafields existed
  field_list <- c(stkcd_field, target_field, date_field)
  if (!all(field_list %in% names(ds_source.df))) {
    error_fields <- NULL
    for (field_name in field_list) {
      if (!field_name %in% names(ds_source.df)) {
        error_fields <- ifelse(is.null(error_fields),
          field_name,
          paste(error_fields, field_name, sep = ",")
        )
      }
    }

    error_msg <- sprintf("%s doesn't exist in dataset ", error_fields)
    rlang::abort(error_msg)
  }

  tseries_type <- match.arg(tseries_type)
  if (is.null(tseries_type)) {
    rlang::warn("teries_type should be timeSeries or xts, set as timeSeries by default")
    tseries_type <- "timeSeries"
  }

  # Coerce stock code to 6 digit of characters
  if (!is.character(stock_cd)) {
    stock_cd <- stringr::str_pad(stock_cd, width = 6, pad = "0")
    msg <- "Coerce stock cd to character of 6 digits if it were number"
    warnings(msg)
  }

  # Get related data of the specified stock from all stock trd_mnth data table
  ds_stock_data.df <- na.omit(ds_source.df[ds_source.df$stkcd == stock_cd, ])


  # Build result dataset for specified stock
  result_ts <- NULL
  if (tseries_type == "timeSeries") {
    # timeSeries data series
    ds_name <- sprintf("ds_%s_%s.fts", stock_cd, target_field)
    result_ts <- timeSeries::timeSeries(
      ds_stock_data.df[target_field],
      zoo::as.Date(zoo::as.yearmon(
        ds_stock_data.df[[date_field]]
      ))
    )
    colnames(result_ts) <- stock_cd
  } else {
    # xts data series
    ds_name <- sprintf("ds_%s_%s.xts", stock_cd, target_field)
    result_ts <- xts::xts(ds_stock_data.df[target_field],
      order.by = zoo::as.Date(
        zoo::as.yearmon(ds_stock_data.df[[date_field]])
      )
    )
    colnames(result_ts) <- stock_cd
  }

  # Sort result
  result_ts <- sort(result_ts)

  # Return the result timeseries of stock
  if (debug) {

    # keep the result_ts in GlobalEnv for debug check
    assign(ds_name, result_ts, pos = .GlobalEnv)
    return(get(ds_name))
  } else {

    # reutrn the result_as as ususal
    return(result_ts)
  }
}

# Get several timeseries of stocks data for multiple stocks from periodic dataset
fetch_stock_field_dataset <- function(ds_source.df,
                                      stock_cd_list,
                                      replaceNA = c("keep", "zeros", "mean", "median"),
                                      ...) {


  # Validate params
  if (is.null(ds_source.df) || missing(stock_cd_list)) {
    rlang::abort("ds_source.df, stock_cd_list mustn't be null")
  }

  stopifnot(length(stock_cd_list) != 0, length(ds_source.df) != 0)

  # Coerce stock code to 6 digit of characters
  if (!is.character(stock_cd_list)) {
    stock_cd_list <- stringr::str_pad(stock_cd_list, width = 6, pad = "0")
    msg <- "Coerce stock cd to character of 6 digits if it were number"
    warnings(msg)
  }

  ds_result <- NULL
  for (the_stock_cd in stock_cd_list) {

    # get stock data for specified stock
    ds_stock_data <- get_stock_field_dataset(
      ds_source.df = ds_source.df,
      stock_cd = the_stock_cd, ...
    )

    # Build the result dataset
    if (is.null(ds_result)) {
      # Set stock data as result dataset
      ds_result <- ds_stock_data
    } else {
      # Merge stock data into the result dataset
      ds_result <- merge(ds_result, ds_stock_data)
    }
  }

  # remove the na as request
  na_type <- match.arg(replaceNA)
  if (na_type != "keep") {
    ds_nona_result <- timeSeries::substituteNA(ds_result, type = na_type)
    if (xts::is.xts(ds_result)) {
      ds_result <- xts::as.xts(ds_nona_result)
    }
    else {
      ds_result <- ds_nona_result
    }
  }

  return(ds_result)
}

# Generic functions for tranlation between code and name -----------------------

#' Translate code into name
#'
#' Generic function to translate code into name
#'
#' @param x    A object containing code/name information.
#' @param code A code or a vector of codes to be translated.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return     A name or a vector of names.
#' @export
#'


# S3 generic definition
# code2name <- function(x, code, ...) {
#   UseMethod("code2name")
# }

# S4 generic definition
setGeneric(
  name = "code2name",
  signature = c("x"),
  def = code2name <- function(x, code, ...) {
    standardGeneric("code2name")
  }
)


#' Translate name into code
#'
#' Generic function to translate name into code
#'
#' @param x     A object containing code/name information.
#' @param name  A name or a vector of names to be translated.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family stock_db generics
#'
#' @return      A code or a vector of codes.
#' @export
#'


# S3 generic definition
# name2code <- function(x, name, ...) {
#   UseMethod("name2code")
# }

# S4 generic definition
setGeneric(
  name = "name2code",
  signature = c("x"),
  def = name2code <- function(x, name, ...) {
    standardGeneric("name2code")
  }
)
