# setClassUnion("optionalList", c("list", "NULL"))

# Class definition of stock_db class -------------------------------

# Abstract Class of StockDB
# setClassUnion("ANYOrNull", c("ANY", "NULL"))
setRefClass("stock_db",
            fields =  list(dsn = "character",
                           connection = "ANY"),
            contains = "VIRTUAL")

#' subclass object factory of stock_db class
#'
#' factory for creating for subclass object of stock_db class
#'
#' @param stock_db_class   specific creator of stock database class,
#' e.g. gta_db
#' @param ... addition params used by specific class of stock database(e.g. a dsn string
#' "GTA_SQLData")
#'
#' @return a object of stock db
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#'
stock_db <- function(stock_db_class, ...) {

  # build class object
  class_object <- stock_db_class(...)

  return(class_object)

}

# Generic functions for stock_db operation ---------------------------------

#' Generic function for stock_db
#
#' Generic function of operating stock database
#'
#' A group generic function to deal with a stock_db object
#'
#' @name stock_db_operations
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
NULL

#' Open stock database
#'
#' Generic function to open a stock database
#'
#' @param stock_db a stock database object to operate
#'
#' @return TRUE if success, else FALSE
#' @export
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)

# S3 generic definition
# open_stock_db <- function(stock_db, ...) {
#   UseMethod("open_stock_db")
# }

# S4 generic definition
setGeneric(name = "open_stock_db",
           signature = c("stock_db"),
           def = open_stock_db <- function(stock_db, ...) {
             standardGeneric("open_stock_db")
           })


#' Close the stock database
#'
#' Generic function to close a stock database.
#'
#' @param stock_db a stock database object to operate.
#'
#' @return TRUE if success, else FALSE.
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)

# S3 generic definition
# close_stock_db <- function(stock_db, ...) {
#   UseMethod("close_stock_db")
# }

# S4 generic definition
setGeneric(name = "close_stock_db",
           signature = c("stock_db"),
           def = close_stock_db <- function(stock_db, ...) {
             standardGeneric("close_stock_db")
           })


#' Init param of stock db
#'
#' Generic function to initiate params of a stock database.
#'
#' @param stock_db a stock database object to operate.
#'
#' @return TRUE if success, else FALSE.
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' init_stock_db(stock_db)

# S3 generic definition
# init_stock_db <- function(stock_db, ...) {
#   UseMethod("init_stock_db")
# }

# S4 generic definition
setGeneric(name = "init_stock_db",
           signature = c("stock_db"),
           def = init_stock_db <- function (stock_db, ...) {
             standardGeneric("init_stock_db")
           })


#' List all tables of stcok_db
#'
#' Generic function to list all tables of stock_db.
#'
#' @param stock_db  a stock database object to operate.
#'
#' @return a vectors of characters of table names.
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' init_stock_db(stock_db)
#' list_stock_tables(stock_db)

# S3 generic definition
# list_stock_tables <- function(stock_db,...) {
#   UseMethod("list_stock_tables")
# }

# S4 generic definition
setGeneric(name = "list_stock_tables",
           signature = c("stock_db"),
           def = list_stock_tables <- function (stock_db, ...) {
             standardGeneric("list_stock_tables")
           })

#' Get adataset from a table in stock_db
#'
#' Generic function to get one dataset from stock_db.
#'
#' @param stock_db    a stock database object to operate.
#' @param table_name  name of target table.
#'
#' @return A data frame on success, or NULL.
#' @export
#'
#' @examples
#' ds_trd_mnth.df <- get_table_dataset(stock_db, table_name = "TRD_Mnth_月个股回报率")

# S3 generic definition
# get_table_dataset <- function(stock_db, table_name,...) {
#   UseMethod("get_table_dataset")
# }

# S4 generic definition
setGeneric(name = "get_table_dataset",
           signature = c("stock_db"),
           def = get_table_dataset <- function (stock_db, table_name,...) {
             standardGeneric("get_table_dataset")
           })

#' Get a dataset of a list of stock_cd from a table in stock_db
#'
#' Generic function to get a dataset of a list of stock_cd from table in stock.
#'
#' @param stock_db      a stock database object to operate.
#' @param table_name    name of target table.
#' @param stock_cd_list a list of stock cd, default value of NULL means.
#'     all stock data will be returned.
#'
#' @return A data frame on success, or NULL.
#' @export
#'
#' @examples
#' ds_trd_dalyr.df <- get_stock_dataset(stock_db,
#'                             table_name = "TRD_Dalyr_日个股回报率",
#'                             stock_cd_list = c("600066", "000550"))

# S3 generic definition
# get_stock_dataset <- function(stock_db,
#                               table_name,
#                               stock_cd_list = NULL,
#                               ...) {
#   UseMethod("get_stock_dataset")
# }

# S4 generic definition
setGeneric(name = "get_stock_dataset",
           signature = c("stock_db"),
           def = get_stock_dataset <- function(stock_db,
                                               table_name,
                                               stock_cd_list = NULL,
                                               ...) {
             standardGeneric("get_stock_dataset")
           })


#' Fetch many datasets from stock_db
#'
#' Generic function to fetch many datasets from stock_db.
#'
#' @param stock_db   a stock database object to operate.
#' @param table_list a character vector of table names.
#'
#' @return A list of names of table fetched successfully.
#' @export
#'
#' @examples

# S3 generic definition
# fetch_table_dataset <- function(stock_db, table_list, ...) {
#   UseMethod("fetch_table_dataset")
# }

# S4 generic definition
setGeneric(name = "fetch_table_dataset",
           signature = c("stock_db"),
           def = fetch_table_dataset <- function(stock_db, table_list, ...) {
             standardGeneric("fetch_table_dataset")
           })

#' Get stock return timeseries from stock_db
#'
#' Generic function to get stock return timeseries from stock_db.
#'
#' @param stock_db    a stock database object to operate.
#' @param stock_cd_list a list of stock cd, default value of NULL means
#'     all stock data will be returned.
#' @param period_type date peroid for time series, e.g. "daily", "weekly",
#'                    "monthly", "annual", default value is "daily".
#' @param return_type a character string naming the method how the returns were computed.
#' @param use_stock_name use stock name as field name instead of stock cd, default is TRUE.
#' @param cumulated   calculate cumulated return, default value is FALSE.
#' @param output_type format of output result, e.g "timeSeries", "tibble".
#'
#'
#'
#' @return A timeseries of stock return.
#' @export
#'
#' @examples

# S3 generic definition
# get_stock_return <- function(stock_db,
#                              stock_cd_list = NULL,
#                              period_type = c("daily", "weekly", "monthly", "annual"),
#                              return_type = c("simple", "compound"),
#                              use_stock_name = TRUE,
#                              cumulated = FALSE,
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
                                     period_type = c("daily", "weekly", "monthly", "annual"),
                                     return_type = c("simple", "compound"),
                                     use_stock_name = TRUE,
                                     cumulated = FALSE,
                                     output_type = c("timeSeries", "tibble"),
                                     ...) {
      standardGeneric("get_stock_return")
    }
)

#' Get market return timesereis from stock_db
#'
#' Generic function to get stock return timeseries from stock_db.
#'
#' @param stock_db    a stock database object to operate.
#' @param period_type date peroid for time series, e.g. "daily", "weekly",
#'                    "monthly", "annual", default value is  "daily".
#' @param return_type a character string naming the method how the returns were computed.
#' @param cumulated   calculate cumulated return, default value is FALSE.
#' @param output_type format of output result, e.g "timeSeries", "tibble".
#'
#' @return A timeseries of market return.
#' @export
#'
#' @examples

# S3 generic definition
# get_market_return <- function(stock_db,
#                               period_type = c("daily", "weekly", "monthly", "annual"),
#                               return_type = c("simple", "compound"),
#                               cumulated = FALSE,
#                               output_type = c("timeSeries", "tibble"),
#                               ...) {
#   UseMethod("get_market_return")
# }

# S4 generic definition
setGeneric(
  name = "get_market_return",
  signature = c("stock_db"),
  def = get_market_return <- function(stock_db,
                                      period_type = c("daily", "weekly", "monthly", "annual"),
                                      return_type = c("simple", "compound"),
                                      cumulated = FALSE,
                                      output_type = c("timeSeries", "tibble"),
                                      ...) {
    standardGeneric("get_market_return")
  }
)

#' Get factor indicator timeseries from stock_db
#'
#' Generic function to get  factor indicator from stock_db.
#'
#' @param stock_db         a stock database object to operate.
#' @param factor_list      factor name list.
#'
#'
#' @return A timeseries of factor indicator
#' @export
#'
#' @examples
# S3 generic definition
# get_factor_indicator <- function(stock_db, factor_group,...){
#   UseMethod("get_factors")
# }
# S4 generic definition
setGeneric(
  name = "get_factor_indicator",
  signature = c("stock_db"),
  def = get_factor_indicator <- function(stock_db, factor_list,...) {
    standardGeneric("get_factor_indicator")
  }
)


#' Get factors Info from stock_db
#'
#' Generic function to get factors info from stock_db.
#'
#' @param stock_db         a stock database object to operate.
#' @param factor_groups    a character vector of factor groups, if NULL will
#' return all factors, otherwise return factors of matched fator groups,
#' by default NULL.
#'
#'
#' @return A dataframe of factors of matched factor groups if successfully,
#' otherwise NULL.
#' @export
#'
#' @examples
# S4 generic definition
setGeneric(
  name = "get_factors_info",
  signature = c("stock_db"),
  def = get_factors_info <- function(stock_db, factor_groups =NULL, ...) {
    standardGeneric("get_factors_info")
  }
)

# Non-generic functions for stock db operation ---------------------------------

#' Get assets return from market and stocks return
#'
#' Get assets return timeseries by combining marekt return anad stocks return
#'
#' @param benchmark_return   a timeseries of market index
#' @param stocks_return      a timeseries of a group stock stocks
#'
#' @return A timeseries of assets return
#' @export
#'
#' @examples

get_assets_return <- function(benchmark_return, stocks_return) {

  stopifnot(timeSeries::is.timeSeries(benchmark_return),
            timeSeries::is.timeSeries(stocks_return))


  start_date_market <- timeSeries::start(benchmark_return)
  end_date_market   <- timeSeries::end(benchmark_return)

  start_date_stocks <- timeSeries::start(stocks_return)
  end_date_stocks   <- timeSeries::end(stocks_return)

  # Used later start_date as start_date
  if (start_date_market >= start_date_stocks ) {
    start_date <- start_date_market
  } else {
    start_date <- start_date_stocks
  }

  # Used early end_date as end_date
  if (end_date_market <= end_date_stocks ) {
    end_date <- end_date_market
  } else {
    end_date <- end_date_stocks
  }

  # get data window between start_date and end_date
  benchmark_return <- timeSeries::window(benchmark_return, start_date, end_date)
  stocks_return    <- timeSeries::window(stocks_return, start_date, end_date)

  #Combine benchmark and stocks return
  assets_return <- merge(benchmark_return, stocks_return)
  colnames(assets_return) <- stringr::str_replace(colnames(assets_return), "X", "")

  return(assets_return)

}


# Get a timeseries of stock data for specified stock from table datasets
get_stock_field_dataset <- function(ds_source.df,
                                    stock_cd,
                                    target_field,
                                    stkcd_field = "stkcd",
                                    date_field="trdmnt",
                                    tseries_type = c("timeSeries", "xts"),
                                    debug = FALSE) {

  # Validate param
  if (is.null(ds_source.df) || missing(stock_cd)
      || missing(target_field) || missing(date_field)) {
    stop("ds_source.df, stock_cd, target_field, date_field  mustn't be null")
  }

  # Check whether the datafields existed
  field_list <- c(stkcd_field, target_field, date_field)
  if (!all(field_list %in% names(ds_source.df))) {
    error_fields <- NULL
    for (field_name in field_list) {
      if (!field_name %in% names(ds_source.df)) {
        error_fields <- ifelse(is.null(error_fields),
                               field_name,
                               paste(error_fields, field_name, sep = ","))
      }
    }

    error_msg <- sprintf("%s dosen't exist in dataset ", error_fields)
    stop(error_msg)
  }

  tseries_type <- match.arg(tseries_type)
  if (is.null(tseries_type)) {
    warning("teries_type should be timeSeries or xts, set as timeSeries by default")
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
  result_ts = NULL
  if (tseries_type == "timeSeries") {
    # timeSeries data series
    ds_name <- sprintf("ds_%s_%s.fts", stock_cd, target_field)
    result_ts <- timeSeries::timeSeries(ds_stock_data.df[target_field],
                                        zoo::as.Date(zoo::as.yearmon(
                                          ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stock_cd

  } else {
    # xts data series
    ds_name <- sprintf("ds_%s_%s.xts", stock_cd, target_field)
    result_ts <- xts::xts(ds_stock_data.df[target_field],
                          order.by = zoo::as.Date(
                            zoo::as.yearmon(ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stock_cd
  }

  # Sort result
  result_ts <- sort(result_ts)

  # Return the result timeseries of stock
  if (debug) {

    # keep the result_ts in GlobalEnv for debug check
    assign(ds_name, result_ts, pos = .GlobalEnv)
    return(get(ds_name))

  }else {

    # reutrn the result_as as ususal
    return(result_ts)
  }
}

# Get several timeseries of stocks data for multiple stocks from peroidic dataset
fetch_stock_field_dataset <- function(ds_source.df,
                                      stock_cd_list,
                                replaceNA = c("keep", "zeros", "mean", "median"),
                                      ...) {


  # Validate params
  if (is.null(ds_source.df) || missing(stock_cd_list) ) {
    stop("ds_source.df, stock_cd_list mustn't be null")
  }

  stopifnot(length(stock_cd_list) != 0, length(ds_source.df) != 0)

  # Coerce stock code to 6 digit of characters
  if (!is.character(stock_cd_list)) {
      stock_cd_list <- stringr::str_pad(stock_cd_list, width = 6, pad = "0")
      msg <- "Coerce stock cd to character of 6 digits if it were number"
      warnings(msg)
  }

  ds_result = NULL
  for (the_stock_cd in stock_cd_list) {

    # get stock data for specified stock
    ds_stock_data <- get_stock_field_dataset(ds_source.df = ds_source.df,
                                             stock_cd = the_stock_cd, ...)

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
  na_type = match.arg(replaceNA)
  if (na_type != "keep") {
    ds_nona_result <- timeSeries::substituteNA(ds_result, type = na_type )
    if (xts::is.xts(ds_result)) {
      ds_result <- xts::as.xts(ds_nona_result)
    }
    else{
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
#' @param x    a object containg code/name infomation
#' @param code a code or a vector of codes to be translated
#' @param ...  other arguments to be provided to underlyling functions
#'
#' @return     a name or a vector of names
#' @export
#'
#' @examples

# S3 generic definition
# code2name <- function(x, code, ...) {
#   UseMethod("code2name")
# }

# S4 generic definition
setGeneric(name = "code2name",
           signature = c("x"),
           def = code2name <- function (x, code, ...) {
             standardGeneric("code2name")
           })


#' Translate name into code
#'
#' Generic function to translate name into code
#'
#' @param x     a object containg code/name infomation
#' @param name  a name or a vector of names to be translated
#' @param ...   other arguments to be provided to underlyling functions
#'
#' @return      a code or a vector of codes
#' @export
#'
#' @examples

# S3 generic definition
# name2code <- function(x, name, ...) {
#   UseMethod("name2code")
# }

# S4 generic definition
setGeneric(name = "name2code",
           signature = c("x"),
           def = name2code <- function (x, name, ...) {
             standardGeneric("name2code")
           })


