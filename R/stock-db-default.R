
# Default functions for S3 generic functions of stock db ------------------------------


# Default function of open stock database
open_stock_db.default <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - close stock database
close_stock_db.default <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get path of profile of stock_db
get_profile.default <- function(stock_db, profile_name, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - initiate params of stock db
init_stock_db.default <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# Default function - list all datasets of stock_db
list_stock_tables.default <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# Default function - get one dataset from stock_db
get_table_dataset.default <- function(stock_db, table_name, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get a dataset of a list of stock_cd from a table in stock_db
get_stock_dataset.default <- function(stock_db,
                                      table_name,
                                      stock_cd_list = NULL,
                                      ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get stock info from stock_db
get_stock_info.default <- function(stock_db,
                                   stock_cd_list = NULL,
                                   ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get industry info from stock_db
get_industry_info.default <- function(stock_db,
                                      industry_codes = NULL,
                                      ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}



# Default function - get stock return timeseries from stock_db
get_stock_return.default <- function(stock_db,
                                     stock_cd_list = NULL,
                                     period_type = c("day", "month", "year"),
                                     period_date = c("start", "end"),
                                     output_type = c("timeSeries", "tibble"),
                                     ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get market return timeseries from stock_db
get_market_return.default <- function(stock_db,
                                      period_type = c("day", "month", "year"),
                                      period_date = c("start", "end"),
                                      output_type = c("timeSeries", "tibble"),
                                      ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get financial report timeseries from stock_db
get_financial_report.default <- function(stock_db,
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
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get indicators from specified source in stock_db
get_indicators_from_source.default <- function(stock_db, indicator_source,
                                               indicator_codes = NULL,
                                               ouput_format = c("long", "wide"),
                                               ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - save indicators to specified source in stock_db
save_indicators_to_source.default <- function(stock_db, indicator_source,
                                              ts_indicators, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get indicators from stock_db
get_indicators.default <- function(stock_db, indicator_codes, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get indicators Info from stock_db
get_indicators_info.default <- function(stock_db, indicator_codes = NULL,
                                        ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get factors from stock_db
get_factors.default <- function(stock_db, factor_codes, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get factors Info from stock_db
get_factors_info.default <- function(stock_db, factor_codes = NULL,
                                     factor_groups = NULL, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get industry info of stocks from stock_db
get_stock_industry.default <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get stock info of special treatment from stock_db
get_spt_stocks.default <- function(stock_db, ...) {

}

# Default function - get riskfree rate from stock_db
get_riskfree_rate.default <- function(stock_db,
                                      period = c(
                                        "day", "month",
                                        "quarter", "year"
                                      ), ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - get Path of Data Directory from stock_db
dir_path_db.default <- function(stock_db,
                                dir_id = c(
                                  "DIR_DB_DATA",
                                  "DIR_DB_DATA_SOURCE",
                                  "DIR_DB_DATA_ORIGIN",
                                  "DIR_DB_DATA_LOG",
                                  "DIR_DB_DATA_INDICATOR"
                                ),
                                force = TRUE, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - translate code to name
code2name.default <- function(x, code, exact_match = TRUE, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function - translate name to code
name2code.default <- function(x, name, exact_match = TRUE, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}
