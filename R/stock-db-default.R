
# Default funcations for S3 generic functions of stock db ------------------------------


# Default funcation of open stock database
open_stock_db.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# Default funcation to close stock database
close_stock_db.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default funcation to init param of stock db
init_stock_db.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# Default funcation to list all datasets of stck_db
list_stock_tables.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# Default funcation to get one dataset from stock_db
get_table_dataset.default <- function(stock_db, table_name, quietly = FALSE) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# Default funcation to fetch many datasets from stock_db
fetch_table_dataset.default <- function(stock_db, table_list) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function to get stock return timeseries from stock_db
get_stock_return.default <- function(stock_db, stock_cd_list = NULL,
                             period_type = c("daily", "weekly", "monthly", "annual"),
                             return_type = c("simple", "compound"),
                             use_stock_name = TRUE,
                             cumulated = FALSE) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default function to get market return timeseries from stock_db
get_market_return.default <- function(stock_db,
                              period_type = c("daily", "weekly", "monthly", "annual"),
                              return_type = c("simple", "compound"),
                              cumulated = FALSE) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default funcation to get factor indicator timeseries for stock_db
get_factor_indicator.default <- function(stock_db, factor){
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# Default funcation to translate code to name
code2name.default <- function(x, code, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

#Default funcation to translate name to code
name2code.default <- function(x, name, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}
