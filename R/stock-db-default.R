
# default functions for S3 generic functions of stock db ------------------------------


# default function of open stock database
open_stock_db.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# default function to close stock database
close_stock_db.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# default function to init param of stock db
init_stock_db.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# default function to list all datasets of stck_db
list_stock_tables.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# default function to get one dataset from stock_db
get_table_dataset.default <- function(stock_db, table_name, quietly = FALSE) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}


# default function to fetch many datasets from stock_db
fetch_table_dataset.default <- function(stock_db, table_list) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# default function to create stock_field_list
stock_field_list.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# default function to create stock_name_list
stock_name_list.default <- function(stock_db) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

# default function to translate code to name
code2name.default <- function(x, code, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}

#default function to translate name to code
name2code.default <- function(x, name, ...) {
  stopifnot(!is.null(stock_db))
  stop("Unknow class")
}
