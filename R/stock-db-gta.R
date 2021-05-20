
# CONSTANT DEFINATION -----------------------------------------------------

# profile variable defination
# Since R devtools doesn't suport loading Chinese characters in script file, we
# have to use reference name of varible in profile for refering Chinese table
# name in DB
.GTA_PROFILE_FILE <- "gta_profile.xlsx"


.GTA_TABLE_NAME_LIST <- list(
  "gta_fieldname_list", # name for gta_fieldname_list

  "TRD_Co", # name for TRD_Co_公司基本情况

  "TRD_DALYR", # name for TRD_Dalyr_日个股回报率
  "TRD_WEEK", # name for TRD_Week_周个股回报率
  "TRD_MNTH", # name for TRD_Mnth_月个股回报率
  "TRD_YEAR", # name for TRD_Year_年个股回报率

  "TRD_CNDALYM", # name for TRD_Cndalym_综合市场日度回报
  "TRD_WEEKCM", # name for TRD_Weekcm_综合市场周度回报
  "TRD_CNMONT", # name for TRD_Cnmont_综合市场月度回报
  "TRD_YEARCM", # name for TRD_Yearcm_综合市场年度回报

  "FS_COMBAS", # name for FS_Combas_资产负债表
  "FS_COMINS", # name for FS_Comins_损益表
  "FS_COMINS_TTM", # name for FS_Comins_ttm_损益表
  "FS_COMSDFD", # name for FS_Comscfd_现金流量表_直接
  "FS_COMSDFD_TTM", # name for FS_Comscfd_ttm_现金流量表_直接
  "FS_COMSCFI", # name for FS_Comscfi_现金流量表_间接
  "FS_COMSCFI_TTM", # name for FS_Comscfi_ttm_现金流量表_间接

  "TRD_NRRATE", # name for TRD_Nrrate_利率

  "TRD_STOCK_INDUSTRY", # name for table with stock industry info
  "SPT_TRDCHG" # name for SPT_Trdchg_特殊处理
)



# Class definition of gta_db class -----------------------------------------


#' @include stock-db.R
#' @include code-name-list.R

# Definition of gta_db class
setRefClass("gta_db",
  fields = list(
    table_list = "list",
    stock_field_list = "code_name_listOrNull",
    stock_name_list = "code_name_listOrNull",
    industry_name_list = "code_name_listOrNull",
    factor_name_list = "code_name_listOrNull",
    indicator_name_list = "code_name_listOrNull"
  ),
  contains = "stock_db"
)


#' Creator of gta_db class
#'
#' Create a object for gta_db class
#'
#' @param dsn odbc dsn string
#' @return an object of gta_db class
#' @export
#'
#' @examples
#' \dontrun{
#' gta_db <- gta_db()
#' }
gta_db <- function(dsn = "GTA_SQLData") {
  stopifnot(!is.null(dsn))

  # use envir for class member storage - created only for s3 class
  # class_env <- new.env()
  # class_env$dsn <- dsn
  # class_env$connection <- NULL

  # create object of S3 Class
  # stock_db <- structure(class_env, class = c("gta_db", "stock_db"))

  # create object of S4 Class
  stock_db <- new("gta_db", dsn = dsn, connection = NULL)

  return(stock_db)
}

# Generic functions implemetation by gta_db class ------------------------

# Open the stock database
# Method definition for s3 generic
# @describeIn open_stock_db Open a database of gta_db class
# @export
open_stock_db.gta_db <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  success <- TRUE

  # Use data-engine of RODBC
  # con_stock_db <- tryCatch( RODBC::odbcConnect(dsn = stock_db$dsn),
  #                           error = function(e) e)

  # Use data-engine of DBI:odbc
  con_stock_db <- tryCatch(DBI::dbConnect(odbc::odbc(),
    dsn = stock_db$dsn,
    encoding = "CP936"
  ),
  error = function(e) e
  )


  if (inherits(con_stock_db, "error")) {
    msg <- conditionMessage(con_stock_db)
    success <- FALSE
  } else {
    msg <- sprintf("Connect data source of %s successfully.", stock_db$dsn)
    stock_db$connection <- con_stock_db
    success <- TRUE
  }

  rlang::inform(msg)

  return(invisible(success))
}
# Method definition for s4 generic
#' @describeIn open_stock_db Open a database of gta_db class
#' @export
setMethod(
  "open_stock_db",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    open_stock_db.gta_db(stock_db, ...)
  }
)

# Close the stock database
# Method definition for s3 generic
# @describeIn close_stock_db Close a database of gta_db class
# @export
close_stock_db.gta_db <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  success <- TRUE

  if (!is.null(stock_db$connection)) {

    # Use data-engine of RODBC
    # success <- tryCatch(RODBC::odbcClose(stock_db$connection),
    #                     error = function(e) e)

    # Use data-engine of DBI:odbc
    success <- tryCatch(DBI::dbDisconnect(stock_db$connection),
      error = function(e) e
    )

    if (inherits(success, "error")) {

      # fail to close the connect
      msg <- sprintf("Fail to close the connection of %s.", stock_db$dsn)
      success <- FALSE
    } else {

      # close the connection succesfully
      msg <- sprintf("Close the connection of %s successfully.", stock_db$dsn)
      stock_db$connection <- NULL
      success <- TRUE
    }

    rlang::inform(msg)
  }

  return(invisible(success))
}
# Method definition for s4 generic
#' @describeIn close_stock_db Close a database of gta_db class
#' @export
setMethod(
  "close_stock_db",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    close_stock_db.gta_db(stock_db, ...)
  }
)



# Get profile of stock_db
# Method definition for s3 generic
# @describeIn get_profile get profile of a database of gta_db class
# @export
get_profile.gta_db <- function(stock_db, profile_name = .GTA_PROFILE_FILE,
                               ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # set default profile_name
  if (missing(profile_name)) {
    profile_name <- .GTA_PROFILE_FILE
  }

  gta_profile <- get_profile_path(profile_name)

  return(gta_profile)
}

# Method definition for s4 generic
#' @describeIn get_profile get profile of a database of gta_db class
#' @export
setMethod(
  "get_profile",
  signature(stock_db = "gta_db"),
  function(stock_db, profile_name, ...) {
    get_profile.gta_db(stock_db, profile_name, ...)
  }
)

# Init param of stock db
# Method definition for s3 generic
# @describeIn init_stock_db Init param of database of gta_db class
# @export
init_stock_db.gta_db <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  success <- TRUE

  # set up table name mapping for referece
  gta_profile_name <- get_profile(stock_db)

  # set up table name list
  stock_db$table_list <- list()
  for (i in seq_along(.GTA_TABLE_NAME_LIST)) {
    table_name_id <- .GTA_TABLE_NAME_LIST[[i]]
    table_name_value <- profile_get_varible_setting(gta_profile_name, table_name_id)
    if (is.null(table_name_value)) {
      msg <- sprintf(
        "Fail to load %s from %.", table_name_id,
        gta_profile_name
      )
      rlang::abort(msg)
      success <- FALSE
    } else {
      stock_db$table_list[table_name_id] <- table_name_value
    }
  }

  # set up field_name list
  if (success) {
    stock_db$stock_field_list <- stock_field_list.gta_db(stock_db)
    if (is.null(stock_db$stock_field_list)) {
      rlang::warn("Fail to set up field_name_list")
      success <- FALSE
    }
  }

  # set up stock_name list
  if (success) {
    stock_db$stock_name_list <- stock_name_list.gta_db(stock_db)
    if (is.null(stock_db$stock_name_list)) {
      rlang::warn("Fail to set up stock_name_list")
      success <- FALSE
    }
  }

  # set up industry_name list
  if (success) {
    stock_db$industry_name_list <- industry_name_list.gta_db(stock_db)
    if (is.null(stock_db$industry_name_list)) {
      rlang::warn("Fail to set up industry_name_list")
      success <- FALSE
    }
  }

  # set up factor_name list
  if (success) {
    stock_db$factor_name_list <- factor_name_list.gta_db(stock_db)
    if (is.null(stock_db$factor_name_list)) {
      rlang::warn("Fail to set up factor_name_list")
      success <- FALSE
    }
  }

  # set up indicator_name list
  if (success) {
    stock_db$indicator_name_list <- indicator_name_list.gta_db(stock_db)
    if (is.null(stock_db$indicator_name_list)) {
      rlang::warn("Fail to set up indicator_name_list")
      success <- FALSE
    }
  }



  return(invisible(success))
}
# Method definition for s4 generic
#' @describeIn init_stock_db Init param of database of gta_db class
#' @export
setMethod(
  "init_stock_db",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    init_stock_db.gta_db(stock_db, ...)
  }
)




# List all tables of stck_db
# Method definition for s3 generic
# @describeIn list_stock_tables List names of tables in database of gta_db class
# @export
list_stock_tables.gta_db <- function(stock_db, ...) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # Use data-engine of RODBC
  # db_tables <- RODBC::sqlTables(stock_db$connection, schema = "dbo")
  # db_tables <- db_tables[db_tables$TABLE_SCHEM == "dbo", "TABLE_NAME" ]

  # Use data-engine of DBI:odbc
  db_tables <- DBI::dbListTables(stock_db$connection, schema_name = "dbo")
  db_tables <- iconv(db_tables, from = "UTF-8", to = "CP936")

  return(db_tables)
}
# Method definition for s4 generic
#' @describeIn list_stock_tables List names of tables in database of gta_db class
#' @export
setMethod(
  "list_stock_tables",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    list_stock_tables.gta_db(stock_db, ...)
  }
)


# Translate name into code for field or stock
# Method definition for s3 generic
# @param type  A character of conversion type, e.g, "stock", "field", "industry".
#  Default "stock" means to covert stock name to stock code.
# @describeIn name2code Translate name into code in a database of gta_db class
# @export
name2code.gta_db <- function(stock_db, name, exact_match = TRUE,
                             type = c(
                               "stock", "field", "industry",
                               "factor", "indicator"
                             ), ...) {
  stopifnot(inherits(stock_db, "gta_db"), !missing(name))

  target_type <- match.arg(type)
  code <- switch(target_type,
    field = name2code(stock_db$stock_field_list,
      name = name,
      exact_match = exact_match
    ),
    stock = name2code(stock_db$stock_name_list,
      name = name,
      exact_match = exact_match
    ),
    industry = name2code(stock_db$industry_name_list,
      name = name,
      exact_match = exact_match
    ),
    factor = name2code(stock_db$factor_name_list,
      name = name,
      exact_match = exact_match
    ),
    indicator = name2code(stock_db$indicator_name_list,
      name = name,
      exact_match = exact_match
    )
  )

  return(code)
}
# Method definition for s4 generic
# Translate name into code for field or stock
#' @param type  A character of conversion type, e.g, "stock", "field", "industry".
#'  Default "stock" means to covert stock name to stock code.
#' @describeIn name2code Translate name into code in a database of gta_db class
#' @export
setMethod(
  "name2code",
  signature(x = "gta_db"),
  function(x, name, exact_match = TRUE,
           type = c(
             "stock", "field", "industry",
             "factor", "indicator"
           ), ...) {
    name2code.gta_db(stock_db = x, name, exact_match, type, ...)
  }
)

# Translate code into name for field or stock
# Method definition for s3 generic
# @param type  A character of conversion type, e.g, "stock", "field", "industry".
#  Default "stock" means to covert stock code to stock name.
# @describeIn code2name Translate code into name in a database of gta_db class
# @export
code2name.gta_db <- function(stock_db, code, exact_match = TRUE,
                             type = c(
                               "stock", "field", "industry",
                               "factor", "indicator"
                             ), ...) {
  stopifnot(inherits(stock_db, "gta_db"), !missing(code))

  target_type <- match.arg(type)
  name <- switch(target_type,
    field = code2name(stock_db$stock_field_list,
      code = code,
      exact_match = exact_match
    ),
    stock = code2name(stock_db$stock_name_list,
      code = code,
      exact_match = exact_match
    ),
    industry = code2name(stock_db$industry_name_list,
      code = code,
      exact_match = exact_match
    ),
    factor = code2name(stock_db$factor_name_list,
      code = code,
      exact_match = exact_match
    ),
    indicator = code2name(stock_db$indicator_name_list,
      code = code,
      exact_match = exact_match
    )
  )

  return(name)
}
# Method definition for s4 generic
#' @param type  A character of conversion type, e.g, "stock", "field", "industry".
#'  Default "stock" means to covert stock code to stock name.
#' @describeIn code2name Translate code into name in a database of gta_db class
#' @export
setMethod(
  "code2name",
  signature(x = "gta_db"),
  function(x, code, exact_match = TRUE,
           type = c(
             "stock", "field", "industry",
             "factor", "indicator"
           ), ...) {
    code2name.gta_db(stock_db = x, code, exact_match, type, ...)
  }
)
# Get a dataset from a table in stock_db
# Method definition for s3 generic
# @describeIn get_table_dataset get a table dataset from a database of gta_db class
# @export
get_table_dataset.gta_db <- function(stock_db, table_name, ...) {

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name)) {
    rlang::abort("Table name must be character string")
  }

  # Get table data  from database

  # Use data-engine of RODBC
  # ds_result <- tryCatch(RODBC::sqlFetch(stock_db$connection, table_name,
  #                                       stringsAsFactors = FALSE),
  #                        error = function(e) e)

  # Use data-engine of DBI:odbc
  table_name <- iconv(table_name, from = "UTF-8", to = "CP936")
  ds_result <- tryCatch(DBI::dbReadTable(stock_db$connection, table_name),
    error = function(e) e
  )
  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf("Get data from %s successfully.", table_name)
    colnames(ds_result) <- tolower(colnames(ds_result))
  }
  rlang::inform(msg)

  # Coerce stock code to 6 digit of characters
  if (!is.null(ds_result)) {
    field_names <- names(ds_result)
    stkcd_names <- field_names[field_names %in% c("stkcd")]
    if (length(stkcd_names) != 0) {
      if (!is.character(ds_result[, stkcd_names])) {
        ds_result[, stkcd_names] <- stringr::str_pad(ds_result[, stkcd_names],
          width = 6, pad = "0"
        )
        msg <- "Coerce stock cd to character of 6 digits if it were number."
        warnings(msg)
      }
    }
  }

  return(invisible(ds_result))
}
# Method definition for s4 generic
#' @describeIn get_table_dataset get a table dataset from a database of gta_db class
#' @export
setMethod(
  "get_table_dataset",
  signature(stock_db = "gta_db"),
  function(stock_db, table_name, ...) {
    get_table_dataset.gta_db(stock_db, table_name, ...)
  }
)

# Get a dataset of a list of stock_cd from table in stock
# Method definition for s3 generic
# @describeIn get_stock_dataset get a dataset of a list of stock_cd from table
#  in a database of gta_db class
# @export
get_stock_dataset.gta_db <- function(stock_db,
                                     table_name,
                                     stock_cd_list = NULL,
                                     ...) {

  # Validate param
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name)) {
    rlang::abort("Table name must be character string")
  }

  # if (missing(stock_cd_list) || !is.character(stock_cd_list) ) {
  #    rlang::abort("stock_cd_list must be character string")
  # }

  # Build sql command for data query
  stock_cd_list_str <- NULL
  if (!is.null(stock_cd_list)) {

    # Coerce stock code to 6 digit of characters
    if (!is.character(stock_cd_list)) {
      stock_cd_list <- stringr::str_pad(stock_cd_list, width = 6, pad = "0")
      msg <- "Coerce stock cd to character of 6 digits if it were numeric."
      warnings(msg)
    }

    # build stock_cd list string
    for (stock_cd in stock_cd_list) {
      if (is.null(stock_cd_list_str)) {
        stock_cd_list_str <- stock_cd
      } else {
        stock_cd_list_str <- paste(stock_cd_list_str, stock_cd, sep = ",")
      }
    }
  }

  if (length(stock_cd_list_str) != 0) {
    sql_cmd <- sprintf(
      "select * from %s where Stkcd in (%s)",
      table_name, stock_cd_list_str
    )
  } else {
    sql_cmd <- sprintf("select * from %s", table_name)
  }

  # Select stock data from table

  # Use data-engine of RODBC
  # ds_result <- tryCatch(RODBC::sqlQuery(stock_db$connection, sql_cmd,
  #                                       stringsAsFactors = FALSE),
  #                       error = function(e) e)

  # Use data-engine of DBI:odbc
  sql_cmd <- iconv(sql_cmd, from = "UTF-8", to = "CP936")
  ds_result <- tryCatch(DBI::dbGetQuery(stock_db$connection, sql_cmd),
    error = function(e) e
  )

  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf(
      "Get stock data of(%s) from %s successfully.", stock_cd_list_str,
      table_name
    )
    colnames(ds_result) <- tolower(colnames(ds_result))
  }
  rlang::inform(msg)

  # Coerce stock code to 6 digit of characters
  if (!is.null(ds_result)) {
    field_names <- names(ds_result)
    stkcd_names <- field_names[field_names %in% c("stkcd")]
    if (length(stkcd_names) != 0) {
      if (!is.character(ds_result[, stkcd_names])) {
        ds_result[, stkcd_names] <- stringr::str_pad(ds_result[, stkcd_names],
          width = 6, pad = "0"
        )
        msg <- "Coerce stock cd to character of 6 digits if it were number."
        warnings(msg)
      }
    }
  }


  return(invisible(ds_result))
}
# Method definition for s4 generic
#' @describeIn get_stock_dataset get a dataset of a list of stock_cd from table
#' in a database of gta_db class
#' @export
setMethod(
  "get_stock_dataset",
  signature(stock_db = "gta_db"),
  function(stock_db, table_name, stock_cd_list = NULL, ...) {
    get_stock_dataset.gta_db(stock_db, table_name, stock_cd_list, ...)
  }
)

# Fetch many datasets from stock_db
# Method definition for s3 generic
# @describeIn fetch_table_dataset get many datasets from a database of gta_db class
# @export
fetch_table_dataset.gta_db <- function(stock_db, table_list, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_list) || length(table_list) == 0) {
    rlang::abort("Table_list must contain one table at least")
  }

  # get datasets from stock db
  result_table_list <- list(length(table_list))
  for (table_index in seq_along(table_list)) {
    the_table <- table_list[table_index]
    # get stock data for specified stock
    ds_result <- get_table_dataset.gta_db(stock_db,
      table_name = the_table
    )

    # keep the result_ts in GlobalEnv for debug check
    if (!is.null(ds_result)) {

      # get table successfully
      ds_name <- sprintf("ds_%s.df", the_table)
      assign(ds_name, ds_result, envir = .GlobalEnv)
      # assign(ds_name, ds_result, envir = parent.frame())
      result_table_list[table_index] <- ds_name
    } else {

      # fail to get table
      result_table_list[table_index] <- NULL
    }
  }

  return(result_table_list)
}
# Method definition for s4 generic
#' @describeIn fetch_table_dataset get many datasets from a database of gta_db class
#' @export
setMethod(
  "fetch_table_dataset",
  signature(stock_db = "gta_db"),
  function(stock_db, table_list, ...) {
    fetch_table_dataset.gta_db(stock_db, table_list, ...)
  }
)

# Get stock info from stock_db
# Method definition for s3 generic
# @describeIn get_stock_info get stock info from a database of gta_db class
# @export
get_stock_info.gta_db <- function(stock_db,
                                  stock_cd_list = NULL, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }
  if (!is.null(stock_cd_list)) assertive::assert_is_character(stock_cd_list)


  # get stock info from database
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_stock_info_raw <- get_table_dataset(stock_db,
    table_name = table_name
  )
  ds_stock_info_raw <- tibble::as_tibble(ds_stock_info_raw)

  # filter stock_info by stkcds
  if (!is.null(stock_cd_list)) {
    ds_stock_info_raw <- ds_stock_info_raw %>%
      dplyr::filter(stkcd %in% stock_cd_list)
  }

  # covert marekt_type
  market_type <- c("SH_A", "SH_B", "SZ_A", "SZ_B", "SZ_G")
  markeytype <- c(1, 2, 4, 8, 16)
  names(market_type) <- markeytype
  ds_stock_info_raw <- ds_stock_info_raw %>%
    dplyr::mutate(market_type = unname(market_type[as.character(markettype)]))

  # build output datasets
  ds_stock_info <- ds_stock_info_raw %>%
    dplyr::select(stkcd,
      stkname = stknme,
      indcd = nnindcd, indname = nnindnme,
      establish_date = estbdt,
      list_date = listdt,
      market_type
    )

  return(ds_stock_info)
}
# Method definition for s4 generic
#' @describeIn get_stock_info get stock info from a database of gta_db class
#' @export
setMethod(
  "get_stock_info",
  signature(stock_db = "gta_db"),
  function(stock_db, stock_cd_list, ...) {
    get_stock_info.gta_db(
      stock_db, stock_cd_list, ...
    )
  }
)

# Get industry info from stock_db
# Method definition for s3 generic
# @describeIn get_industry_info get industry info from a database of gta_db class
# @export
get_industry_info.gta_db <- function(stock_db,
                                     industry_codes = NULL, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }
  if (!is.null(industry_codes)) assertive::assert_is_character(industry_codes)

  # Get industry info
  ds_industry_info <- tibble::tibble(
    indcd = stock_db$industry_name_list$code,
    indname = stock_db$industry_name_list$name
  )

  # Filter industry_info by industry_coeds
  if (!is.null(industry_codes)) {
    ds_industry_info <- ds_industry_info %>%
      dplyr::filter(.data[["indcd"]] %in% industry_codes)
  }

  ds_industry_info <- ds_industry_info %>%
    dplyr::arrange(.data[["indcd"]])

  return(ds_industry_info)
}
# Method definition for s4 generic
#' @describeIn get_industry_info get industry info from a database of gta_db class
#' @export
setMethod(
  "get_industry_info",
  signature(stock_db = "gta_db"),
  function(stock_db, industry_codes, ...) {
    get_industry_info.gta_db(
      stock_db, industry_codes, ...
    )
  }
)


# Get stock return timeseries from stock_db
# Method definition for s3 generic
# @describeIn get_stock_return get stock return timeseries from a database of gta_db class
# @export
get_stock_return.gta_db <- function(stock_db, stock_cd_list = NULL,
                                    period_type = c("day", "month", "year"),
                                    period_date = c("start", "end"),
                                    output_type = c("timeSeries", "tibble"),
                                    ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # get stock return dataset
  field_stkcd <- rlang::quo(stkcd)
  period_type <- match.arg(period_type)
  switch(
    period_type,
    "day" = {
      table_name <- stock_db$table_list[["TRD_DALYR"]]
      field_date <- rlang::quo(trddt)
      field_return <- rlang::quo(dretwd)
      date_format <- "ymd"
      period_unit <- "day"
    },
    "month" = {
      table_name <- stock_db$table_list[["TRD_MNTH"]]
      field_date <- rlang::quo(trdmnt)
      field_return <- rlang::quo(mretwd)
      date_format <- "ym"
      period_unit <- "month"
    },
    "year" = {
      table_name <- stock_db$table_list[["TRD_YEAR"]]
      field_date <- rlang::quo(trdynt)
      field_return <- rlang::quo(yretwd)
      date_format <- "y"
      period_unit <- "year"
    }
  )

  # Coerce stock code to 6 digit of characters
  if (!is.null(stock_cd_list) && !is.character(stock_cd_list)) {
    stock_cd_list <- stringr::str_pad(stock_cd_list, width = 6, pad = "0")
    msg <- "Coerce stock cd to character of 6 digits if it were number."
    warnings(msg)
  }

  # Warning: ds_return is simple return in database by default !!
  ds_return <- get_stock_dataset.gta_db(stock_db, table_name, stock_cd_list)
  if (!is.null(ds_return)) {
    ds_return <- ds_return %>%
      tibble::as_tibble() %>%
      dplyr::select(
        date = !!field_date, stkcd = !!field_stkcd,
        return = !!field_return
      )

    msg <- sprintf(
      "Get stock return: return table: %s , return field: %s, date field: %s.",
      table_name, rlang::quo_text(field_return), rlang::quo_text(field_date)
    )
    rlang::inform(msg)
  } else {
    success <- FALSE
  }

  # Set date format of date field
  if (success) {

    # Change date format: datetime --> date
    origin_date <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    origin_date <- lubridate::as_date(origin_date)

    # Set date of period(start date/end date of the period)
    period_date <- match.arg(period_date)
    switch(period_date,
      "start" = {
        # first day of period = floor_date
        floor_date <- lubridate::floor_date(origin_date,
          unit = period_unit
        )
        ds_return$date <- floor_date
      },
      "end" = {
        # last day of period = ceiling_date -1
        ceiling_date <- lubridate::ceiling_date(origin_date,
          unit = period_unit,
          change_on_boundary = TRUE
        )
        ds_return$date <- ceiling_date - 1
      }
    )
  }

  # Build simple return results
  ts_return <- NULL
  if (success) {
    output_type <- match.arg(output_type)
    switch(output_type,
      tibble = {
        # Sort by date in each stock
        ts_return <- ds_return %>%
          dplyr::group_by(.data$stkcd) %>%
          dplyr::arrange(.data$date, .by_group = TRUE) %>%
          dplyr::ungroup()
      },
      timeSeries = {

        # spread stkcd as columns
        ds_return <- ds_return %>%
          tidyr::pivot_wider(names_from = stkcd, values_from = return)

        # arrange colname as the order of stock_cd_list
        if (!is.null(stock_cd_list) && length(stock_cd_list) != 0) {
          ds_return <- dplyr::select(ds_return, date, stock_cd_list)
        }

        # Build time series
        charvec <- ds_return$date
        ts_return.fts <- timeSeries::timeSeries(ds_return[, -1], charvec)
        ts_return.fts <- sort(ts_return.fts) # Sort by date

        ts_return <- ts_return.fts
      }
    )
  }

  return(ts_return)
}
# Method definition for s4 generic
#' @describeIn get_stock_return get stock return timeseries from a database of gta_db class
#' @export
setMethod(
  "get_stock_return",
  signature(stock_db = "gta_db"),
  function(stock_db,
           stock_cd_list = NULL,
           period_type = c("day", "month", "year"),
           period_date = c("start", "end"),
           output_type = c(
             "timeSeries",
             "tibble"
           ),
           ...) {
    get_stock_return.gta_db(
      stock_db = stock_db,
      stock_cd_list = stock_cd_list,
      period_type = period_type,
      period_date = period_date,
      output_type = output_type,
      ...
    )
  }
)

# Get market return timeseries from stock_db
# Method definition for s3 generic
# @describeIn get_market_return get market return timeseries from a database of gta_db class
# @export
get_market_return.gta_db <- function(stock_db,
                                     period_type = c("day", "month", "year"),
                                     period_date = c("start", "end"),
                                     output_type = c("timeSeries", "tibble"),
                                     ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # get market return dataset
  field_markettype <- rlang::quo(markettype)
  period_type <- match.arg(period_type)
  switch(
    period_type,
    "day" = {
      table_name <- stock_db$table_list[["TRD_CNDALYM"]]
      field_date <- rlang::quo(trddt)
      field_return <- rlang::quo(cdretwdtl)
      date_format <- "ymd"
      period_unit <- "day"
    },
    "month" = {
      table_name <- stock_db$table_list[["TRD_CNMONT"]]
      field_date <- rlang::quo(trdmnt)
      field_return <- rlang::quo(cmretwdtl)
      date_format <- "ym"
      period_unit <- "month"
    },
    "year" = {
      table_name <- stock_db$table_list[["TRD_YEARCM"]]
      field_date <- rlang::quo(trdynt)
      field_return <- rlang::quo(cyretwdtl)
      date_format <- "y"
      period_unit <- "year"
    }
  )

  # Warning: ds_return is simple return in database by default !!
  ds_return <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_return)) {
    ds_return <- ds_return %>%
      tibble::as_tibble() %>%
      dplyr::filter(!!field_markettype == 21) %>%
      dplyr::select(date = !!field_date, market_index = !!field_return)

    msg <- sprintf(
      "Get return data from return table: %s , return field: %s, date field: %s.",
      table_name, rlang::quo_text(field_return), rlang::quo_text(field_date)
    )
    rlang::inform(msg)
  } else {
    success <- FALSE
  }

  # Set date format of date field
  if (success) {

    # Change date format: datetime --> date
    origin_date <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    origin_date <- lubridate::as_date(origin_date)

    # Set date of period(start date/end date of the period)
    period_date <- match.arg(period_date)
    switch(period_date,
      "start" = {
        # first day of period = floor_date
        floor_date <- lubridate::floor_date(origin_date,
          unit = period_unit
        )
        ds_return$date <- floor_date
      },
      "end" = {
        # last day of period = ceiling_date -1
        ceiling_date <- lubridate::ceiling_date(origin_date,
          unit = period_unit,
          change_on_boundary = TRUE
        )
        ds_return$date <- ceiling_date - 1
      }
    )
  }

  # Build simple return results
  ts_return <- NULL
  if (success) {
    output_type <- match.arg(output_type)
    switch(output_type,
      tibble = {
        ts_return <- ds_return %>%
          dplyr::arrange(.data$date)
      },
      timeSeries = {
        # Build time series
        charvec <- ds_return$date
        ts_return.fts <- timeSeries::timeSeries(ds_return[, -1], charvec)
        ts_return.fts <- sort(ts_return.fts) # Sort by date
        ts_return <- ts_return.fts
      }
    )
  }


  return(ts_return)
}
# Method definition for s4 generic
#' @describeIn get_market_return get market return timeseries from a database of gta_db class
#' @export
setMethod(
  "get_market_return",
  signature(stock_db = "gta_db"),
  function(stock_db,
           period_type = c("day", "month", "year"),
           period_date = c("start", "end"),
           output_type = c("timeSeries", "tibble"),
           ...) {
    get_market_return.gta_db(
      stock_db = stock_db,
      period_type = period_type,
      period_date = period_date,
      output_type = output_type,
      ...
    )
  }
)


# Get financial report timeseries from stock_db
# Method definition for s3 generic
# @describeIn get_financial_report get financial report timeseries from a database of gta_db class
# @export
get_financial_report.gta_db <- function(stock_db,
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

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_logical(consolidated)

  statement <- match.arg(statement)
  switch(statement,
    "balance_sheet" = {
      table_name <- stock_db$table_list[["FS_COMBAS"]]
    },
    "income" = {
      table_name <- stock_db$table_list[["FS_COMINS"]]
    },
    "cashflow_direct" = {
      table_name <- stock_db$table_list[["FS_COMSDFD"]]
    },
    "cashflow_indirect" = {
      table_name <- stock_db$table_list[["FS_COMSCFI"]]
    },
    "income_ttm" = {
      table_name <- stock_db$table_list[["FS_COMINS_TTM"]]
    },
    "cashflow_direct_ttm" = {
      table_name <- stock_db$table_list[["FS_COMSDFD_TTM"]]
    },
    "cashflow_indirect_ttm" = {
      table_name <- stock_db$table_list[["FS_COMSCFI_TTM"]]
    }
  )

  success <- TRUE
  ds_report <- NULL

  # get financial report from stock_db
  ds_report_raw <- get_stock_dataset(stock_db,
    table_name = table_name,
    stock_cd_list = stock_cd_list
  )
  if (!is.null(ds_report_raw)) {
    ds_report <- ds_report_raw %>%
      tibble::as_tibble()
  } else {
    success <- FALSE
  }


  # filter report by type
  if (success) {
    if (consolidated) {
      ds_report <- ds_report_raw %>%
        dplyr::filter(typrep == "A") %>%
        dplyr::select(-typrep)
    } else {
      ds_report <- ds_report_raw %>%
        dplyr::filter(typrep == "B") %>%
        dplyr::select(-typrep)
    }
  }

  # get report by period type
  period_type <- match.arg(period_type)
  if (success) {

    # filter out data at month of 3,6,9,12
    ds_report <- ds_report %>%
      dplyr::mutate(
        date = accper,
        month_index = lubridate::month(date)
      ) %>%
      dplyr::filter(month_index %in% c(3, 6, 9, 12)) %>%
      dplyr::select(-accper)

    # filter year or quarter report
    if (period_type == "year") {
      ds_report <- ds_report %>%
        dplyr::filter(month_index == 12)
    }

    ds_report <- ds_report %>%
      dplyr::select(-month_index)
  }


  # translate date into last day of period, add period field
  if (success) {
    ts_date <- as.Date(ds_report$date)

    # turn it into the last day of period
    dates_period <- guess_dates_period(ts_date)
    switch(dates_period,
      "day" = {
        period_unit <- "day"
      },
      "month" = {
        period_unit <- "month"
      },
      "quarter" = {
        period_unit <- "quarter"
      },
      "year" = {
        period_unit <- "year"
      },
      "unknown" = {
        period_unit <- "day"
      }
    )

    # change date to start/end date of period
    period_date <- match.arg(period_date)
    if (period_date == "start") {
      # first day of period
      ts_date <- lubridate::floor_date(ts_date, unit = period_unit)
    } else {
      # last day of period = ceiling_date -1
      ceiling_date <- lubridate::ceiling_date(ts_date,
        unit = period_unit,
        change_on_boundary = TRUE
      )
      ts_date <- ceiling_date - 1
    }

    ds_report <- dplyr::mutate(ds_report, date = ts_date)
  }


  # Build final result
  ts_report <- NULL
  if (success) {
    ts_report <- ds_report %>%
      dplyr::arrange(date, stkcd) %>%
      dplyr::select(date, stkcd, dplyr::everything())
  }

  return(ts_report)
}
# Method definition for s4 generic
#' @describeIn get_financial_report get financial report timeseries from a database of gta_db class
#' @export
setMethod(
  "get_financial_report",
  signature(stock_db = "gta_db"),
  function(stock_db,
           stock_cd_list,
           statement,
           consolidated,
           period_type,
           period_date,
           ...) {
    get_financial_report.gta_db(
      stock_db = stock_db,
      stock_cd_list = stock_cd_list,
      statement = statement,
      consolidated = consolidated,
      period_type = period_type,
      period_date = period_date,
      ...
    )
  }
)



# Get indicators from specified data source(table/file) in stock_db
# Method definition for s3 generic
#  @param data_filters   A list of filter to apply on result data. The list
#  consists of items which has field as its name and filter expr string as
#  its content, default is list( typrep = "typrep == 'A'",
#   markettype = "markettype == 21"). Only valid for gta_db.
# @param date_fields    A character vector of possible date field in original
#  data, which will be renamed as "date". default is c("date", "accper",
#  "trddt", "trdmnt", "trdynt","clsdt", "shrchdgt"). Only valid for gta_db.
# @param retain_fields  A character vector of possible fields to retain in
#  final dataset, default is c("stkcd", "indcd"). Only valid for gta_db.
# @export
get_indicators_from_source.gta_db <- function(stock_db,
                                              indicator_source,
                                              indicator_codes = NULL,
                                              ouput_format = c("long", "wide"),
                                              ...,
                                              data_filters = list(
                                                typrep = "typrep == 'A'",
                                                markettype = "markettype == 21"
                                              ),
                                              date_fields = c(
                                                "date", "accper",
                                                "trddt", "trdmnt", "trdynt",
                                                "clsdt", "shrchdgt"
                                              ),
                                              retain_fields = c(
                                                "stkcd",
                                                "indcd"
                                              )) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  assertive::assert_is_character(indicator_source)

  # get dataset from data source of different types
  # guess type of data source
  source_type <- ""
  file_ext <- tolower(tools::file_ext(indicator_source))
  if (file_ext != "") {
    # "*.ext": file
    source_type <- "file"
  } else {
    # "{xxx}": R code to run
    if (stringr::str_detect(indicator_source, "^\\{|\\}$")) {
      source_type <- "code"
    } else {
      # "xxx": table of database
      source_type <- "table"
    }
  }

  # get dataset according different type of data source
  ds_indicators_raw <- NULL
  switch(source_type,
    "table" = {
      # get dataset from table
      ds_indicators_raw <- get_table_dataset(stock_db,
        table_name = indicator_source
      )
    },
    "code" = {
      # get dataset from dynamic source by running code

      code_expr <- create_expr(!!indicator_source)

      # redefine eval_tidy to return default value and display error,
      # if an error occurred in evaluating expr
      eval_tidy_with_dfault <- purrr::possibly(rlang::eval_tidy,
        otherwise = NULL,
        quiet = FALSE
      )

      # evaluate code expr
      ds_indicators_raw <- eval_tidy_with_dfault(code_expr,
        env = rlang::caller_env()
      )
    },
    "file" = {
      # get dataset from file

      # get valid file path of indicator
      path_dir_indicator <- dir_path_db.gta_db(stock_db,
        dir_id = "DIR_DB_DATA_INDICATOR",
        force = FALSE
      )
      path_indicator_source <- file.path(path_dir_indicator, indicator_source)

      # get indicators from different format of file
      result <- tryCatch(
        {
          switch(file_ext,
            "rds" = {
              readRDS(path_indicator_source)
            },
            "csv" = {
              suppressMessages(
                readr::read_csv(path_indicator_source,
                  locale = readr::locale(encoding = "CP936")
                )
              )
            },
            {
              msg <- sprintf(
                "Can't read indicator from unsupport format file(*.%s).",
                file_ext
              )
              rlang::warn(msg)
              return(NULL)
            }
          )
        },
        error = function(e) e
      )
      if (inherits(result, "error")) {
        msg <- sprintf(
          "Fail to read indicator from %s:\n%s.",
          path_indicator_source,
          conditionMessage(result)
        )
        rlang::warn(msg)
        ds_indicators_raw <- NULL
      } else {
        ds_indicators_raw <- result
      }
    }
  )

  # transform indicators data
  ds_indicators <- NULL
  if (!is.null(ds_indicators_raw)) {
    ds_indicators <- tibble::as_tibble(ds_indicators_raw)
    ds_field_names <- names(ds_indicators)
    output_fields <- NULL
    output_indicators <- NULL
    output_non_indicators <- NULL

    # determine indicators output
    if (!is.null(indicator_codes)) {
      # output specified indicators
      indicator_codes <- tolower(indicator_codes)
      indicators_is_existed <- indicator_codes %in% ds_field_names
      output_indicators <- c(indicator_codes)
    } else {
      # output all indicators if indicator_list is null
      indicators_is_existed <- TRUE
      output_indicators <- c("dplyr::everything()")
    }

    # tranfrom fields for output
    if (all(indicators_is_existed)) {

      # apply data filters
      for (i in seq_along(data_filters)) {
        filter_field <- names(data_filters)[[i]]
        filter_string <- data_filters[[i]]
        filter_expr <- create_expr(!!filter_string)
        if (filter_field %in% ds_field_names) {
          # filter dataset
          ds_filter_indicators <- dplyr::filter(
            ds_indicators,
            !!filter_expr
          )

          # check filtered result
          if (NROW(ds_filter_indicators) > 0) {
            # return filtered data
            ds_indicators <- ds_filter_indicators
          } else {
            # return all data
            msg <- sprintf(
              "No effect of applying filter(%s), all data will return.",
              filter_string
            )
            rlang::inform(msg)
          }
        }
      }

      # convert numeric stkcd into string stkcd if need
      if (("stkcd" %in% ds_field_names) && is.numeric(ds_indicators$stkcd)) {
        ds_indicators <- ds_indicators %>%
          dplyr::mutate(stkcd = sprintf("%06d", stkcd))
        msg <- "Coerce stock cd to character of 6 digits if it were numeric."
        rlang::warn(msg)
      }

      # rename and transform date-related field if needed
      if (!is.null(date_fields) & (length(date_fields) > 0) &
        is.character(date_fields)) {
        date_fields <- tolower(date_fields)
        match_date_fields <- ds_field_names[ds_field_names %in% date_fields]
        if (length(match_date_fields) >= 1) {
          # rename date field as "date" if needed
          date_field <- match_date_fields[[1]]
          ds_indicators <- dplyr::rename(ds_indicators, date = !!date_field)

          # put "date" field into output non-indicator field
          output_non_indicators <- c(output_non_indicators, "date")

          if (length(match_date_fields) > 1) {
            msg <- sprintf(
              "More than one possible date field(%s), the first field(%s) is renamed to 'date'.",
              stringr::str_c(match_date_fields, collapse = ","),
              date_field
            )
          } else {
            msg <- sprintf("Date field(%s) is renamed to 'date'.", date_field)
          }

          rlang::inform(msg)
        }
      }


      # transform date field
      if ("date" %in% names(ds_indicators)) {
        ts_date <- NULL

        # transform date field into date format
        if (lubridate::is.timepoint(ds_indicators$date)) {
          # translate all time format into date format
          ts_date <- lubridate::as_date(ds_indicators$date)
        } else {
          # try translate date as charater as possible
          ts_date <- as.character(ds_indicators$date)

          # parse date field as character and return na if not recognised
          ts_date <- lubridate::parse_date_time(ts_date,
            orders = c("y", "ym", "yW", "ymd")
          )
          if (all(is.na(ts_date))) {
            # invalid type for date field
            msg <- sprintf(
              "Fail to translate date field of type of %s.",
              typeof(ds_indicators$date)
            )
            rlang::abort(msg)
          }
          ts_date <- lubridate::as_date(ts_date)
        }

        # translate date into last day of period, add period field
        if (!is.null(ts_date)) {

          # turn it into the last day of period
          dates_period <- guess_dates_period(ts_date)
          switch(dates_period,
            "day" = {
              period_unit <- "day"
            },
            "month" = {
              period_unit <- "month"
            },
            "quarter" = {
              period_unit <- "quarter"
            },
            "year" = {
              period_unit <- "year"
            },
            "unknown" = {
              period_unit <- "day"
            }
          )

          # last day of period = ceiling_date -1
          ceiling_date <- lubridate::ceiling_date(ts_date,
            unit = period_unit,
            change_on_boundary = TRUE
          )

          # keep original ts_date for filter invalid date
          origin_ts_date <- ts_date

          ts_date <- ceiling_date - 1

          # add period field and translated date field
          ds_indicators <- dplyr::mutate(ds_indicators,
            period = period_unit,
            origin_date = origin_ts_date,
            date = ts_date
          )

          # filter out data with invalid date accoring period
          # speccal coding for quarterly financial report
          if (dates_period == "quarter") {
            ds_indicators <- ds_indicators %>%
              dplyr::filter(lubridate::month(origin_date)
              %in% c(3, 6, 9, 12))
          }

          # remove origin_date fields
          ds_indicators <- ds_indicators %>%
            dplyr::select(-origin_date)

          # put "period" field into output non-indicators field
          output_non_indicators <- c(output_non_indicators, "period")
        }
      }

      # Output indicators fields from dataset

      # put remain fields into output field
      if (!is.null(retain_fields) & (length(retain_fields) > 0) &
        is.character(retain_fields)) {
        retain_fields <- tolower(retain_fields)
        match_retain_fields <- ds_field_names[ds_field_names %in% retain_fields]

        # put "retain" field into output non-indicators field
        if (length(match_retain_fields) >= 1) {
          output_non_indicators <- c(output_non_indicators, match_retain_fields)
        }
      }

      # output select fields
      output_fields <- c(output_non_indicators, output_indicators)
      ds_indicators <- dplyr::select(
        ds_indicators,
        !!!rlang::parse_exprs(output_fields)
      )

      # message results
      if (!is.null(indicator_codes)) {
        msg <- sprintf(
          "Get indicators: %s(%s) from %s.",
          stringr::str_c(indicator_codes, collapse = ","),
          stringr::str_c(code2name(stock_db, indicator_codes, type = "field"),
            collapse = ","
          ),
          indicator_source
        )
      } else {
        msg <- sprintf(
          "Get all indicators from %s.",
          indicator_source
        )
      }

      rlang::inform(msg)
    } else {

      # some indicators miss from result dataset
      msg <- sprintf(
        "Indicators: %s aren't in the table of %s.",
        stringr::str_c(indicator_codes[!indicators_is_existed], collapse = ","),
        indicator_source
      )
      rlang::abort(msg)
    }
  }

  # transform by output format
  if (!is.null(ds_indicators)) {
    ouput_format <- match.arg(ouput_format)

    # by default, ds_indicator is shorter and wider format
    # transform it into longer and narrower format if specified
    if (ouput_format == "long") {
      # use numeric fields as value fields for transform
      value_fields <- expect_type_fields(ds_indicators, expect_type = "double")
      if (length(value_fields) > 0) {
        ds_indicators <- ds_indicators %>%
          tidyr::pivot_longer(
            names_to = "ind_code",
            values_to = "ind_value",
            !!value_fields,
            values_drop_na = FALSE # must keep vars with NAs
          )
      } else {
        # raise error for no numeric fields
        msg <- sprintf(
          "Can't tranfrom into long-formt dataset, since no numeric field in the table of %s.",
          indicator_source
        )
        rlang::abort(msg)
      }
    }
  }

  return(ds_indicators)
}
# Method definition for s4 generic
#' @param data_filters   A list of filter to apply on result data. The list
#'  consists of items which has field as its name and filter expr string as
#'  its content, default is list( typrep = "typrep == 'A'",
#'  markettype = "markettype == 21"). Only valid for gta_db.
#' @param date_fields    A character vector of possible date field in original
#'  data, which will be renamed as "date". default is c("date", "accper",
#'  "trddt", "trdmnt", "trdynt","clsdt", "shrchdgt"). Only valid for gta_db.
#' @param retain_fields  A character vector of possible fields to retain in
#'  final dataset, default is c("stkcd", "indcd"). Only valid for gta_db.
#'
#' @describeIn get_indicators_from_source  get indicator timeseries from
#'  specified source in a database of gta_db class
#' @export
setMethod(
  "get_indicators_from_source",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_source, indicator_codes, ouput_format, ...,
           data_filters, date_fields, retain_fields) {
    get_indicators_from_source.gta_db(
      stock_db, indicator_source,
      indicator_codes, ouput_format, ...,
      data_filters, date_fields, retain_fields
    )
  }
)

# Save indicators to specified data source(table/file) in stock_db
# Method definition for s3 generic
# @describeIn save_indicators_to_source   save indicator timeseries to
#  specified source in a database of gta_db class
#
# @export
save_indicators_to_source.gta_db <- function(stock_db,
                                             indicator_source,
                                             ts_indicators,
                                             ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  if (missing(indicator_source) || !is.character(indicator_source)) {
    rlang::abort("Data source name must be character string")
  }

  assertive::assert_is_data.frame(ts_indicators)
  assertive::assert_is_character(indicator_source)

  # save dataset from data source of different types
  # guess type of data source
  source_type <- ""
  file_ext <- tolower(tools::file_ext(indicator_source))
  if (file_ext != "") {
    # "*.ext": file
    source_type <- "file"
  } else {
    # "{xxx}": R code to run
    if (stringr::str_detect(indicator_source, "^\\{|\\}$")) {
      source_type <- "code"
    } else {
      # "xxx": table of database
      source_type <- "table"
    }
  }

  # save dataset according different type of data source
  switch(source_type,
    "table" = {
      # don't save dataset to table
      rlang::abort("Can't save indicator into a table in databse.")
    },
    "code" = {
      # don't save data to read-only dynamic source
      rlang::abort("Can't save indicator into read-only dynamic source.")
    },
    "file" = {
      # save dataset to file
      # get valid file path of indicator
      path_dir_indicator <- dir_path_db.gta_db(stock_db,
        dir_id = "DIR_DB_DATA_INDICATOR",
        force = FALSE
      )
      path_indicator_source <- file.path(path_dir_indicator, indicator_source)

      # save indicator into specified format file
      switch(file_ext,
        "rds" = {
          saveRDS(ts_indicators, file = path_indicator_source)
        },
        "csv" = {
          readr::write_csv(ts_indicators, file = path_indicator_source)
        },
        {
          msg <- sprintf(
            "Can't save indicator into unsupport format file(*.%s).",
            file_ext
          )
          rlang::abort(msg)
        }
      )
    }
  )

  return(invisible(NULL))
}
# Method definition for s4 generic
#' @describeIn save_indicators_to_source  save indicator timeseries to
#'  specified source in a database of gta_db class
#' @export
setMethod(
  "save_indicators_to_source",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_source, ts_indicators, ...) {
    save_indicators_to_source.gta_db(
      stock_db, indicator_source, ts_indicators, ...
    )
  }
)


# Get indicators timeseries from stock_db
# Method definition for s3 generic
# @describeIn get_indicators get indicator timeseries from
#  a database of gta_db class
# @export
get_indicators.gta_db <- function(stock_db, indicator_codes, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  assertive::assert_is_not_null(indicator_codes)
  assertive::assert_is_character(indicator_codes)

  success <- TRUE

  # get file table name mapping for referece
  gta_profile_name <- get_profile(stock_db)

  # get indcator info of matched factor
  matched_indicators <- profile_get_indicators(
    gta_profile_name,
    indicator_codes
  )

  if (!is.null(matched_indicators)) {
    # build table_list for fetching indicators
    indicator_sources <- matched_indicators %>%
      dplyr::filter(ind_code %in% tolower(indicator_codes)) %>%
      dplyr::group_by(ind_source) %>%
      tidyr::nest()
  } else {
    success <- FALSE
  }


  # get indicators dataset from indicator sources
  ds_combined_indicators <- NULL
  if (success) {
    for (i in seq_len(nrow(indicator_sources))) {
      indicator_source <- indicator_sources[i, ]$ind_source
      indicator_params <- indicator_sources[i, ]$data[[1]]
      indicator_codes <- indicator_params$ind_code

      # Get a indicator dataset from database
      if (success) {
        ds_indicators <- get_indicators_from_source(stock_db,
          indicator_source = indicator_source,
          indicator_codes = indicator_codes,
          ouput_format = "long"
        )
        if (!is.null(ds_indicators)) {
        } else {
          success <- FALSE
        }
      }

      # bind the factors dataset
      if (success) {
        if (is.null(ds_combined_indicators)) {
          ds_combined_indicators <- ds_indicators
        } else {
          ds_combined_indicators <- ds_combined_indicators %>%
            dplyr::bind_rows(ds_indicators)
          # dplyr::bind_rows(ds_indicators, .id = "table")
        }
      }
    }
  }

  return(ds_combined_indicators)
}
# Method definition for s4 generic
#' @describeIn get_indicators get indicator timeseries from
#'  a database of gta_db class
#' @export
setMethod(
  "get_indicators",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_codes, ...) {
    get_indicators.gta_db(stock_db, indicator_codes, ...)
  }
)

# Get indicators Info from stock_db
# Method definition for s3 generic
# @describeIn get_indicators_info get indicators info from a database of
#   gta_db class
# @export
get_indicators_info.gta_db <- function(stock_db,
                                       indicator_codes = NULL,
                                       ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # get file table name mapping for reference
  gta_profile_name <- get_profile(stock_db)

  # get indicators info of matched by indicator_codes
  matched_indicators_info <- profile_get_indicators(gta_profile_name,
    indicator_codes = indicator_codes
  )

  # build specified result of matched indicators
  matched_indicators_info <- matched_indicators_info %>%
    dplyr::select(
      ind_code = ind_code,
      ind_name = ind_name,
      ind_type = ind_type,
      ind_category = ind_category,
      ind_description = ind_description
    )

  return(matched_indicators_info)
}

# Method definition for s4 generic
#' @describeIn get_indicators_info  get indicators info from a database of
#' gta_db class
#' @export
setMethod(
  "get_indicators_info",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_codes, ...) {
    get_indicators_info.gta_db(stock_db, indicator_codes, ...)
  }
)

# Get factors timeseries from stock_db
# Method definition for s3 generic
# @describeIn get_factors get factor timeseries from
#  a database of gta_db class
# @export
get_factors.gta_db <- function(stock_db, factor_codes, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  assertive::assert_is_not_null(factor_codes)
  assertive::assert_is_character(factor_codes)

  success <- TRUE

  # get file table name mapping for referece
  gta_profile_name <- get_profile(stock_db)

  # get indcator info of matched factor
  matched_factors_info <- profile_get_factors(gta_profile_name, factor_codes)
  if (is.null(matched_factors_info)) {
    success <- FALSE
  }

  # get indicators for matched factors
  ds_indicators <- NULL
  if (success) {
    indicator_codes <- matched_factors_info$indicator_code
    ds_indicators <- get_indicators(stock_db,
      indicator_codes = indicator_codes
    )
    if (is.null(ds_indicators)) {
      success <- FALSE
    }
  }

  # translate indicators into factors
  ds_factors <- NULL
  if (success) {

    # use lookup table to translate value of ind_name to value of factor_name
    lookup <- matched_factors_info$factor_code
    names(lookup) <- tolower(matched_factors_info$indicator_code)
    ds_factors <- ds_indicators %>%
      dplyr::mutate(ind_code = unname(lookup[ind_code]))

    # change colname: ind_name/value into factor_name/value
    ds_factors <- ds_factors %>%
      dplyr::rename(
        factor_code = ind_code,
        factor_value = ind_value
      )
  }

  return(ds_factors)
}
# Method definition for s4 generic
#' @describeIn get_factors get factor timeseries from
#'  a database of gta_db class
#' @export
setMethod(
  "get_factors",
  signature(stock_db = "gta_db"),
  function(stock_db, factor_codes, ...) {
    get_factors.gta_db(stock_db, factor_codes, ...)
  }
)

# Get factors Info from stock_db
# Method definition for s3 generic
# @describeIn get_factors_info get factors info from a database of gta_db class
# @export
get_factors_info.gta_db <- function(stock_db,
                                    factor_codes = NULL,
                                    factor_groups = NULL,
                                    ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # get file table name mapping for referece
  gta_profile_name <- get_profile(stock_db)

  # get factors info of matched by factor_codes/factor_group
  matched_factors_info <- profile_get_factors(gta_profile_name,
    factor_codes = factor_codes,
    factor_groups = factor_groups
  )

  # build specified result of matched factors
  matched_factors_info <- matched_factors_info %>%
    dplyr::select(
      factor_code = factor_code,
      factor_name = factor_name,
      factor_type = factor_type,
      factor_group = factor_group,
      factor_description = factor_description,
      factor_lag_month = factor_lag_month
    )

  return(matched_factors_info)
}

# Method definition for s4 generic
#' @describeIn get_factors_info  get factors info from a database of gta_db class
#'
#' @export
setMethod(
  "get_factors_info",
  signature(stock_db = "gta_db"),
  function(stock_db, factor_codes, factor_groups, ...) {
    get_factors_info.gta_db(stock_db, factor_codes, factor_groups, ...)
  }
)

# Get industry info of stocks from stock_db
# # Method definition for s3 generic
# @describeIn get_stock_industry  get industry info timeseries of stocks from
#  a database of gta_db class
# @export
get_stock_industry.gta_db <- function(stock_db, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # get industry info about stocks from database
  table_name <- stock_db$table_list[["TRD_STOCK_INDUSTRY"]]
  ds_stock_industry_raw <- get_table_dataset(stock_db,
    table_name = table_name
  )

  # build result dataset
  ds_stock_industry <- NULL
  if (!is.null(ds_stock_industry_raw)) {
    ds_stock_industry <- ds_stock_industry_raw %>%
      tibble::as_tibble() %>%
      dplyr::filter(typrep == "A") %>%
      dplyr::mutate(date = as.Date(accper)) %>%
      dplyr::select(date, stkcd, indcd) %>%
      dplyr::arrange(stkcd, date)
  }

  return(ds_stock_industry)
}
# Method definition for s4 generic
#' @describeIn get_stock_industry  get industry info timeseries of stocks from
#' a database of gta_db class
#' @export
setMethod(
  "get_stock_industry",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    get_stock_industry.gta_db(stock_db, ...)
  }
)


# Get stock info of special treatment from stock_db
# Method definition for s3 generic
# @describeIn get_spt_stocks  get stock info of special treatment from
#  a database of gta_db class
# @export
get_spt_stocks.gta_db <- function(stock_db, ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # get spt info about stocks from database
  table_name <- stock_db$table_list[["SPT_TRDCHG"]]
  ds_spt_stocks_raw <- get_table_dataset(stock_db,
    table_name = table_name
  )

  # get company info from database
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_company_info <- get_table_dataset(stock_db,
    table_name = table_name
  )

  # build result
  ds_spt_stocks_raw <- ds_spt_stocks_raw %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      annoudt = lubridate::as_date(annoudt),
      execudt = lubridate::as_date(execudt)
    )

  # get inital status of spt stocks at listing date
  spt_stkcds <- unique(ds_spt_stocks_raw$stkcd)
  ds_spt_stocks_initital_status <- ds_company_info %>%
    tibble::as_tibble() %>%
    dplyr::filter(stkcd %in% spt_stkcds) %>%
    dplyr::mutate(status_code = "A", date = lubridate::as_date(listdt)) %>%
    dplyr::select(stkcd, date, status_code)

  # get changed status of spt stocks after listing date
  ds_spt_stocks_changed_status <- ds_spt_stocks_raw %>%
    tidyr::separate(chgtype,
      into = c("before_status", "after_status"),
      sep = 1
    ) %>%
    dplyr::select(stkcd, date = execudt, status_code = after_status)

  # get full history of status of spt stocks
  ds_spt_stocks_status <- ds_spt_stocks_changed_status %>%
    dplyr::bind_rows(ds_spt_stocks_initital_status) %>%
    dplyr::arrange(stkcd, date)

  # translate status_code into status_name
  # A:正常上市
  # B:ST
  # D:*ST
  # C:PT
  # S:代表暂停上市
  # T:代表退市整理期
  # X:代表终止上市
  trade_status <- c(
    "A" = "list",
    "B" = "st",
    "D" = "*st",
    "C" = "pt",
    "S" = "suspend",
    "T" = "pre_delist",
    "X" = "delist"
  )

  # add status_names fields crossponding to status_code
  ds_spt_stocks_status <- ds_spt_stocks_status %>%
    dplyr::mutate(trade_status = trade_status[status_code])

  return(ds_spt_stocks_status)
}
# Method definition for s4 generic
#' @describeIn get_spt_stocks  get stock info of special treatment from
#' a database of gta_db class
#' @export
setMethod(
  "get_spt_stocks",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    get_spt_stocks.gta_db(stock_db, ...)
  }
)



# Get riskfree rate from stock_db
# Method definition for s3 generic
# @describeIn get_riskfree_rate  get riskfree rate timeseries from a database of
#   gta_db class
#  @export
get_riskfree_rate.gta_db <- function(stock_db,
                                     period = c(
                                       "day", "month",
                                       "quarter", "year"
                                     ),
                                     ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # get riskfree rate from database
  table_name <- stock_db$table_list[["TRD_NRRATE"]]
  ds_riskfree_rate_raw <- get_table_dataset(stock_db,
    table_name = table_name
  )

  # Notice: all rates in raw data are pecentage
  ds_riskfree_rate <- ds_riskfree_rate_raw %>%
    tibble::as_tibble() %>%
    dplyr::filter(nrr1 == "NRI01") %>%
    dplyr::mutate(date = as.Date(clsdt), daily_return = nrrdaydt / 100) %>%
    dplyr::select(date, daily_return) %>%
    dplyr::arrange(date)

  # refreq riskfree rate into regular daily series
  ds_riskfree_rate <- ds_riskfree_rate %>%
    ts_asfreq(
      freq_rule = "day",
      fillna_method = "ffill"
    )

  # build dataset of riskfree rate of period
  period <- match.arg(period)
  ds_riskfree_rate_period <- NULL
  .compoud_return <- function(x, ...) {
    prod(1 + x, ...) - 1
  }
  freq_rule <- period
  ds_riskfree_rate_period <- ds_riskfree_rate %>%
    ts_resample(
      freq_rule = freq_rule,
      fillna_method = "ffill",
      agg_fun = .compoud_return,
      na.rm = TRUE
    )

  ds_riskfree_rate_period <- ds_riskfree_rate_period %>%
    dplyr::select(date, riskfree_return = daily_return) %>%
    dplyr::mutate(period = !!period) %>%
    dplyr::select(date, period, dplyr::everything())

  return(ds_riskfree_rate_period)
}
# Method definition for s4 generic
#' @describeIn get_riskfree_rate get riskfree rate timeseries from a database of
#'  gta_db class
#' @export
setMethod(
  "get_riskfree_rate",
  signature(stock_db = "gta_db"),
  function(stock_db, period, ...) {
    get_riskfree_rate.gta_db(stock_db, period, ...)
  }
)

# Get Path of Data Directory from stock_db
# Method definition for s3 generic
# @describeIn dir_path_db get path of data directory from a database of
#   gta_db class
# @export
dir_path_db.gta_db <- function(stock_db,
                               dir_id = c(
                                 "DIR_DB_DATA",
                                 "DIR_DB_DATA_SOURCE",
                                 "DIR_DB_DATA_ORIGIN",
                                 "DIR_DB_DATA_LOG",
                                 "DIR_DB_DATA_INDICATOR"
                               ),
                               force = TRUE,
                               ...) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    rlang::abort("Stock db isn't connected, try to connect db again")
  }

  # get dir variable from profile
  gta_profile_name <- get_profile(stock_db)
  dir_id <- match.arg(dir_id)
  dir_path <- profile_get_varible_setting(gta_profile_name, dir_id)

  # check whether the dir exists and return full path
  if (!is.null(dir_path)) {
    if (force) {
      # return path whether it exist or not.
      dir_path <- normalizePath(dir_path,
        winslash = "/",
        mustWork = FALSE
      )
    } else {
      # check whether path exist and return path
      dir_path <- normalizePath(dir_path,
        winslash = "/",
        mustWork = TRUE
      )
    }
  }

  return(dir_path)
}

# Method definition for s4 generic
#' @describeIn dir_path_db get path of data directory from a database of
#'  gta_db class
#' @export
setMethod(
  "dir_path_db",
  signature(stock_db = "gta_db"),
  function(stock_db, dir_id, force, ...) {
    dir_path_db.gta_db(stock_db, dir_id, force, ...)
  }
)

# Non-generic internal functions for gta_db operation ---------------------------------

# Create a stock_filed_list for a database of gta_db class
stock_field_list.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build field_name list
  field_name_list <- NULL
  table_name <- stock_db$table_list[["gta_fieldname_list"]]
  field_list.df <- get_table_dataset(stock_db, table_name)
  if (!is.null(field_list.df)) {
    codes <- field_list.df[, "field_code"]
    codes <- tolower(codes)
    names <- field_list.df[, "field_name"]

    field_name_list <- code_name_list(codes, names)
  } else {
    rlang::warn("Can't create code_name_list due to failing
            to get data from stock db")
  }

  return(field_name_list)
}

# Create a stock_name_list for a database of gta_db class
stock_name_list.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {
    codes <- ds_trd_company.df[, "stkcd"]
    # Coerce stock code to 6 digit of characters of format of "xxxxxxx"
    if (!is.character(codes)) {
      codes <- stringr::str_pad(codes, width = 6, pad = "0")
      msg <- "Coerce stock cd to character of 6 digits if it were number."
      warnings(msg)
    }

    names <- ds_trd_company.df[, "stknme"]

    stock_name_list <- code_name_list(codes, names)
  } else {
    rlang::warn("Can't create code_name_list due to failing
            to get data from stock db")
  }

  return(stock_name_list)
}

# Create a industry_name_list for a database of gta_db class
industry_name_list.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {
    ds_indistry <- ds_trd_company.df %>%
      dplyr::select(nnindcd, nnindnme) %>%
      dplyr::distinct()

    codes <- ds_indistry[, "nnindcd"]
    codes <- as.character(codes)
    names <- ds_indistry[, "nnindnme"]

    stock_name_list <- code_name_list(codes, names)
  } else {
    rlang::warn("Can't create code_name_list due to failing
            to get data from stock db")
  }

  return(stock_name_list)
}

# Create a factor_name_list for a database of gta_db class
factor_name_list.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # Build factor_name_list
  factor_name_list <- NULL
  factors_info <- zstmodelr::get_factors_info(stock_db)
  if (!is.null(factors_info)) {
    codes <- dplyr::pull(factors_info[, "factor_code"])
    names <- dplyr::pull(factors_info[, "factor_name"])
    factor_name_list <- code_name_list(codes, names)
  }

  return(factor_name_list)
}

# Create a indicator_name_list for a database of gta_db class
indicator_name_list.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # Build indicator_name_list
  indicator_name_list <- NULL
  indicators_info <- zstmodelr::get_indicators_info(stock_db)
  if (!is.null(indicators_info)) {
    codes <- dplyr::pull(indicators_info[, "ind_code"])
    names <- dplyr::pull(indicators_info[, "ind_name"])
    indicator_name_list <- code_name_list(codes, names)
  }

  return(indicator_name_list)
}
