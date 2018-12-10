
#' Generate indicators in batch mode
#'
#' Generate indistors in batch from stock_db.
#'
#' There are two methods of generating indicators:
#' \itemize{
#'       \item formal production: produce final result of indicators on full
#'       dataset from stock_db, which takes longer time ;
#'       \item validating defintion: produce trial results of indicators on
#'       small dataset from stock_db, which takes shorter time.
#'   }
#'
#' @param stock_db  A stock database object to operate.
#' @param ds_indicator_defs  A dataframe of indicator definintion to generate.
#' @param validate_def A logical, whether to validate indicator definition.
#'  Default False, means to produce indicators on full dataset, not validating
#'  definintion on small datasets.
#' @param parallel   A logic to deterimine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#' @param retry_log Log file for re-generating indicators with recording failure
#'   in log file. If NULL, it will generate indicators specifeid by indicator_codes,
#'   otherse only generate indicators with recording failure in log file.
#'   By default NULL.
#' @param log_dir Path to save updating log file. If NULL, don't save log file
#'   by default "./log".
#'
#'
#' @family indicator build functions
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
generate_indictors <- function(stock_db,
                               ds_indicator_defs,
                               validate_def = FALSE,
                               parallel = TRUE,
                               retry_log = NULL,
                               log_dir = "./log") {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ds_indicator_defs)

  success <- TRUE

  # load vars dataset for generating indicators
  ds_all_vars <- get_indicator_vars(stock_db, ds_indicator_defs)
  if (is.null(ds_all_vars)) success <- FALSE


  # Set validate params
  debug <- FALSE
  if (success) {
    if (validate_def) {

      # filter vars to small scale dataset to validate indicator definition
      validate_stkcds <- c("600031", "600066", NA)
      ds_all_vars <- ds_all_vars %>%
        dplyr::filter(stkcd %in% validate_stkcds)

      # turn on debug on create indicators
      debug <- TRUE

      # change output filet as *.csv
      ds_indicator_defs <- ds_indicator_defs %>%
        dplyr::mutate(ind_source = purrr::map_chr(
          ds_indicator_defs$ind_source,
          ~stringr::str_replace(.x, pattern = "\\.\\w*", replacement = ".csv")
        ))

      msg <- sprintf(
        "\nOnly validate definition of indicators on %s, not a real production!\n",
        paste(validate_stkcds, collapse = ",")
      )
      rlang::inform(msg)
    }
  }

  # add attribute of indcd
  if (success) {
    new_attr_indcd <- ind_attr_def_indcd(stock_db)
  }

  # generate indicators in batch
  if (success) {
    for (i in seq_len(NROW(ds_indicator_defs))) {
      indicator_def <- ds_indicator_defs[i, ]
      ind_def_fun <- indicator_def$ind_def_fun[[1]]
      ind_vars <- indicator_def$ind_vars[[1]]
      key_fields <- indicator_def$ind_keys[[1]]

      success <- TRUE
      if (!is.null(ind_def_fun)) {
        msg <- sprintf(
          "\nCompute indicator: %s(%s) ...\n", indicator_def$ind_code,
          indicator_def$ind_name
        )
        rlang::inform(msg)

        # filter vars of the indicator
        ds_def_vars <- ds_all_vars %>%
          dplyr::filter(ind_name %in% ind_vars)

        # create a indicator from vars dataset.
        ds_indicator <- create_indicator(ds_def_vars,
          ind_def_fun = ind_def_fun,
          debug = debug,
          date_index_field = "date",
          key_fields = key_fields,
          parallel = parallel
        )
        if (is.null(ds_indicator)) success <- FALSE

        # add attribute of industry code(indcd)
        # Notice: indcd must use stkcd as key_fields
        if (success) {
          if (!("indcd" %in% names(ds_indicator))) {
            ds_indicator <- modify_indicator(
              ts_indicator = ds_indicator,
              modify_fun = new_attr_indcd,
              replace_exist = FALSE,
              date_index_field = "date",
              key_fields = "stkcd",
              parallel = parallel
            )
          }
          if (is.null(ds_indicator)) success <- FALSE
        }


        # save indicators into source
        if (success) {
          save_indicators_to_source(stock_db,
            indicator_source = indicator_def$ind_source,
            ts_indicators = ds_indicator
          )
        }

        if (success) {
          msg <- sprintf(
            "Compute indicator successfully, save in: %s(%s) in %s.\n",
            indicator_def$ind_code,
            indicator_def$ind_name,
            indicator_def$ind_source
          )
          rlang::inform(msg)
        } else {
          msg <- sprintf(
            "Fail to compute indicator: %s(%s), because ind_def_fun is NULL.\n",
            indicator_def$ind_code,
            indicator_def$ind_name
          )
          rlang::warn(msg)
        }
      }
    }
  }
}


#' Delete indicators files
#'
#' Delete clear indistors files in inidcators dir from stock_db.
#'
#' @param stock_db  A stock database object to operate.
#' @param ds_indicator_defs  A dataframe of indicator definintion to generate.
#'
#' @family indicator build functions
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
delete_indicators <- function(stock_db,
                             ds_indicator_defs) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ds_indicator_defs)

  # remove indicators files in indicator dir
  dir_indicators <- dir_path_db(stock_db,"DIR_DB_DATA_INDICATOR")
  path_ouput_files <- paste0(dir_indicators,"/",ds_indicator_defs$ind_source)
  quiet_file_remove <- purrr::possibly(file.remove, otherwise = FALSE )
  purrr::walk(path_ouput_files, ~quiet_file_remove(.x))

  return(invisible(NULL))

}
