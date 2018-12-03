# Tools for producing customized indicators periodically
library(zstmodelr)

# Main function to procduce indicators
produce_indictors <- function(parallel = TRUE) {

  # conect to target stock db
  stock_db <- stock_db(gta_db, "GTA_SQLData")
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  # build indicator definition list
  ds_indicator_defs <- get_indicator_defs(stock_db)

  # load vars dataset for generating indicators
  ds_vars <- get_indicator_vars(stock_db, ds_indicator_defs)

  # produce indicators batchly
  for (i in seq_len(NROW(ds_indicator_defs))) {
    indicator_def <- ds_indicator_defs[i, ]
    ind_def_fun <- indicator_def$ind_def_fun[[1]]

    if (!is.null(ind_def_fun)) {
      # compute a indicator from vars dataset.
      msg <- sprintf(
        "Compute indicator: %s(%s) ...\n", indicator_def$ind_code,
        indicator_def$ind_name
      )
      message(msg)

      ds_indicator <- compute_indicator(ds_vars,
        ind_def_fun = ind_def_fun,
        date_index_field = "date",
        key_fields = "stkcd",
        parallel = parallel
      )

      # save indicators into source
      success <- save_indicators_to_source(stock_db,
        indicator_source = indicator_def$ind_source,
        ts_indicators = ds_indicator
      )
      if (success) {
        msg <- sprintf(
          "Save indicator: %s(%s) in %s successfully.\n",
          indicator_def$ind_code,
          indicator_def$ind_name,
          indicator_def$ind_source
        )
        message(msg)
      } else {
        msg <- sprintf(
          "Fail to compute indicator: %s(%s), because ind_def_fun is NULL.\n",
          indicator_def$ind_code,
          indicator_def$ind_name
        )
        warning(msg)
      }
    }
  }

  open_stock_db(stock_db)
}

# Run indicators producer
# produce_indictors()
