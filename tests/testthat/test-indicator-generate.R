# Tests for function of indicator generate  ----
context("Tests for function of indicator generate ")

# set up testing context
dsn <- "GTA_SQLData"
DB_PROFILE_FILE <- "gta_profile.xlsx"

stock_db <- stock_db(gta_db, dsn)
suppressMessages(db_ready <- open_stock_db(stock_db))
# skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for stock_db", dsn)
)
suppressMessages(init_stock_db(stock_db))

# prepare test datasets
customized_indictors_info <- tibble::tibble(
  ind_code = c("m_ep_ttm", "q_ep_ttm"),
  ind_type = "",
  ind_name = c("Month_EP_TTM", "Quater_EP_TTM"),
  ind_category = "customized",
  ind_source = c("test_m_ep_ttm.csv", "test_q_ep_ttm.csv"),
  ind_description = "",
  ind_formula = c(
    "price <- dplyr::lag(mclsprc, n=2)
                 earning <- f090101c
                 ep <- earning/price",
    "price <- dplyr::lag(mclsprc, n=2)
                 earning <- f090101c
                 ep <- earning/price"
  ),
  ind_keys = list(c("stkcd")),
  rolling_window = 0,
  period = c("month", "quarter"),
  output_format = "csv",
  is_active = TRUE
)

# create test_indicator_defs
test_indicator_defs <- build_indicator_defs.gta_db(stock_db, customized_indictors_info)

test_that("generate_indictors", {

  ## generate_indictors with various arguments ====
  ds_indicators <- generate_indictors(stock_db,
                                      ds_indicator_defs = test_indicator_defs,
                                      validate_def = TRUE,
                                      parallel = FALSE
  )

  #check output file
  dir_indicators <- dir_path_db(stock_db,"DIR_DB_DATA_INDICATOR")
  path_ouput_files <- paste0(dir_indicators,"/",test_indicator_defs$ind_source)
  purrr::map(path_ouput_files, ~expect_true(file.exists(.x)))

  #check log file


})

test_that("delete_indicators", {

  ## delete_indicators with various arguments ====
  delete_indicators(stock_db,
                   ds_indicator_defs = test_indicator_defs)

  #check output file
  dir_indicators <- dir_path_db(stock_db,"DIR_DB_DATA_INDICATOR")
  path_ouput_files <- paste0(dir_indicators,"/",test_indicator_defs$ind_source)
  purrr::map(path_ouput_files, ~expect_false(file.exists(.x)))

})
