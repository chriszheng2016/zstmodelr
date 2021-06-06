# Utility functions as the helpers for testthat ----


#' Extend testthat expectation for not null
#'
#' This is an expectation as a counterpart of [testthat::expect_null()].
#'
#' @param object Object to test.
#' @param info Extra information to be included in the message.
#'  This argument is soft-deprecated and should not be used in new code.
#'  Instead see alternatives in [testthat::quasi_label()].
#' @param label Used to customize failure messages. For expert use only.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' x <- NULL
#' y <- 10
#'
#' expect_not_null(y)
#'
#' testthat::show_failure(expect_not_null(x))
#' }
expect_not_null <- function(object, info = NULL, label = NULL) {
  # Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), label, arg = "object")

  # Call expect()
  testthat::expect(!is.null(object), sprintf("%s is null.", act$lab),
    info = info
  )

  # Invisibly return the value
  invisible(act$val)
}

# Skip tests if stock db is not ready

#' Skip tests if stock db is not ready
#'
#' This wrapper of [skip_if_not()][testthat::skip_if_not] to skip tests
#' if stock db is not available for testing.
#'
#' You could simulate a environment without stock db by setting environment
#' variable "NO_STOCK_DB" to "true".
#'
#' @param dsn A string of DSN to connected. Default value "GTA_SQLData".
#'
#' @return No return value
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Skip tests when stock db is unavailable in the test file
#' skip_if_stock_db_not_ready()
#' test_that("tests related to stock db", {
#'   expect_equal(1, 3) #'
#' })
#'
#' # Run Tests in a simulated environment without stock db
#' withr::with_envvar(
#'   new = c("NO_STOCK_DB" = "true"),
#'   devtools::test() # test package
#'   )
#' }
#'
skip_if_stock_db_not_ready <- function(dsn = "GTA_SQLData") {

  # NO_STOCK_DB is used to simulate stock db is not unavailable
  no_stock_db <- identical(Sys.getenv("NO_STOCK_DB"), "true")

  if (!no_stock_db) {
    # Test stock database is ready when stock db is avaliable
    stock_db <- stock_db(gta_db, dsn)
    suppressMessages(db_ready <- open_stock_db(stock_db))
    withr::defer({
      suppressMessages(close_stock_db(stock_db))
    })
  } else {
    db_ready <- FALSE
  }

  # Skip if stock database is not ready
  testthat::skip_if_not(db_ready,
    message = sprintf(
      "DSN(%s) is not ready, skip tests related to stock database ",
      dsn
    )
  )
}
