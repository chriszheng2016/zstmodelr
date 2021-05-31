# Extend testthat expection for not null
expect_not_null <- function(object, info = NULL, label = NULL) {
  # Capture object and label
  act <- quasi_label(rlang::enquo(object))

  # Call expect()
  expect(!is.null(object), sprintf("%s is null.", act$lab),
    info = info
  )

  # Invisibly return the value
  invisible(act$val)
}
