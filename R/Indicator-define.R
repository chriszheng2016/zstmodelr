
# translate indicator formula into function

create_indicator_def <- function(indicator_expr,
                                 indicator_name,
                                 rolly_window = 0) {

  #validate params
  if (!rlang::is_call(indicator_expr)) {
    msg <- "indicator_expr should be exprs!!"
    stop(msg)
  }


  # define method of evaluting exprs
  .eval_expr <- function(ds_vars) {

    result <- NA

    rlang::catch_cnd(
      result <- eval_tidy(indicator_expr, data = ds_vars)
    )

    return(result)
  }

  # define rollify methods for evaluation
  .rollify <- function(.data, window, .f, ...,
                         unlist = TRUE, na_value = NULL) {

    roll_length <- NROW(.data)

    # initialize `output` vector
    output <- rlang::rep_along(1:roll_length, list(na_value))

    # get rolling result
    if (window <= roll_length) {
      for (i in window:roll_length) {
        f_data <- .data[(i - window + 1):i, ]
        output[[i]] <- .f(f_data, ...)
      }
    }

    # unlist result if request, except foratomic scalar
    if (unlist) {
      is_scalar_atomic <- purrr::map_lgl(output, rlang::is_scalar_atomic)
      if (all(is_scalar_atomic)) {
        output <- unlist(output)
      }
    }

    return(output)
  }

  # define indicator definition function
  def_fun <- function(ds_vars, ....) {

    # evaluate exprs in ds_vars
    if (rolly_window > 0) {
      # rolling evalattion
      ds_indicator <- .rollify(ds_vars,
        .eval_expr,
        window = rolly_window,
        unlist = TRUE,
        na_value = NA
      )
    } else {
      # normal evaluation
      ds_indicator <- .eval_expr(ds_vars)
    }


    # build result dataset
    ds_indicator <- tibble::tibble(!!indicator_name := ds_indicator)

    # validate result
    if (NROW(ds_vars) != NROW(ds_indicator)) {
      msg <- sprintf("length of indicator(%d) isn't equal to that of vars(%d)",
                     NROW(ds_indicator), NROW(ds_vars))
      stop(msg)
    }

    return(ds_indicator)
  }

  # return function for defining indicator
  return(def_fun)
}
