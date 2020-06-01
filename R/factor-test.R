
# Class Definination of factor_test classes--------------------------------------


setClassUnion("data.frameOrNull", c("data.frame", "NULL"))

# Abstract class of factor testing
setClass("factor_test",
  slots = list(
    summary = "data.frame",
    raw_result = "data.frameOrNull"
  ),
  contains = "VIRTUAL"
)

# Spficific classes of various factor testing

setClass("factor_test_uniregress",
  slots = list(factor_returns = "data.frame"),
  contains = "factor_test"
)

setClass("factor_test_IC",
  slots = list(factor_ICs = "data.frame"),
  contains = "factor_test"
)

setClass("factor_test_sort_portfolios",
  slots = list(
    factor_returns = "data.frame",
    portfolios_summary = "data.frame",
    portfolios_return = "data.frame"
  ),
  contains = "factor_test"
)


# Creating Functions of factor_test classes -------------------------------------------

#  univariate regression Test

#' Creator of factor_test_uniregress class
#'
#' Conduct univariate regression test for descriptors of factors and build object
#' of factor_test_uniregress class as output.
#'
#'
#'
#' @param ds_test          A timeseries dataset with descriptors of factors for test
#' @param regress_fun      a function to conduct regress.
#' @param ...              Arguments passed to regress_fun.
#' @param output_type      Type of output data, i.e."summary", "raw", if "raw",
#' raw data will be append to output object for diagnosis.
#' @param factor_field       Name of factor field of ds_test, by default "factor_name".
#' @param date_field         Name of date field of ds_test, by default "date",
#' Column must be date-like.
#'
#' @return                 A object of factor_test_uniregress class.
#'
#' @export
factor_test_uniregress <- function(ds_test,
                                   regress_fun,
                                   ...,
                                   output_type = c("summary", "raw"),
                                   factor_field = "factor_name",
                                   date_field = "date") {
  # Validate params
  stopifnot(!is.null(ds_test), inherits(ds_test, "data.frame"))
  ds_test_data <- tibble::as_tibble(ds_test)

  stopifnot(!is.null(regress_fun), inherits(regress_fun, "function"))

  factor_field <- rlang::parse_quo(factor_field, env = rlang::caller_env())
  date_field <- rlang::parse_quo(date_field, env = rlang::caller_env())

  # Nest test data by group of factor_name and date
  # cross section: group data by factor and date_field(cross section setting)
  ds_test_groupdata <- ds_test_data %>%
    dplyr::group_by(!!factor_field, !!date_field) %>%
    tidyr::nest()


  # Conduct factor regression test
  ds_test_result <- ds_test_groupdata %>%
    dplyr::mutate(
      model = purrr::map(data, purrr::possibly(regress_fun,
        otherwise = NULL,
        quiet = TRUE
      ), ...),
      glance = purrr::map(model, broom::glance),
      tidy = purrr::map(model, broom::tidy),
      augument = purrr::map2(model, data, broom::augment)
    )

  # Build Result summary dataset

  # Raw factor return data to process
  ds_factor_returns_raw <- ds_test_result %>%
    tidyr::unnest(tidy) %>%
    dplyr::filter(term != "(Intercept)")


  # Distribution summary of factor return series
  result_factor_return_distrbution <- ds_factor_returns_raw %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::summarise(
      obs = dplyr::n(),
      nas = sum(is.na(estimate)),
      avg = mean(estimate, na.rm = TRUE),
      med = median(estimate, na.rm = TRUE),
      min = min(estimate, na.rm = TRUE),
      max = max(estimate, na.rm = TRUE),
      std = sd(estimate, na.rm = TRUE),
      skew = PerformanceAnalytics::skewness(estimate, na.rm = TRUE),
      kurt = PerformanceAnalytics::kurtosis(estimate, na.rm = TRUE),
      pos_pct = mean(estimate >= 0, na.rm = TRUE),
      neg_pct = mean(estimate < 0, na.rm = TRUE),
      odds = ifelse((neg_pct != 0), pos_pct / neg_pct, NA)
    )


  # t-test for estimation of mean of factor return series
  result_factor_return_t.test <- ds_factor_returns_raw %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::do(broom::tidy(t.test(.$estimate))) %>%
    dplyr::select(
      !!factor_field,
      t.test_t = statistic,
      t.test_p = p.value
    )

  # Normal distribution test for factor return series
  result_factor_return_normal.test <- ds_factor_returns_raw %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::do(broom::tidy(shapiro.test(.$estimate))) %>%
    dplyr::select(
      !!factor_field,
      normal.test_p = p.value
    )

  # Build factor return series
  ds_factor_returns <- ds_factor_returns_raw %>%
    dplyr::select(!!factor_field, !!date_field, return = estimate) %>%
    # tidyr::spread(key = !!factor_field, value = return) %>%
    tidyr::pivot_wider(names_from = !!factor_field, values_from = return) %>%
    dplyr::arrange(!!date_field)


  # Integrate into result summary
  result_summary <- result_factor_return_distrbution %>%
    dplyr::left_join(result_factor_return_t.test,
      by = rlang::quo_text(factor_field)
    ) %>%
    dplyr::left_join(result_factor_return_normal.test,
      by = rlang::quo_text(factor_field)
    )


  # Build factor test object with results info
  output_type <- match.arg(output_type)
  if (output_type == "summary") {
    # include test result summary dataset
    result_object <- new("factor_test_uniregress",
      summary = result_summary,
      factor_returns = ds_factor_returns,
      raw_result = NULL
    )
  } else {
    # include test result summary dataset and test result raw dataset
    result_object <- new("factor_test_uniregress",
      summary = result_summary,
      factor_returns = ds_factor_returns,
      raw_result = ds_test_result
    )
  }

  return(invisible(result_object))
}

# Information Coefficients Test

#' Creator of factor_test_IC class
#'
#' Conduct information coefficients test for descriptors of factors and build object
#' of factor_test_IC class as output.
#'
#'
#'
#' @param ds_test          A timeseries dataset with descriptors of factors for test.
#' @param IC_fun           A function to compute information coefficients.
#' @param ...              Arguments passed to IC_fun.
#' @param output_type      Type of output data, i.e."summary", "raw", if "raw",
#' raw data will be append to output object for diagnosis.
#' @param factor_field       Name of factor field of ds_test, by default "factor_name".
#' @param date_field         Name of date field of ds_test, by default "date",
#' Column must be date-like.
#'
#' @return                 A object of factor_test_IC class.
#'
#' @export
factor_test_IC <- function(ds_test,
                           IC_fun,
                           ...,
                           output_type = c("summary", "raw"),
                           factor_field = "factor_name",
                           date_field = "date") {

  # Validate params
  stopifnot(!is.null(ds_test), inherits(ds_test, "data.frame"))
  ds_test_data <- tibble::as_tibble(ds_test)

  factor_field <- rlang::parse_quo(factor_field, env = rlang::caller_env())
  date_field <- rlang::parse_quo(date_field, env = rlang::caller_env())

  # Nest test data by group of factor_name and date as cross section data
  ds_test_groupdata <- ds_test_data %>%
    dplyr::group_by(!!factor_field, !!date_field) %>%
    tidyr::nest()


  # Compute IC for crossection data
  ds_test_result <- ds_test_groupdata %>%
    dplyr::mutate(
      model = purrr::map(data, purrr::possibly(IC_fun, otherwise = NULL, quiet = TRUE), ...),
      glance = purrr::map(model, broom::glance)
    )


  # Build Result summary dataset

  # Raw factor ICs data to process
  ds_factor_ICs_raw <- ds_test_result %>%
    tidyr::unnest(glance)

  # Distribution summary of factor ICs series
  result_factor_ICs_distrbution <- ds_factor_ICs_raw %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::summarise(
      obs = dplyr::n(),
      nas = sum(is.na(estimate)),
      avg = mean(estimate, na.rm = TRUE),
      med = median(estimate, na.rm = TRUE),
      min = min(estimate, na.rm = TRUE),
      max = max(estimate, na.rm = TRUE),
      std = sd(estimate, na.rm = TRUE),
      skew = PerformanceAnalytics::skewness(estimate, na.rm = TRUE),
      kurt = PerformanceAnalytics::kurtosis(estimate, na.rm = TRUE),
      pos_pct = mean(estimate >= 0, na.rm = TRUE),
      neg_pct = mean(estimate < 0, na.rm = TRUE),
      odds = ifelse((neg_pct != 0), pos_pct / neg_pct, NA)
    )

  # t-test for estimation of mean of factor IC series
  result_factor_ICs_t.test <- ds_factor_ICs_raw %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::do(broom::tidy(t.test(.$estimate))) %>%
    dplyr::select(
      !!factor_field,
      t.test_t = statistic,
      t.test_p = p.value
    )

  # Normal distribution test for factor IC series
  result_factor_ICs_normal.test <- ds_factor_ICs_raw %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::do(broom::tidy(shapiro.test(.$estimate))) %>%
    dplyr::select(
      !!factor_field,
      normal.test_p = p.value
    )

  # Build factor IC series
  ds_factor_ICs <- ds_factor_ICs_raw %>%
    dplyr::select(!!factor_field, !!date_field, IC = estimate) %>%
    # tidyr::spread(key = !!factor_field, value = IC) %>%
    tidyr::pivot_wider(names_from = !!factor_field, values_from = IC) %>%
    dplyr::arrange(!!date_field)


  # Integrate into result summary
  result_summary <- result_factor_ICs_distrbution %>%
    dplyr::left_join(result_factor_ICs_t.test,
      by = rlang::quo_text(factor_field)
    ) %>%
    dplyr::left_join(result_factor_ICs_normal.test,
      by = rlang::quo_text(factor_field)
    )


  # Build factor test object with results info
  output_type <- match.arg(output_type)
  if (output_type == "summary") {
    # include test result summary dataset
    result_object <- new("factor_test_IC",
      summary = result_summary,
      factor_ICs = ds_factor_ICs,
      raw_result = NULL
    )
  } else {
    # include test result summary dataset and test result raw dataset
    result_object <- new("factor_test_IC",
      summary = result_summary,
      factor_ICs = ds_factor_ICs,
      raw_result = ds_test_result
    )
  }

  return(invisible(result_object))
}



# Factor Sort Portfolios Test

#' Creator of factor_test_sort_portfolios class
#'
#' Conduct factor sort portfolios test for descriptors of factors and build object
#' of factor_test_sort_portfolios class as output.
#'
#'
#'
#' @param ds_test          A timeseries dataset with descriptors of factors for test.
#' @param sort_portfolios_fun  A function to sort descriptors of factors to build.
#' portfolios for test
#' @param ...              Arguments passed to sort_portfolios_fun.
#' @param output_type      Type of output data, i.e."summary", "raw", if "raw",
#' raw data will be append to output object for diagnosis.
#' @param factor_field     Name of factor field of ds_test, by default "factor_name"
#' @param date_field       Name of date field of ds_test, by default "date",
#' Column must be date-like.
#' @param stkcd_field      Name of stkcd field of ds_test, by default "stkcd".
#' @param return_field     Name of return field of ds_test, by default "return".
#'
#' @return                 A object of factor_test_sort_portfolios class.
#'
#' @export
factor_test_sort_portfolios <- function(ds_test,
                                        sort_portfolios_fun,
                                        ...,
                                        output_type = c("summary", "raw"),
                                        factor_field = "factor_name",
                                        date_field = "date",
                                        stkcd_field = "stkcd",
                                        return_field = "return") {


  # Method of computing croossectional return of sort portfolios
  .compute_crosssection_portfolios_return <- function(ds_crosssection, sort_portfolios) {

    # compute portfolio return for each sort portfolio
    portfolios_return <- sort_portfolios %>%
      dplyr::left_join(ds_crosssection, by = c("stkcd" = stkcd_field)) %>%
      dplyr::group_by(portfolio_group) %>%
      dplyr::summarise(return = sum((!!return_field) * weight, na.rm = TRUE)
      / sum(weight, na.rm = TRUE))

    return(portfolios_return)
  }

  # Method of summuarizing sort portoflios return
  .summarize_portfolios_return <- function(ds_portfolios_return) {
    col_names <- colnames(ds_portfolios_return)
    is_date_field <- col_names %in% rlang::quo_text(date_field)
    if (!any(is_date_field)) {
      stop("Can't find date colomns")
    }

    date_field_name <- col_names[is_date_field]
    portfilo_groups_name <- col_names[!is_date_field]
    ts_portfolios_return <- xts::xts(ds_portfolios_return[, portfilo_groups_name],
      order.by = ds_portfolios_return[, date_field_name][[1]]
    )

    # compute peformance indicators
    annual_returns <- PerformanceAnalytics::Return.annualized(ts_portfolios_return)
    anuual_std <- PerformanceAnalytics::StdDev.annualized(ts_portfolios_return)
    annual_sharpe <- PerformanceAnalytics::SharpeRatio.annualized(ts_portfolios_return)
    max_drawdown <- PerformanceAnalytics::maxDrawdown(ts_portfolios_return)


    # build summary table
    summary_table <- rbind(annual_returns, anuual_std, annual_sharpe, max_drawdown)
    summary_table <- as.data.frame(summary_table)
    summary_table <- tibble::rownames_to_column(summary_table, var = "indicator")

    return(summary_table)
  }


  # Validate params
  stopifnot(!is.null(ds_test), inherits(ds_test, "data.frame"))
  ds_test_data <- tibble::as_tibble(ds_test)

  factor_field <- rlang::parse_quo(factor_field, env = rlang::caller_env())
  date_field <- rlang::parse_quo(date_field, env = rlang::caller_env())
  return_field <- rlang::parse_quo(return_field, env = rlang::caller_env())

  # Nest test data by group of factor_name and date as cross section data
  ds_test_groupdata <- ds_test_data %>%
    dplyr::group_by(!!factor_field, !!date_field) %>%
    tidyr::nest()

  # Compute return for sort portfolios
  ds_test_result <- ds_test_groupdata %>%
    dplyr::mutate(
      sort_portfolios = purrr::map(
        data,
        purrr::possibly(sort_portfolios_fun, otherwise = NULL, quiet = TRUE),
        ...
      ),
      sort_portfolio_return = purrr::map2(
        .x = data, .y = sort_portfolios,
        .compute_crosssection_portfolios_return
      )
    )

  # Expand sort portfolios returns
  ds_portfolios_return_raw <- ds_test_result %>%
    dplyr::select(-c(data, sort_portfolios)) %>%
    tidyr::unnest(sort_portfolio_return) %>%
    # tidyr::spread(key = portfolio_group, value = return)
    tidyr::pivot_wider(names_from = portfolio_group, values_from = return)


  # Build zero-portfolio to complete portfolios return dataset
  # group_zero = group_hi - group_low
  ds_portfolios_return <- ds_portfolios_return_raw %>%
    dplyr::mutate(group_zero = group_hi - group_lo)


  # Build Result of portfolios return summary
  result_portfolios_summary <- ds_portfolios_return %>%
    dplyr::group_by(!!factor_field) %>%
    tidyr::nest() %>%
    dplyr::rename(portfolios_return = data) %>%
    dplyr::mutate(portfolios_return_summary = purrr::map(
      portfolios_return,
      .summarize_portfolios_return
    )) %>%
    dplyr::select(-c(portfolios_return)) %>%
    tidyr::unnest(portfolios_return_summary)


  # Build Result of summary

  # Distribution summary of zero-portfolio return series
  result_zero_portfolio_return_distrbution <- ds_portfolios_return %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::summarise(
      obs = dplyr::n(),
      nas = sum(is.na(group_zero)),
      avg = mean(group_zero, na.rm = TRUE),
      med = median(group_zero, na.rm = TRUE),
      min = min(group_zero, na.rm = TRUE),
      max = max(group_zero, na.rm = TRUE),
      std = sd(group_zero, na.rm = TRUE),
      skew = PerformanceAnalytics::skewness(group_zero, na.rm = TRUE),
      kurt = PerformanceAnalytics::kurtosis(group_zero, na.rm = TRUE),
      pos_pct = mean(group_zero >= 0, na.rm = TRUE),
      neg_pct = mean(group_zero < 0, na.rm = TRUE),
      odds = ifelse((neg_pct != 0), pos_pct / neg_pct, NA)
    )

  # t-test for estimation of mean of zero-portfolio return series
  result_zero_portfolio_return_t.test <- ds_portfolios_return %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::do(broom::tidy(t.test(.$group_zero))) %>%
    dplyr::select(
      !!factor_field,
      t.test_t = statistic,
      t.test_p = p.value
    )

  # Normal distribution test for zero-portfolio return series
  result_zero_portfolio_return_normal.test <- ds_portfolios_return %>%
    dplyr::group_by(!!factor_field) %>%
    dplyr::do(broom::tidy(shapiro.test(.$group_zero))) %>%
    dplyr::select(
      !!factor_field,
      normal.test_p = p.value
    )

  # Integrate into result summary
  result_summary <- result_zero_portfolio_return_distrbution %>%
    dplyr::left_join(result_zero_portfolio_return_t.test,
      by = rlang::quo_text(factor_field)
    ) %>%
    dplyr::left_join(result_zero_portfolio_return_normal.test,
      by = rlang::quo_text(factor_field)
    )

  # Build factor returns datasets from group_zero portfolio
  ds_factor_returns <- ds_portfolios_return %>%
    dplyr::select(!!factor_field, !!date_field, return = group_zero) %>%
    # tidyr::spread(key = !!factor_field, value = return)
    tidyr::pivot_wider(names_from = !!factor_field, values_from = return)



  # Build factor test object with results info
  output_type <- match.arg(output_type)
  if (output_type == "summary") {
    # include test result summary dataset
    result_object <- new("factor_test_sort_portfolios",
      summary = result_summary,
      factor_returns = ds_factor_returns,
      portfolios_summary = result_portfolios_summary,
      portfolios_return = ds_portfolios_return,
      raw_result = NULL
    )
  } else {
    # include test result summary dataset and test result raw dataset
    result_object <- new("factor_test_sort_portfolios",
      summary = result_summary,
      portfolios_summary = result_portfolios_summary,
      portfolios_return = ds_portfolios_return,
      raw_result = ds_test_result
    )
  }

  return(invisible(result_object))
}


#' Build sort portfoilos basing on group list of stocks
#'
#' use stocks list and factor value list to build sort portfoilos by sorting
#' factor value and cutting in n groups.
#'
#' @param stocks_list           A list of stkcds of stocks.
#' @param factor_value_list     A list of factor value of corresponding stocks.
#' @param stocks_weight_list    A list of stocks weight or NULL for equal weight of 1,
#' by default NULL.
#' @param ngroup                Numbers of groups to cut stocks, by default 5.
#'
#'
#' @return                 A tibble dataset of portfolio_group, stkcd, weight,
#'                         factor_value.
#'
#' @export
build_sort_portfolios <- function(stocks_list,
                                  factor_value_list,
                                  stocks_weight_list = NULL,
                                  ngroup = 5) {

  # Validate params
  stopifnot(!is.null(stocks_list), length(stocks_list) != 0)
  stopifnot(!is.null(factor_value_list), length(factor_value_list) != 0)
  stopifnot(ngroup >= 1)

  if (length(stocks_list) != length(factor_value_list)) {
    stop("length of stock_list is different with factor_value_list")
  }

  if (!is.null(stocks_weight_list)) {
    if (length(stocks_list) != length(stocks_weight_list)) {
      stop("length of stock_list is different with stocks_weight_list")
    }
  } else {
    # Set each weight as 1
    stocks_weight_list <- rep(1, length(stocks_list))
  }


  # create sort portfolios
  sort_portfolios <- tibble::tibble(
    stkcd = stocks_list,
    weight = stocks_weight_list,
    factor_value = factor_value_list
  )


  # Build factor groups

  # Notice: rank always return order numbers from smallest to largest
  factor_value_rank <- rank(sort_portfolios$factor_value)
  factor_groups <- cut(factor_value_rank, breaks = ngroup, ordered_result = TRUE)

  # set name of groups/levels
  levels_count <- nlevels(factor_groups)
  groups_name <- paste("group", 1:levels_count, sep = "_")
  groups_name[1] <- paste("group", "lo", sep = "_")
  groups_name[levels_count] <- paste("group", "hi", sep = "_")
  levels(factor_groups) <- groups_name

  # Finalize sort portfolios
  sort_portfolios <- sort_portfolios %>%
    dplyr::mutate(portfolio_group = factor_groups) %>%
    dplyr::arrange(portfolio_group, stkcd) %>%
    dplyr::group_by(portfolio_group) %>%
    dplyr::select(portfolio_group, stkcd, weight, factor_value)

  return(sort_portfolios)
}


# Generic Functions Implmentation for factor_test classes   ---------------------

# Generic Implementation of summary for factor_test class
# @export
summary.factor_test <- function(object, ...) {

  # Validate params
  stopifnot(!is.null(object), inherits(object, "factor_test"))

  # print(factor_test_result@summary)

  object@summary
}
setMethod(
  "summary",
  signature(object = "factor_test"),
  function(object, ...) {
    summary.factor_test(object, ...)
  }
)


# Generic Implementation of plot for factor_test class
# @export
plot.factor_test <- function(x, ...) {
  # validate params
  stopifnot(!is.null(x), inherits(x, "factor_test"))
}
setMethod(
  "plot",
  signature(x = "factor_test", y = "missing"),
  function(x, y, ...) {
    plot.factor_test(x, ...)
  }
)

# Generic Implementation of plot for factor_test_uniregress
# Plot result for factor_test_uniregress class
# @export
plot.factor_test_uniregress <- function(x, ...) {


  # Validate params
  stopifnot(
    !is.null(x),
    inherits(x, "factor_test_uniregress")
  )

  # Plot distribution of factors return
  .plot_distribution_factors_series(x@factor_returns,
    series_name = "factor return"
  )


  # Plot return summary

  invisible(return)
}
setMethod(
  "plot",
  signature(x = "factor_test_uniregress", y = "missing"),
  function(x, y, ...) {
    plot.factor_test_uniregress(x, ...)
  }
)

# Generic Implementation of plot for factor_test_uniregress
# Plot result for factor_test_uniregress class
# @export
plot.factor_test_IC <- function(x, ...) {


  # Validate params
  stopifnot(
    !is.null(x),
    inherits(x, "factor_test_IC")
  )

  # Plot Distribution of factors ICs
  .plot_distribution_factors_series(x@factor_ICs,
    series_name = "factor IC"
  )

  # Plot return summary

  invisible(return)
}
setMethod(
  "plot",
  signature(x = "factor_test_IC", y = "missing"),
  function(x, y, ...) {
    plot.factor_test_IC(x, ...)
  }
)


# Generic Implementation of plot for factor_test_sort_portfolios
# Plot result for factor_test_sort_portfolios
# @export
plot.factor_test_sort_portfolios <- function(x, ...) {
  .plot_portfolio_returns <- function(ds_portfolios_return, factor_name) {

    # validate params
    stopifnot(!is.null(ds_portfolios_return), inherits(ds_portfolios_return, "data.frame"))

    col_names <- colnames(ds_portfolios_return)
    is_date_field <- purrr::map_lgl(ds_portfolios_return, lubridate::is.Date)
    if (sum(is_date_field) != 1) {
      stop("Can't find date colomns or have multiple date columns")
    }
    date_field_name <- col_names[is_date_field]

    ds_portfolios_date <- ds_portfolios_return %>%
      dplyr::select(date_field_name)

    ds_portfolios_groups <- ds_portfolios_return %>%
      dplyr::select(contains("group")) %>%
      dplyr::select(group_zero, everything())


    # Convert to xts for plot
    ts_portfolios_return <- xts::xts(ds_portfolios_groups,
      order.by = ds_portfolios_date[[1]]
    )


    # charts.PerformanceSummary(edhec[,c(1,13)])

    # Plot performance of portfolios

    # Plot Performance Summary
    title <- sprintf("Perormance Summary(Factor:%s)", factor_name)
    PerformanceAnalytics::charts.PerformanceSummary(ts_portfolios_return,
      ylog = TRUE,
      method = "StdDev",
      main = title,
      colorset = PerformanceAnalytics::redfocus,
      legend.loc = "topleft",
      cex.axis = 1,
      cex.main = 1.2
    )

    # Plot Rolling Performance
    title <- sprintf("Rolling 12-Month Performance(Factor:%s)", factor_name)
    PerformanceAnalytics::charts.RollingPerformance(ts_portfolios_return,
      main = title,
      colorset = PerformanceAnalytics::redfocus,
      legend.loc = "topleft",
      cex.axis = 1,
      cex.main = 1.2
    )
  }

  # Validate params
  stopifnot(
    !is.null(x),
    inherits(x, "factor_test_sort_portfolios")
  )

  # Plot Distribution of factor returns
  .plot_distribution_factors_series(x@factor_returns,
    series_name = "factor return"
  )


  # Plot return of portfolios
  ds_portfolios_return <- x@portfolios_return %>%
    dplyr::group_by(factor_name) %>%
    tidyr::nest(.key = "returns")

  purrr::map2(
    ds_portfolios_return$returns,
    as.list(ds_portfolios_return$factor_name),
    .plot_portfolio_returns
  )

  # Plot return summary

  invisible(return)
}
setMethod(
  "plot",
  signature(x = "factor_test_sort_portfolios", y = "missing"),
  function(x, y, ...) {
    plot.factor_test_sort_portfolios(x, ...)
  }
)

# Plot distribution for factor series
.plot_distribution_factors_series <- function(ds_factors_series,
                                              series_name = "factor return") {

  # Validate params
  stopifnot(!is.null(ds_factors_series), inherits(ds_factors_series, "data.frame"))

  col_names <- colnames(ds_factors_series)
  is_date_field <- purrr::map_lgl(ds_factors_series, lubridate::is.Date)
  date_field_numbers <- sum(is_date_field)
  is_ds_timeseries <- FALSE
  if (date_field_numbers == 0) {
    is_ds_timeseries <- FALSE
  } else if (date_field_numbers == 1) {
    is_ds_timeseries <- TRUE
  } else {
    stop("Too many date columns, can't determine which to use")
  }

  # Plot factors timeseries
  if (is_ds_timeseries) {
    date_field_name <- col_names[is_date_field]
    ds_series_date <- ds_factors_series %>%
      dplyr::select(date_field_name)

    factors_field_name <- col_names[!is_date_field]
    ds_series_factors <- ds_factors_series %>%
      dplyr::select(factors_field_name)


    # Convert to xts for plot
    ts_factors_return <- xts::xts(ds_series_factors,
      order.by = ds_series_date[[1]]
    )

    # Plot Boxplot for distribution of factors series
    title <- "Distribution of Factors Return"
    title <- paste("Distribution of", series_name)
    PerformanceAnalytics::chart.Boxplot(ts_factors_return,
      sort.by = "median",
      main = title,
      cex.axis = 1,
      xlab = series_name,
      cex.main = 1.2
    )


    # Plot Histogram for distribtion of factors series
    plyr::l_ply(ts_factors_return,
      PerformanceAnalytics::chart.Histogram,
      methods = c("add.density", "add.normal", "add.rug", "add.qqplot"),
      show.outliers = TRUE,
      xlab = series_name,
      .progress = plyr::progress_win(title = "Working...")
    )
  }
}
