
plot.factor_test <- function(factor_test_result){

  # validate params
  stopifnot(!is.null(factor_test_result), inherits(factor_test_result, "factor_test"))

}

# Generic Implementation of summary for factor_test class
summary.factor_test <- function(factor_test_result){

  # validate params
  stopifnot(!is.null(factor_test_result), inherits(factor_test_result, "factor_test"))

  print(factor_test_result$summary)

}


# Univarate reggression test
factor_test_uniregress <- function(ds_test,
                                   regress_method = c("cross_section", "pooling"),
                                   regress_fun,
                                   ...,
                                   output_type = c("summary", "raw"),
                                   factor_field = "factor_name",
                                   date_field = "date") {
  # Validate params
  stopifnot(!is.null(ds_test), inherits(ds_test, "data.frame"))
  ds_test_data <- tibble::as.tibble(ds_test)

  stopifnot(!is.null(regress_fun), inherits(regress_fun, "function"))

  factor_field <- rlang::parse_quosure(factor_field)
  date_field <- rlang::parse_quosure(date_field)

  # Nest test data by group of factor_name and date
  regress_method <- match.arg(regress_method)
  if (regress_method == "cross_section") {
    #cross section: group data by factor and date_field(cross section setting)
    ds_test_groupdata <- ds_test_data %>%
      dplyr::group_by(!!factor_field, !!date_field) %>%
      tidyr::nest()
  } else {
    # pooling: group data by factor, no crossetion
    ds_test_groupdata <- ds_test_data %>%
      dplyr::group_by(!!factor_field) %>%
      tidyr::nest()
  }

  # Conduct factor regression test
  ds_test_result <- ds_test_groupdata %>%
    dplyr::mutate(
      model = purrr::map(data, purrr::possibly(regress_fun, otherwise = NULL, quiet = TRUE), ...),
      glance = purrr::map(model, broom::glance),
      tidy = purrr::map(model, broom::tidy),
      augument = purrr::map2(model, data, broom::augment)
    )

  # Build test results set
  output_type <- match.arg(output_type)
  if (output_type == "summary") {
    # result summary dataset

    # Result about significance of model
    result_significance_model <- ds_test_result %>%
      tidyr::unnest(glance, .drop = TRUE) %>%
      dplyr::group_by(!!factor_field) %>%
      dplyr::summarise_all(.funs = mean, na.rm = TRUE) %>%
      dplyr::select(!!factor_field, r.squared, f_pvalue = p.value)

    # Result about significance of coefficients
    result_significance_coefficient <- ds_test_result %>%
      tidyr::unnest(tidy, .drop = TRUE) %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::select(-term) %>%
      dplyr::group_by(!!factor_field) %>%
      dplyr::summarise(
        beta_t_abs_mean = mean(abs(statistic), na.rm = TRUE),
        beta_t_sig_ratio = sum(abs(statistic) >= 2, na.rm = TRUE) / n(),
        beta_t_mean = mean(statistic, na.rm = TRUE),
        beta_t_sd = sd(statistic, na.rm = TRUE),
        beta_t_mean_std_ratio = beta_t_mean / beta_t_sd
      )

    # Result about significance of factor return
    if (regress_method == "cross_section") {

      # raw factor return data to process
      ds_factor_returns_raw <- ds_test_result %>%
        tidyr::unnest(tidy, .drop = TRUE) %>%
        dplyr::filter(term != "(Intercept)")

      # cross section: conduct t-test for estimated factor return series
      result_significance_factor_return <- ds_factor_returns_raw %>%
        dplyr::group_by(!!factor_field) %>%
        dplyr::do(broom::tidy(t.test(.$estimate))) %>%
        dplyr::select(
          !!factor_field,
          factor_return = estimate,
          factor_return_tvalue = statistic,
          factor_return_pvalue = p.value
        )

      # build factor return series
      ds_factor_returns <- ds_factor_returns_raw %>%
        dplyr::select(!!factor_field,!!date_field, return = estimate) %>%
        tidyr::spread(key = !!factor_field, value = return) %>%
        dplyr::arrange(!!date_field)

    } else {
      # pooling: use estitmation of factor return from regression as result
      result_significance_factor_return <- ds_test_result %>%
        tidyr::unnest(tidy, .drop = TRUE) %>%
        dplyr::filter(term != "(Intercept)") %>%
        dplyr::select(
          !!factor_field,
          factor_return = estimate,
          factor_return_tvalue = statistic,
          factor_return_pvalue = p.value
        )

      # build factor return series
      ds_factor_returns <- result_significance_factor_return %>%
        dplyr::select(-factor_return_tvalue, -factor_return_pvalue)%>%
        tidyr::spread(key = !!factor_field, value = factor_return)
    }

    # Integrate regression result
    result_summary <- result_significance_model %>%
      dplyr::left_join(result_significance_coefficient,
                       by = rlang::quo_text(factor_field)) %>%
      dplyr::left_join(result_significance_factor_return,
                       by = rlang::quo_text(factor_field))

    result_object <- structure(list(summary = result_summary,
                                    factor_returns = ds_factor_returns ),
                               class = c("factor_test_uniregress", "factor_test"))

    ds_result <- result_object


  } else {
    # raw result dataset
    ds_result <- ds_test_result
  }

  return(ds_result)

}

# Information Coefficients test
factor_test_IC <- function(ds_test,
                           IC_fun,
                           ...,
                           output_type = c("summary", "raw"),
                           factor_field = "factor_name",
                           date_field = "date") {

  # Validate params
  stopifnot(!is.null(ds_test), inherits(ds_test, "data.frame"))
  ds_test_data <- tibble::as.tibble(ds_test)

  factor_field <- rlang::parse_quosure(factor_field)
  date_field <- rlang::parse_quosure(date_field)

  # Nest test data by group of factor_name and date as cross section data
  ds_test_groupdata <- ds_test_data %>%
      dplyr::group_by(!!factor_field, !!date_field) %>%
      tidyr::nest()


  # Compute IC for crossection data
  ds_test_result <- ds_test_groupdata %>%
    dplyr::mutate(model = purrr::map(data, purrr::possibly(IC_fun, otherwise = NULL, quiet = TRUE), ...),
                  glance = purrr::map(model, broom::glance))

  # Build test results set
  output_type <- match.arg(output_type)
  if (output_type == "summary") {
    # Result summary dataset

    # Result about significance of model
    result_model <- ds_test_result %>%
      tidyr::unnest(glance, .drop = TRUE)

    # Summary of result
    result_summary <- result_model %>%
      dplyr::group_by(!!factor_field) %>%
      dplyr::summarise(IC_mean = mean(statistic, na.rm = TRUE),
                       IC_std  =  sd(statistic, na.rm = TRUE),
                       IR = IC_mean / IC_std,
                       IC_positive_ratio = sum(statistic >= 0, na.rm = TRUE) / n(),
                       IC_avg_pvalue = mean(p.value, na.rm = TRUE))

    result_object <- structure(list(summary = result_summary),
                               class = c("factor_test_IC", "factor_test"))

    ds_result <- result_object

  } else {
    # Raw result dataset
    ds_result <- ds_test_result

  }

  return(ds_result)

}


# Build sort portfoilos basing on group list of stocks
build_sort_portfolios <- function(stocks_list,
                                  factor_value_list,
                                  ngroup = 3,
                                  first_group_index = 1,
                                  factor_group_order = c("desc", "asc")){

  # Validate params
  stopifnot(!is.null(stocks_list), length(stocks_list) != 0)
  stopifnot(!is.null(factor_value_list), length(factor_value_list) != 0)
  stopifnot(ngroup >= 1)

  if (length(stocks_list) != length(factor_value_list)) {
    stop("length of stock_list is different with factor_value_list")
  }

  # create sort portfolios
  sort_portfolios <- tibble::tibble( stkcd = stocks_list,
                                     factor_value = factor_value_list)


  # build factor groups
  # Notice: rank always return order numbers from smallest to largest
  factor_value_rank <- rank(sort_portfolios$factor_value)
  factor_groups <- cut(factor_value_rank, breaks = ngroup, ordered_result = TRUE)

  # Set the name of group by asc/desc order number(group_1...group_N)
  # by sorting factor value.
  # Since the order of factor is from small to large, so we need
  # build group name basing on the sort order
  # asc:  group_1(smallest factor) ... group_N(largest factor)
  # desc: group_1(largest factor)  ... group_N(smallest factor)
  #
  factor_group_order = match.arg(factor_group_order)
  if (factor_group_order == "asc") {
    levels(factor_groups) <- paste("group",
                                   first_group_index:(first_group_index + nlevels(factor_groups) - 1),
                                   sep = "_")
  } else {
    levels(factor_groups) <- paste("group",
                                   ((first_group_index + nlevels(factor_groups) - 1):first_group_index),
                                   sep = "_")
  }

  # Finalize sort portfolios
  sort_portfolios <- sort_portfolios %>%
    dplyr::mutate(portfolio_group = as.character(factor_groups)) %>%
    dplyr::arrange(portfolio_group, stkcd) %>%
    dplyr::group_by(portfolio_group) %>%
    dplyr::mutate(weight = 1 / n() ) %>%
    dplyr::select(portfolio_group, stkcd, weight, factor_value)

  return(sort_portfolios)

}



# Factor Sort Portfolios Test
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
      dplyr::summarise(return = sum((!!return_field) * weight, na.rm = TRUE))

    return(portfolios_return)

  }

  # Method of summuarizing sort portoflios return
  .summarize_portfolios_return <- function(ds_portfolios_return) {

    col_names <- colnames(ds_portfolios_return)
    is_date_field = col_names %in% quo_text(date_field)
    if (!any(is_date_field)) {
      stop("Can't find date colomns")
    }

    date_field_name <- col_names[is_date_field]
    portfilo_groups_name <- col_names[!is_date_field]
    ts_portfolios_return <- xts::xts(ds_portfolios_return[, portfilo_groups_name],
                                     order.by = ds_portfolios_return[, date_field_name][[1]])

    # compute peformance indicators
    annual_returns <- PerformanceAnalytics::Return.annualized(ts_portfolios_return)
    anuual_std <- PerformanceAnalytics::StdDev.annualized(ts_portfolios_return)
    annual_sharpe <- PerformanceAnalytics::SharpeRatio.annualized(ts_portfolios_return)
    max_drawdown <- PerformanceAnalytics::maxDrawdown(ts_portfolios_return)

    #z test for zero portfolio
    ttest_result_list <- ts_portfolios_return %>%
      plyr::llply(t.test)

    ttest_result_df <- ttest_result_list %>%
      plyr::ldply(function(x) tibble::tibble(t_estimate = x$estimate,
                                             t_statistic = as.numeric(x$statistic),
                                             t_pvalue = as.numeric(x$p.value)))

    ttest_result_df <- ttest_result_df %>%
           tidyr::gather(key="indicator", value="value",-.id) %>%
           tidyr::spread(key = .id, value = value)

    #build summary table
    summary_table <- rbind(annual_returns, anuual_std, annual_sharpe, max_drawdown)
    summary_table <- as.data.frame(summary_table)
    summary_table <- tibble::rownames_to_column(summary_table, var = "indicator")

    summary_table <- rbind(summary_table, ttest_result_df)

    return(summary_table)
  }


  # Validate params
  stopifnot(!is.null(ds_test), inherits(ds_test, "data.frame"))
  ds_test_data <- tibble::as.tibble(ds_test)

  factor_field <- rlang::parse_quosure(factor_field)
  date_field <- rlang::parse_quosure(date_field)
  return_field <- rlang::parse_quosure(return_field)

  # Nest test data by group of factor_name and date as cross section data
  ds_test_groupdata <- ds_test_data %>%
    dplyr::group_by(!!factor_field, !!date_field) %>%
    tidyr::nest()

  # Compute return for sort portfolios
  ds_test_result <- ds_test_groupdata %>%
    dplyr::mutate(
      sort_portpolios = purrr::map(data,
                                   purrr::possibly(sort_portfolios_fun, otherwise = NULL, quiet = TRUE),
                                   ...),
      sort_portpolio_return = purrr::map2(.x = data,
                                          .y = sort_portpolios,
                                          .f = .compute_crosssection_portfolios_return)
    )

  # Expand sort portfolios returns
  ds_test_result_raw <- ds_test_result %>%
    tidyr::unnest(sort_portpolio_return,.drop = TRUE) %>%
    tidyr::spread(key = portfolio_group, value = return)

  # Build zero-portfolio
  col_names <- colnames(ds_test_result_raw)
  max_group_id <- length(na.omit(stringr::str_extract(col_names,"group")))
  last_group_name <- paste("group", max_group_id, sep = "_")
  last_group <- rlang::parse_quosure(last_group_name)
  first_group <- rlang::parse_quosure("group_1")

  ds_test_result <- ds_test_result_raw %>%
    dplyr::mutate(group_zero = (!!first_group) - (!!last_group))

  # Build test results set for return
  output_type <- match.arg(output_type)
  if (output_type == "summary") {
    # Result summary dataset

    ds_test_result_summary <- ds_test_result %>%
      dplyr::group_by(!!factor_field) %>%
      tidyr::nest(.key = "portpolios_return")

    result_summary <- ds_test_result_summary %>%
      dplyr::mutate(portpolios_return_summary = purrr::map(portpolios_return, .summarize_portfolios_return)) %>%
      tidyr::unnest(portpolios_return_summary,.drop = TRUE)

    result_object <- structure(list(summary = result_summary, portfolios_return = ds_test_result ),
                             class = c("factor_test_sort_portfolios", "factor_test"))

    ds_result <- result_object
  } else {
    # Raw result dataset
    ds_result <- ds_test_result_raw

  }

  return(ds_result)
}


# Generic Implementation of plot for factor_test_uniregress
#
# Plot result for factor_test_uniregress
plot.factor_test_uniregress <- function(factor_test_result){

  .plot_returns <- function(ds_factors_return){

    # validate params
    stopifnot(!is.null(ds_factors_return), inherits(ds_factors_return, "data.frame"))

    col_names <- colnames(ds_factors_return)
    is_date_field = purrr::map_lgl(ds_factors_return, lubridate::is.Date)
    date_field_numbers <- sum(is_date_field)
    is_ds_timeseries <- FALSE
    if (date_field_numbers == 0) {
      is_ds_timeseries = FALSE
    } else if (date_field_numbers == 1) {
      is_ds_timeseries = TRUE
    } else {
      stop("Too many date columns, can't determine which to use")
    }

    if (is_ds_timeseries) {
      # plot timeseries of factors return
      date_field_name <- col_names[is_date_field]
      ds_factors_return_date <- ds_factors_return %>%
        dplyr::select(date_field_name)

      factors_field_name <- col_names[!is_date_field]
      ds_factors_return_factors <- ds_factors_return %>%
        dplyr::select(factors_field_name)


      # Convert to xts for plot
      ts_factors_return <- xts::xts(ds_factors_return_factors,
                                       order.by = ds_factors_return_date[[1]])

      # Plot distribution of factors
      title <- "Distribution of Factors Return"
      PerformanceAnalytics::chart.Boxplot(ts_factors_return,
                                           sort.by = "median",
                                           main = title,
                                           cex.axis = 1,
                                           cex.main = 1.2)



      plyr::l_ply(ts_factors_return,
             PerformanceAnalytics::chart.Histogram,
             methods = c("add.density", "add.normal", "add.rug", "add.qqplot"),
             show.outliers = TRUE,
             .progress = plyr::progress_win(title = "Working..."))

      # lapply(ts_factors_return,
      #        PerformanceAnalytics::chart.Histogram,
      #        methods = c("add.density", "add.normal", "add.rug", "add.qqplot"),
      #        show.outliers = TRUE)

      # Plot Performance Summary
      # title <- "Perormance Summary of Factors"
      # PerformanceAnalytics::charts.PerformanceSummary(ts_factors_return,
      #                                                 ylog = FALSE,
      #                                                 method = "StdDev",
      #                                                 main = title,
      #                                                 legend.loc = "topleft",
      #                                                 cex.axis = 1,
      #                                                 cex.main = 1.2)
      # # Plot Rolling Performance
      # title <- "Rolling 12-Month Performance of Factors"
      # charts.RollingPerformance(ts_factors_return,
      #                           main = title,
      #                           legend.loc = "topleft",
      #                           cex.axis = 1,
      #                           cex.main = 1.2)

    } else {

      # plot non-timeseries of factor return

    }


  }

  # validate params
  stopifnot(!is.null(factor_test_result),
            inherits(factor_test_result, "factor_test_uniregress"))

  # plot return of factors
   .plot_returns(factor_test_result$factor_returns)

  # plot return summary

  invisible(return)
}

# Generic Implementation of plot for test_sort_portfolios
#
# Plot result for test_sort_portfolios
plot.factor_test_sort_portfolios <- function(factor_test_result){

  .plot_returns <- function(ds_portfolios_return, factor_name){

    # validate params
    stopifnot(!is.null(ds_portfolios_return), inherits(ds_portfolios_return, "data.frame"))

    col_names <- colnames(ds_portfolios_return)
    is_date_field = purrr::map_lgl(ds_portfolios_return, lubridate::is.Date)
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
                                     order.by = ds_portfolios_date[[1]])


    # charts.PerformanceSummary(edhec[,c(1,13)])

    # Plot performance of portfolios

    # Plot Performance Summary
    title = sprintf("Perormance Summary(Factor:%s)", factor_name)
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
    title = sprintf("Rolling 12-Month Performance(Factor:%s)", factor_name)
    charts.RollingPerformance(ts_portfolios_return,
                              main = title,
                              colorset = PerformanceAnalytics::redfocus,
                              legend.loc = "topleft",
                              cex.axis = 1,
                              cex.main = 1.2)

  }

  # validate params
  stopifnot(!is.null(factor_test_result),
            inherits(factor_test_result, "factor_test_sort_portfolios"))

  # plot return of portfolios
  ds_portfolios_return <- factor_test_result$portfolios_return %>%
       dplyr::group_by(factor_name) %>%
       tidyr::nest(.key = "returns")

  purrr::map2(ds_portfolios_return$returns,
              as.list(ds_portfolios_return$factor_name),
              .plot_returns)

  # plot return summary

  invisible(return)
}




