
# Load factors UI
#' @export
loadFactorsUI <- function(id, factors_list) {

  ns <- shiny::NS(id)

  shiny::tagList(

    #   sidebarPanel(
    #   selectInput(inputId = ns("factors"), label = strong("Factors:"),
    #               choices = unique(factors_list),
    #               multiple = TRUE),
    #
    #   actionButton(inputId = ns("load_factors"), label = "Load Factors")
    #
    # ),
    #
    # mainPanel(
    #    DT::dataTableOutput(outputId = ns("factors_table"))
    # )

    shinydashboard::box(status = "primary",
        title = "Parameters",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,

        shiny::selectInput(inputId = ns("factors"),
                           label = shiny::strong("Factors:"),
                           choices = unique(factors_list),
                           multiple = TRUE),

        shiny::actionButton(inputId = ns("load_factors"),
                     label = shiny::strong("Load Factors")
        )
    ),

    DT::dataTableOutput(outputId = ns("factors_table"))
   )
}

# Load factors Server Function
#' @export
loadFactors <- function(input, output, session) {

  # load factors
  load_factors <- shiny::reactive({

    shiny::req(input$load_factors)

    factors_list <- shiny::isolate(shiny::req(input$factors))

    shiny::withProgress(message = 'Load Factors', value = 0, {

      shiny::incProgress(message = "open_stock_db")

      # Open gta stock databse
      stock_db <- stock_db(gta_db, "GTA_SQLData")
      open_stock_db(stock_db)

      shiny::incProgress(message = "init_stock_db")

      # Initate the stock database
      invisible(init_stock_db(stock_db))

      shiny::incProgress(message = "get_factor_indicator")

      # Get fundamental indicators
      fundamental_factors <- get_factor_indicator(stock_db,
                                                  factor_list = factors_list)

      # fundamental_factors <- readRDS("./../../data-raw/fundamental_factors.rds")

      close_stock_db(stock_db)


    })

    shiny::showNotification("Load factors successfully.")

     return(fundamental_factors)

  })

  # Display loaded factors
  output$factors_table <- DT::renderDataTable(
    DT::datatable(
      load_factors(), options = list(
        columnDefs = list(list(className = 'dt-center')),
        pageLength = 25
      )
    )
  )

  return(load_factors)
}

# Normalize factors UI
#' @export
normalizeFactorsUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::box(
      status = "primary",
      title = "Parameters",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectInput(
            inputId = ns("clean_extremes_method"),
            label = shiny::strong("Clean extremes method:"),
            choices = c("sigma", "mad")          ),

          shiny::sliderInput(inputId = ns("n_sigma"),
                             label = shiny::strong("n Sigmas:"),
                             min = 1,
                             max = 5,
                             value = 3, step = 1),

          shiny::sliderInput(inputId = ns("n_dmad"),
                             label = shiny::strong("n dmads:"),
                             min = 1,
                             max = 5,
                             value = 3, step = 1),
          shiny::selectInput(inputId = ns("value_replace_extremes"),
                             label = shiny::strong("value to replace extremes:"),
                             choices = c("limit", "NA")
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectInput(inputId = ns("standard_method"),
                             label = shiny::strong("Standardize Method:"),
                             choices = c("normal", "rank")
          )
        )
      ),
      shiny::actionButton(inputId = ns("normalize_factors"),
                          label = shiny::strong("Normalize Factors")
      )

    ),

    DT::dataTableOutput(outputId = ns("normalize_factors_table"))
  )
}

# Normalize factors Server Function
#' @export
normalizeFactors <- function(input, output, session, ds_factors) {

  # Normalize factors
  normalized_factors <- shiny::reactive({

    shiny::req(input$normalize_factors)

    ds_factors <- ds_factors()

    shiny::showNotification("Normalizing factors will take few minutes...")

    shiny::isolate({


      if (input$clean_extremes_method == "sigma") {
        extra_arguments = list(n_sigma = input$n_sigma)
      } else {
        extra_arguments = list(n_dmad = input$n_dmad)
      }

      normalized_factors <- normalize_factors(ds_factors,
                                              factors_list = NULL,
                                              group_by = c("date"),
                                              clean_extremes_method = input$clean_extremes_method,
                                              standard_method = input$standard_method,
                                              extreme_value = input$value_replace_extremes,
                                              !!!extra_arguments)

    })

    # normalized_factors <- readRDS("./../../data-raw/fundamental_normal_factors.rds")


    shiny::showNotification("Normalize factors successfully.")

    return(normalized_factors)
  })

  # Display loaded factors
  output$normalize_factors_table <- DT::renderDataTable(
    DT::datatable(
      normalized_factors(), options = list(
        columnDefs = list(list(className = 'dt-center')),
        pageLength = 25
      )
    )
  )

  return(normalized_factors)
}

# Explore factors distribution UI
#' @export
exploreFactorsDistributionUI <- function(id, ds_factors) {
  ns <- shiny::NS(id)

  shiny::tagList(
      #Side panel for input
      shiny::sidebarPanel(
        shiny::radioButtons(inputId = ns("date_type"),
                            label = shiny::strong("Date Range"),
                            choices = list("Single Period" = "single_period",
                                          "Multi Periods" = "multi_period"),
                            selected = "single_period"),

        shiny::conditionalPanel(
          condition = "input.date_type == 'single_period'",
          shiny::sliderInput(inputId = ns("report_date"),
                             label = shiny::strong("report Date:"),
                             min = lubridate::as_date("2017-12-31"),
                             max = lubridate::as_date("2017-12-31"),
                             value = lubridate::as_date("2017-12-31"),
                             step = NULL,
                             timeFormat = "%F"),
          ns = ns
        ),

        shiny::conditionalPanel(
          condition = "input.date_type == 'multi_period'",
          shiny::selectInput(inputId = ns("start_date"),
                             label = shiny::strong("Start Date:"),
                             choices = ""),

          shiny::selectInput(inputId = ns("end_date"),
                             label = shiny::strong("End Date:"),
                             choices = ""),
          ns = ns
        ),

        shiny::selectInput(inputId = ns("plot_type"),
                    label = shiny::strong("Plot Type:"),
                    choices = c("boxplot", "violin", "density", "histogram", "freqpoly"),
                    selected = "boxplot"),

        selectInput(inputId = ns("factors"),
                    label = strong("Factors:"),
                    choices = "", multiple = TRUE),

        shiny::sliderInput(inputId = ns("bins_adjust"),
                           label = "Bins for Histogram/freqpoly:",
                           min = 5, max = 100, value = 30, step = 1)
      ),

      # MainPanel for Output
      shiny::mainPanel(
        shiny::titlePanel(shiny::textOutput(outputId = ns("caption"))),
        shiny::plotOutput(outputId = ns("factor_compare_plot")),
        shiny::tableOutput(outputId = ns("factor_compare_table"))
      )
  )

}

# Explore factors distrubtion Server Function
#' @export
exploreFactorsDistribution <- function(input, output, session, ds_factors) {

  ds_transform_factors <- shiny::reactive({
    ds_factors <- ds_factors()
    ds_transform_factors <- ds_factors %>%
      tidyr::gather(key = "factor_name", value = "factor_exposure", -(date:indcd))

    return(ds_transform_factors)
  })

  # Update UI base ds_factors
  shiny::observe({

    ds_factors <- ds_transform_factors()

    shiny::updateSliderInput(session = session, inputId = "report_date",
                      min = min(unique(ds_factors$date)),
                      max = max(unique(ds_factors$date)),
                      value = max(unique(ds_factors$date)))

    shiny::updateSelectInput(session = session, inputId = "start_date",
                choices = unique(ds_factors$date),
                selected = max(unique(ds_factors$date)))

    shiny::updateSelectInput(session = session, inputId = "end_date",
                choices = unique(ds_factors$date),
                selected = max(unique(ds_factors$date)))

    shiny::updateSelectInput(session = session, inputId = "factors",
                      choices = c("ALL", unique(ds_factors$factor_name)),
                      selected = "ALL")

  })

  # Select date range
  select_date_range <- shiny::reactive({

    ds_factors <- ds_transform_factors()

    date_type <- input$date_type

    switch(date_type,
           'single_period' = {
             report_dates <- sort(unique(ds_factors$date))
             start_date <- min(report_dates[report_dates >= input$report_date])
             end_date <- start_date
             start_date <- format(start_date, "%Y-%m-%d")
             end_date <- format(end_date, "%Y-%m-%d")
           },
           'multi_period' = {
             start_date <- input$start_date
             end_date <- input$start_date
             if (end_date < start_date) end_date <- start_date
           })

    cat(file = stderr(), "selected date:", start_date, ":",
        end_date,"\n")

    return(list(start_date = start_date, end_date = end_date))

  })

  # Filter data of factors
  select_dataset <-shiny::reactive({

    ds_factors <- ds_transform_factors()

    select_factors <- input$factors
    if (all(select_factors %in% "ALL")) {
      select_factors <- unique(ds_factors$factor_name)
    }


    ds_select_factors <- ds_factors %>%
      dplyr::filter(date >= select_date_range()$start_date &
                      date <= select_date_range()$end_date &
                      factor_name %in% !!select_factors )

  })


  # Render captions
  output$caption <- shiny::renderText({

    start_date <- select_date_range()$start_date
    end_date <- select_date_range()$end_date

    caption <- sprintf("Factors Distribution(%s~%s)", start_date, end_date)

    return(caption)
  })

  # plot comparsion plot
  output$factor_compare_plot <- shiny::renderPlot({
    ds_select_factors <- select_dataset()

    # plot the select data
    base_plot <- ggplot(ds_select_factors)
    result_plot <- switch(
      input$plot_type,
      boxplot = base_plot + geom_boxplot(aes(x = factor_name, y = factor_exposure)),
      violin  = base_plot + geom_violin(aes(x = factor_name, y = factor_exposure)),
      density  = base_plot + geom_density(aes(x = factor_exposure, colour = factor_name)),
      histogram  = base_plot + geom_histogram(
        aes(x = factor_exposure, fill = factor_name),
        alpha = 0.4,
        bins =  input$bins_adjust
      ),
      freqpoly  = base_plot + geom_freqpoly(
        aes(x = factor_exposure, colour = factor_name),
        binwidth = input$bins_adjust
      )
    )
    return(result_plot)
  })

  # summarize comparison result
  output$factor_compare_table <- shiny::renderTable({
    ds_select_factors <- select_dataset()

    select_factors_summary <- ds_select_factors %>%
      dplyr::group_by(factor_name) %>%
      dplyr::summarise_at(
        "factor_exposure",
        dplyr::funs(
          NAs = sum(is.na(.)),
          mean,
          sd,
          median,
          mad,
          min,
          Q1 = quantile(., 0.25),
          Q3 = quantile(., 0.75),
          max,
          skewness = PerformanceAnalytics::skewness,
          kurtosis = PerformanceAnalytics::kurtosis(., method="moment")
        ),
        na.rm = TRUE
      )

    return(select_factors_summary)

  })
}

# Explore factors correlation UI
#' @export
exploreFactorsCorrelationUI <- function(id, ds_factors) {
  ns <- shiny::NS(id)

  shiny::tagList(
    #Side panel for input
    shiny::sidebarPanel(
      shiny::radioButtons(inputId = ns("date_type"),
                          label = shiny::strong("Date Range"),
                          choices = list("Single Period" = "single_period",
                                        "Multi Periods" = "multi_period"),
                          selected = "single_period"),

      shiny::conditionalPanel(
        condition = "input.date_type == 'single_period'",
        shiny::sliderInput(inputId = ns("report_date"),
                           label = shiny::strong("report Date:"),
                           min = lubridate::as_date("2017-12-31"),
                           max = lubridate::as_date("2017-12-31"),
                           value = lubridate::as_date("2017-12-31"), step = NULL,
                           timeFormat = "%F"),
        ns = ns
      ),

      shiny::conditionalPanel(
        condition = "input.date_type == 'multi_period'",
        shiny::selectInput(inputId = ns("start_date"),
                           label = shiny::strong("Start Date:"),
                           choices = ""),

        shiny::selectInput(inputId = ns("end_date"),
                           label = shiny::strong("End Date:"),
                           choices = ""),
        ns = ns
      ),

      shiny::selectInput(inputId = ns("factors"),
                         label = shiny::strong("Factors:"),
                         choices = "",
                         multiple = TRUE)


    ),

    # MainPanel for Output
    shiny::mainPanel(
      shiny::titlePanel(shiny::textOutput(outputId = ns("caption"))),
      shiny::plotOutput(outputId = ns("factor_correlation_plot")),
      shiny::verbatimTextOutput(outputId = ns("factor_corelation_table"))
    )
  )
}

# Explore factors correlation Server Function
#' @export
exploreFactorsCorrelation <- function(input, output, session, ds_factors) {

  ds_transform_factors <- shiny::reactive({
    ds_factors <- ds_factors()
    ds_transform_factors <- ds_factors %>%
      tidyr::gather(key = "factor_name", value = "factor_exposure", -(date:indcd))

    return(ds_transform_factors)
  })

  # Update UI base ds_factors
  shiny::observe({

    ds_factors <- ds_transform_factors()

    shiny::updateSliderInput(session = session, inputId = "report_date",
                      min = min(unique(ds_factors$date)),
                      max = max(unique(ds_factors$date)),
                      value = max(unique(ds_factors$date)))

    shiny::updateSelectInput(session = session, inputId = "start_date",
                      choices = unique(ds_factors$date),
                      selected = max(unique(ds_factors$date)))

    shiny::updateSelectInput(session = session, inputId = "end_date",
                      choices = unique(ds_factors$date),
                      selected = max(unique(ds_factors$date)))

    shiny::updateSelectInput(session = session, inputId = "factors",
                      choices = c("ALL", unique(ds_factors$factor_name)),
                      selected = "ALL")

  })

  # Select date range
  select_date_range <- shiny::reactive({

    ds_factors <- ds_transform_factors()

    date_type <- input$date_type

    switch(date_type,
           'single_period' = {
             report_dates <- sort(unique(ds_factors$date))
             start_date <- min(report_dates[report_dates >= input$report_date])
             end_date <- start_date
             start_date <- format(start_date, "%Y-%m-%d")
             end_date <- format(end_date, "%Y-%m-%d")
           },
           'multi_period' = {
             start_date <- input$start_date
             end_date <- input$start_date
             if (end_date < start_date) end_date <- start_date
           })

    cat(file = stderr(), "selected date:", start_date, ":",
        end_date,"\n")

    return(list(start_date = start_date, end_date = end_date))

  })

  # Filter data of factors
  select_dataset <- shiny::reactive({

    ds_factors <- ds_transform_factors()

    select_factors <- input$factors
    if (all(select_factors %in% "ALL")) {
      select_factors <- unique(ds_factors$factor_name)
    }


    ds_select_factors <- ds_factors %>%
      dplyr::filter(date >= select_date_range()$start_date &
                      date <= select_date_range()$end_date &
                      factor_name %in% !!select_factors )
  })


  # Render captions
  output$caption <- shiny::renderText({

    start_date <- select_date_range()$start_date
    end_date <- select_date_range()$end_date

    caption <- sprintf("Factors Correlation(%s~%s)", start_date, end_date)

    return(caption)
  })

  # plot correlation plot
  output$factor_correlation_plot <- shiny::renderPlot({

    ds_select_factors <- select_dataset()

    ds_select_factors <- ds_select_factors %>%
        dplyr::ungroup() %>%
        tidyr::spread(key = "factor_name", value = "factor_exposure", drop = TRUE) %>%
        dplyr::select(-(date:indcd))

    # plot the select data correlation
    car::scatterplotMatrix(ds_select_factors)

  })

  # summarize correlation matrix
  output$factor_corelation_table <- shiny::renderPrint({


    ds_select_factors <- select_dataset()

    ds_select_factors <- ds_select_factors %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = "factor_name", value = "factor_exposure", drop = TRUE) %>%
      dplyr::select(-(date:indcd))

    select_factors_correlation <- cor(ds_select_factors, use = "complete")

    select_factors_correlation <- round(select_factors_correlation, digits = 2)

    return(select_factors_correlation)

  })
}

