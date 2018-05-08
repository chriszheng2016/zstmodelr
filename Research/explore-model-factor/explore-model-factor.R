library(shiny)
library(shinydashboard)

library(PerformanceAnalytics)
library(tidyverse)
library(lubridate)
library(stringr)
library(zstmodelr)


# Load factors UI
loadFactorsUI <- function(id, factors_list) {

  ns <- NS(id)



  tagList(

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

    box(status = "primary",
        title = "Parameters",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,

        selectInput(inputId = ns("factors"),label = strong("Factors:"),
                  choices = unique(factors_list), multiple = TRUE),

        actionButton(inputId = ns("load_factors"),
                     label = strong("Load Factors")
        )
    ),

    DT::dataTableOutput(outputId = ns("factors_table"))
   )
}

# Load factors Server Function
loadFactors <- function(input, output, session) {

  # load factors
  load_factors <- reactive({

    req(input$load_factors)

    factors_list <- isolate(req(input$factors))

    withProgress(message = 'Load Factors', value = 0, {

      incProgress(message = "open_stock_db")

      # Open gta stock databse
      stock_db <- stock_db(gta_db, "GTA_SQLData")
      open_stock_db(stock_db)

      incProgress(message = "init_stock_db")

      # Initate the stock database
      invisible(init_stock_db(stock_db))

      incProgress(message = "get_factor_indicator")

      # Get fundamental indicators
      fundamental_factors <- get_factor_indicator(stock_db,
                                                  factor_list = factors_list)

      # fundamental_factors <- readRDS("./../../data-raw/fundamental_factors.rds")

      close_stock_db(stock_db)


    })

     showNotification("Load factors successfully.")

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
normalizeFactorsUI <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      status = "primary",
      title = "Parameters",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = ns("clean_extremes_method"),
            label = strong("Clean extremes method:"),
            choices = c("sigma", "mad")          ),

          sliderInput(inputId = ns("n_sigma"), label = strong("n Sigmas:"),
                      min = 1,
                      max = 5,
                      value = 3, step = 1),

          sliderInput(inputId = ns("n_dmad"), label = strong("n dmads:"),
                      min = 1,
                      max = 5,
                      value = 3, step = 1),
          selectInput(
            inputId = ns("value_replace_extremes"),
            label = strong("value to replace extremes:"),
            choices = c("limit", "NA")
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = ns("standard_method"),
            label = strong("Standardize Method:"),
            choices = c("normal", "rank")
          )
        )
      ),
      actionButton(
          inputId = ns("normalize_factors"),
          label = strong("Normalize Factors")
      )

    ),

    DT::dataTableOutput(outputId = ns("normalize_factors_table"))
  )
}

# Normalize factors Server Function
normalizeFactors <- function(input, output, session, ds_factors) {

  # Normalize factors
  normalized_factors <- reactive({

    req(input$normalize_factors)

    ds_factors <- ds_factors()

    showNotification("Normalizing factors will take few minutes...")

    isolate({


      # if (input$clean_extremes_method == "sigma") {
      #   arguments_list = rlang::expr("n_sigma = input$n_sigma")
      # } else {
      #   arguments_list = rlang::expr("n_dmad  = input$n_dmad")
      # }
      #
      # normalized_factors <- normalize_factors(ds_factors,
      #                                         group_by = c("date"),
      #                                         clean_extremes_method = input$clean_extremes_method,
      #                                         standard_method = input$standard_method,
      #                                         extreme_value = input$value_replace_extremes,
      #                                         arguments_list)

      if (input$clean_extremes_method == "sigma") {
        normalized_factors <- normalize_factors(ds_factors,
                                                group_by = c("date"),
                                                clean_extremes_method = input$clean_extremes_method,
                                                standard_method = input$standard_method,
                                                extreme_value = input$value_replace_extremes,
                                                n_sigma = input$n_sigma)
      } else {
         normalized_factors <- normalize_factors(ds_factors,
                                                group_by = c("date"),
                                                clean_extremes_method = input$clean_extremes_method,
                                                standard_method = input$standard_method,
                                                extreme_value = input$value_replace_extremes,
                                                n_dmad  = input$n_dmad)
      }

    })

    # normalized_factors <- readRDS("./../../data-raw/fundamental_normal_factors.rds")


    showNotification("Normalize factors successfully.")

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
exploreFactorsDistributionUI <- function(id, ds_factors) {
  ns <- NS(id)

  tagList(
      #Side panel for input
      sidebarPanel(

        radioButtons(inputId = ns("date_type"), label = strong("Date Range"),
                           choices = list("Single Period" = "single_period",
                                          "Multi Periods" = "multi_period"),
                           selected = "single_period"),

        conditionalPanel(
          condition = "input.date_type == 'single_period'",
          sliderInput(inputId = ns("report_date"), label = strong("report Date:"),
                      min = lubridate::as_date("2017-12-31"),
                      max = lubridate::as_date("2017-12-31"),
                      value = lubridate::as_date("2017-12-31"), step = NULL,
                      timeFormat = "%F"),
          ns = ns
        ),

        conditionalPanel(
          condition = "input.date_type == 'multi_period'",
          selectInput(inputId = ns("start_date"), label = strong("Start Date:"),
                      choices = ""),

          selectInput(inputId = ns("end_date"), label = strong("End Date:"),
                      choices = ""),
          ns = ns
        ),

        selectInput(inputId = ns("plot_type"), label = strong("Plot Type:"),
                    choices = c("boxplot", "violin", "density", "histogram", "freqpoly"),
                    selected = "boxplot"),

        selectInput(inputId = ns("factors"), label = strong("Factors:"),
                    choices = "", multiple = TRUE),

        sliderInput(inputId = ns("bins_adjust"), label = "Bins for Histogram/freqpoly:",
                    min = 5, max = 100, value = 30, step = 1)
      ),

      # MainPanel for Output
      mainPanel(
        titlePanel(textOutput(outputId = ns("caption"))),
        plotOutput(outputId = ns("factor_compare_plot")),
        tableOutput(outputId = ns("factor_compare_table"))
      )
  )

}

# Explore factors distrubtion Server Function
exploreFactorsDistribution <- function(input, output, session, ds_factors) {

  ds_transform_factors <- reactive({
    ds_factors <- ds_factors()
    ds_transform_factors <- ds_factors %>%
      tidyr::gather(key = "factor_name", value = "factor_exposure", -(date:indcd))

    return(ds_transform_factors)
  })

  # Update UI base ds_factors
  observe({

    ds_factors <- ds_transform_factors()

    updateSliderInput(session = session, inputId = "report_date",
                      min = min(unique(ds_factors$date)),
                      max = max(unique(ds_factors$date)),
                      value = max(unique(ds_factors$date)))

    updateSelectInput(session = session, inputId = "start_date",
                choices = unique(ds_factors$date),
                selected = max(unique(ds_factors$date)))

    updateSelectInput(session = session, inputId = "end_date",
                choices = unique(ds_factors$date),
                selected = max(unique(ds_factors$date)))

    updateSelectInput(session = session, inputId = "factors",
                      choices = c("ALL", unique(ds_factors$factor_name)),
                      selected = "ALL")

  })

  # Select date range
  select_date_range <- reactive({

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
  select_dataset <- reactive({

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
  output$caption <- renderText({

    start_date <- select_date_range()$start_date
    end_date <- select_date_range()$end_date

    caption <- sprintf("Factors Distribution(%s~%s)", start_date, end_date)

    return(caption)
  })

  # plot comparsion plot
  output$factor_compare_plot <- renderPlot({
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
  output$factor_compare_table <- renderTable({
    ds_select_factors <- select_dataset()

    select_factors_summary <- ds_select_factors %>%
      dplyr::group_by(factor_name) %>%
      dplyr::summarise_at(
        "factor_exposure",
        funs(
          NAs = sum(is.na(.)),
          mean,
          sd,
          median,
          mad,
          min,
          Q1 = quantile(., 0.25),
          Q3 = quantile(., 0.75),
          max,
          skewness,
          kurtosis
        ),
        na.rm = TRUE
      )

    return(select_factors_summary)

  })
}

# Explore factors correlation UI
exploreFactorsCorrelationUI <- function(id, ds_factors) {
  ns <- NS(id)

  tagList(
    #Side panel for input
    sidebarPanel(
      radioButtons(inputId = ns("date_type"), label = strong("Date Range"),
                   choices = list("Single Period" = "single_period",
                                  "Multi Periods" = "multi_period"),
                   selected = "single_period"),

      conditionalPanel(
        condition = "input.date_type == 'single_period'",
        sliderInput(inputId = ns("report_date"), label = strong("report Date:"),
                    min = lubridate::as_date("2017-12-31"),
                    max = lubridate::as_date("2017-12-31"),
                    value = lubridate::as_date("2017-12-31"), step = NULL,
                    timeFormat = "%F"),
        ns = ns
      ),

      conditionalPanel(
        condition = "input.date_type == 'multi_period'",
        selectInput(inputId = ns("start_date"), label = strong("Start Date:"),
                    choices = ""),

        selectInput(inputId = ns("end_date"), label = strong("End Date:"),
                    choices = ""),
        ns = ns
      ),

      selectInput(inputId = ns("factors"), label = strong("Factors:"),
                  choices = "", multiple = TRUE)


    ),

    # MainPanel for Output
    mainPanel(
      titlePanel(textOutput(outputId = ns("caption"))),
      plotOutput(outputId = ns("factor_correlation_plot")),
      verbatimTextOutput(outputId = ns("factor_corelation_table"))
    )
  )
}

# Explore factors correlation Server Function
exploreFactorsCorrelation <- function(input, output, session, ds_factors) {

  ds_transform_factors <- reactive({
    ds_factors <- ds_factors()
    ds_transform_factors <- ds_factors %>%
      tidyr::gather(key = "factor_name", value = "factor_exposure", -(date:indcd))

    return(ds_transform_factors)
  })

  # Update UI base ds_factors
  observe({

    ds_factors <- ds_transform_factors()

    updateSliderInput(session = session, inputId = "report_date",
                      min = min(unique(ds_factors$date)),
                      max = max(unique(ds_factors$date)),
                      value = max(unique(ds_factors$date)))

    updateSelectInput(session = session, inputId = "start_date",
                      choices = unique(ds_factors$date),
                      selected = max(unique(ds_factors$date)))

    updateSelectInput(session = session, inputId = "end_date",
                      choices = unique(ds_factors$date),
                      selected = max(unique(ds_factors$date)))

    updateSelectInput(session = session, inputId = "factors",
                      choices = c("ALL", unique(ds_factors$factor_name)),
                      selected = "ALL")

  })

  # Select date range
  select_date_range <- reactive({

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
  select_dataset <- reactive({

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
  output$caption <- renderText({

    start_date <- select_date_range()$start_date
    end_date <- select_date_range()$end_date

    caption <- sprintf("Factors Correlation(%s~%s)", start_date, end_date)

    return(caption)
  })

  # plot correlation plot
  output$factor_correlation_plot <- renderPlot({

    ds_select_factors <- select_dataset()

    ds_select_factors <- ds_select_factors %>%
        dplyr::ungroup() %>%
        tidyr::spread(key = "factor_name", value = "factor_exposure", drop = TRUE) %>%
        dplyr::select(-(date:indcd))

    # plot the select data correlation
    car::scatterplotMatrix(ds_select_factors)

  })

  # summarize correlation matrix
  output$factor_corelation_table <- renderPrint({


    ds_select_factors <- select_dataset()

    ds_select_factors <- ds_select_factors %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = "factor_name", value = "factor_exposure", drop = TRUE) %>%
      dplyr::select(-(date:indcd))

    select_factors_correlation <- cor(ds_select_factors, use = "complete")

    select_factors_correlation<- round(select_factors_correlation, digits = 2)

    return(select_factors_correlation)

  })
}

