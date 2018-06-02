# Load factors UI
#' @export
loadFactorsUI <- function(id, factors_info) {

  #Prepare factor groups/factor list
  factors_info_by_group <- factors_info %>%
    dplyr::group_by(factor_group) %>%
    dplyr::summarise(factors_list = list(factor_code))

  ns <- shiny::NS(id)

  shiny::tagList(

    shinydashboard::box(status = "primary",
                        title = "Parameters",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 12,

                        shiny::selectInput(inputId = ns("factor_groups"),
                                           label = shiny::strong("Factor Groups:"),
                                           choices = unique(factors_info_by_group$factor_group),
                                           selected = head(factors_info_by_group$factor_group,n = 1),
                                           multiple = TRUE),

                        shiny::selectizeInput(inputId = ns("factors_in_group"),
                                              label = shiny::strong("Factors in Group:"),
                                              choices = NULL,
                                              multiple = TRUE),

                        shiny::selectInput(inputId = ns("select_factors"),
                                           label = shiny::strong("Selected Factors:"),
                                           choices = NULL,
                                           multiple = TRUE),

                        shiny::actionButton(inputId = ns("load_factors"),
                                            label = shiny::strong("Load Factors Data")
                        )
    ),

    # Output: Tabset of output tables
    shiny::tabsetPanel(type = "tabs",
                       shiny::tabPanel("Factor Info",
                                       DT::dataTableOutput(outputId = ns("factors_info_table"))
                       ),
                       shiny::tabPanel("Factor Data",
                                       DT::dataTableOutput(outputId = ns("factors_data_table"))
                       )
    )
  )
}

# Load factors Server Function
#' @export
loadFactors <- function(input, output, session, factors_info) {

  # Update factors_in_group input according selected factor group
  shiny::observe({

    # get new selected factor group
    selected_factor_groups <- shiny::req(input$factor_groups)

    factors_info <- factors_info()

    #Prepare factor groups/factor list
    factors_in_selected_group <- factors_info %>%
      dplyr::filter(factor_group %in% !!selected_factor_groups)

    select_factor_code <- factors_in_selected_group$factor_code
    names(select_factor_code) <- factors_in_selected_group$factor_name
    # select_factor_code <- list(a = select_factor_code)

    # Update factors in group input by specified groups
    shiny::updateSelectizeInput(session, inputId = "factors_in_group",
                                choices = select_factor_code )
  })

  # Update select_factors input according new selected factors
  shiny::observe({

    # get new selected factors
    new_select_factors <- shiny::req(input$factors_in_group)

    # construct current select factors by new and old selected factors
    current_select_factors <- shiny::isolate(input$select_factors)
    current_select_factors <- unique(c(current_select_factors, new_select_factors))

    # Update factors in group by specified groups
    shiny::updateSelectInput(session, inputId = "select_factors",
                             selected = current_select_factors,
                             choices = current_select_factors)
  })


  # load factors
  load_factors <- shiny::reactive({

    shiny::req(input$load_factors)

    factors_list <- shiny::isolate(shiny::req(input$select_factors))

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

  # Display factors Info
  output$factors_info_table <- DT::renderDataTable(
    DT::datatable(
      factors_info(), options = list(
        columnDefs = list(list(className = 'dt-center')),
        pageLength = 25
      )
    )
  )

  # Display loaded factors
  output$factors_data_table <- DT::renderDataTable(
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
