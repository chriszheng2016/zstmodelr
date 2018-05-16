
library(shiny)
library(shinydashboard)
library(zstmodelr)

# Load data

fundamental_factors <- NULL
# fundamental_factors <- readRDS("./../../data-raw/fundamental_factors.rds")
# fundamental_factors <- fundamental_factors %>%
#   tidyr::gather(key = "factor_name", value = "factor_exposure", -(date:indcd))


fundamental_normal_factors <- NULL
# fundamental_normal_factors <- readRDS("./../../data-raw/fundamental_normal_factors.rds")
# fundamental_normal_factors <- fundamental_normal_factors %>%
#   tidyr::gather(key = "factor_name", value = "factor_exposure", -(date:indcd))

fundamental_factors_list <- c("GPM", "ROCE", "PE", "PB", "CUR", "QR")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("数据准备", tabName = "prepare_factors", icon = icon("dashboard")),
      menuItem("非标准化因子", tabName = "non_normalized_factors", icon = icon("th")),
      menuItem("标准化因子", tabName = "normalized_factors", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "prepare_factors",
        tabsetPanel(
          tabPanel("加载因子", loadFactorsUI("load_factors",
                   factors_list = fundamental_factors_list)),
          tabPanel("标准化因子", normalizeFactorsUI("normalize_factors"))
        )
      ),
      tabItem(
        tabName = "non_normalized_factors",
        tabsetPanel(
          tabPanel("因子分布", exploreFactorsDistributionUI("non_normalized_factors_distribution",
                                                        ds_factors = fundamental_factors)),
          tabPanel("因子相关性", exploreFactorsCorrelationUI("non_normalized_factors_correlation",
                                                        ds_factors = fundamental_factors))
        )

      ),
      tabItem(
        tabName = "normalized_factors",
        tabsetPanel(
          tabPanel("因子分布", exploreFactorsDistributionUI("normalized_factors_distribution",
                                                       ds_factors = fundamental_normal_factors)),
          tabPanel("因子相关性", exploreFactorsCorrelationUI("normalized_factors_correlation",
                                                        ds_factors = fundamental_normal_factors))
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # browser()

  fundamental_factors <- callModule(loadFactors, "load_factors")

  fundamental_normal_factors <- callModule(normalizeFactors, "normalize_factors",
              ds_factors = fundamental_factors)

  callModule(exploreFactorsDistribution, "non_normalized_factors_distribution",
             ds_factors = fundamental_factors)

  callModule(exploreFactorsDistribution, "normalized_factors_distribution",
             ds_factors = fundamental_normal_factors)

  callModule(exploreFactorsCorrelation, "non_normalized_factors_correlation",
             ds_factors = fundamental_factors)

  callModule(exploreFactorsCorrelation, "normalized_factors_correlation",
             ds_factors = fundamental_normal_factors)

}


# Run the application
shinyApp(ui = ui, server = server)

