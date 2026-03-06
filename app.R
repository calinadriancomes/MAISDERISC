# app.R (schelet minimal)
source("00_packages.R")
source("01_io.R")
source("02_preprocess.R")
source("03_descriptives.R")
source("04_models.R")
source("05_diagnostics.R")
source("06_predict.R")
source("07_shiny_module_analysis.R")
source("08_dashboard_helpers.R")

ui <- shiny::fluidPage(
  shiny::titlePanel("Analiza litigiilor cu ANAF – modele de tip count"),
  analysisModuleUI("ana")
)

server <- function(input, output, session) {
  analysisModuleServer("ana")
}

shiny::shinyApp(ui, server)