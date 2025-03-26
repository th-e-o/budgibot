library(shiny)
source("../mod_mesures_cat.R")  # <-- adapte le chemin selon ton arborescence

ui <- fluidPage(
  tags$head(tags$title("BudgiBot - Édition Excel")),
  mod_mesures_cat_ui("cat_full")
)

server <- function(input, output, session) {
  mod_mesures_cat_server("cat_full")
}

shinyApp(ui, server)
