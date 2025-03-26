# mod_mesures_cat.R

# UI
mod_mesures_cat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("upload_file"), "📂 Charger un fichier Excel (.xlsx)"),
    uiOutput(ns("sheet_selector")),
    div(style = "margin-top: 10px;",
        reactable::reactableOutput(ns("table_cat"))),
    downloadButton(ns("download_table"), "💾 Exporter le tableau modifié")
  )
}

# Server
mod_mesures_cat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    library(readxl)
    library(openxlsx2)
    library(reactable)
    library(dplyr)
    
    rv_data <- reactiveVal()
    rv_sheets <- reactiveVal()
    rv_path <- reactiveVal()
    
    # 📁 Fichier uploadé
    observeEvent(input$upload_file, {
      req(input$upload_file)
      ext <- tools::file_ext(input$upload_file$name)
      if (tolower(ext) != "xlsx") {
        showNotification("Veuillez charger un fichier Excel (.xlsx)", type = "error")
        return()
      }
      
      path <- input$upload_file$datapath
      rv_path(path)
      rv_sheets(excel_sheets(path))
    })
    
    # 🗂️ Sélecteur de feuille
    output$sheet_selector <- renderUI({
      req(rv_sheets())
      selectInput(session$ns("selected_sheet"), "🗂️ Choisir une feuille", choices = rv_sheets())
    })
    
    # 📄 Lecture de la feuille
    observeEvent(input$selected_sheet, {
      req(rv_path(), input$selected_sheet)
      df <- read_excel(rv_path(), sheet = input$selected_sheet, skip = 4)
      rv_data(df)
    })
    
    # 🧾 Affichage tableau avec reactable
    output$table_cat <- renderReactable({
      req(rv_data())
      reactable(
        rv_data(),
        searchable = TRUE,
        filterable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 20,
        paginationType = "simple",
        striped = TRUE,
        style = list(fontSize = "13px"),
        theme = reactableTheme(
          borderColor = "#ccc",
          stripedColor = "#f6f8fa",
          highlightColor = "#e5f5ff"
        )
      )
    })
    
    # 💾 Export table
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("mesures_cat_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- wb_workbook()
        wb_add_worksheet(wb, "Mesures")
        wb_add_data(wb, sheet = 1, x = rv_data())
        wb_save(wb, file)
      }
    )
  })
}
