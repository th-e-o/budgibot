# 📦 mod_mesures_cat.R

# UI module
mod_mesures_cat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .rhandsontable {
        overflow-x: auto !important;
        overflow-y: auto !important;
        max-height: 450px;
        border: 1px solid #ddd;
      }
    ")),
    fileInput(ns("upload_file"), "📂 Charger un fichier Excel (.xlsx)"),
    uiOutput(ns("sheet_selector")),
    div(
      style = "overflow-x: auto;",
      rHandsontableOutput(ns("table_cat"))
    ),
    downloadButton(ns("download_table"), "💾 Exporter le tableau modifié", class = "btn btn-secondary")
  )
}

# Server module
mod_mesures_cat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    library(readxl)
    library(rhandsontable)
    library(openxlsx2)
    
    rv_table <- reactiveVal()
    rv_sheets <- reactiveVal()
    rv_path <- reactiveVal()
    
    # 🔄 Chargement du fichier
    observeEvent(input$upload_file, {
      req(input$upload_file)
      ext <- tools::file_ext(input$upload_file$name)
      if (tolower(ext) != "xlsx") {
        showNotification("Veuillez charger un fichier Excel (.xlsx)", type = "error")
        return()
      }
      
      path <- input$upload_file$datapath
      rv_path(path)
      
      sheets <- excel_sheets(path)
      rv_sheets(sheets)
      
      showNotification("✅ Upload complet", type = "message")
    })
    
    # 🧾 Menu déroulant des feuilles
    output$sheet_selector <- renderUI({
      req(rv_sheets())
      selectInput(session$ns("selected_sheet"), "🗂️ Choisir une feuille", choices = rv_sheets())
    })
    
    # 📥 Lecture d’une feuille
    observeEvent(input$selected_sheet, {
      req(rv_path(), input$selected_sheet)
      df <- tryCatch({
        read_excel(rv_path(), sheet = input$selected_sheet, skip = 4)
      }, error = function(e) {
        showNotification("❌ Erreur lors de la lecture de la feuille", type = "error")
        return(NULL)
      })
      
      rv_table(as.data.frame(df))
    })
    
    # 📊 Affichage du tableau RH
    output$table_cat <- renderRHandsontable({
      req(rv_table())
      rhandsontable(rv_table(), rowHeaders = NULL, height = 450, width = "100%") %>%
        hot_cols(manualColumnResize = TRUE, colWidths = 140) %>%
        hot_table(stretchH = "all")
    })
    
    observeEvent(input$table_cat, {
      rv_table(hot_to_r(input$table_cat))
    })
    
    # 💾 Export Excel
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("mesures_cat_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- wb_workbook()
        wb_add_worksheet(wb, "Mesures")
        wb_add_data(wb, sheet = 1, x = rv_table())
        wb_save(wb, file)
      }
    )
  })
}
