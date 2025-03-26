# mod_mesures_cat.R

# UI module
mod_mesures_cat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("upload_file"), "📂 Charger un fichier Excel (.xlsx)"),
    uiOutput(ns("sheet_selector")),
    
    # ✅ Reactable avec scroll horizontal
    div(style = "overflow-x: auto;", reactableOutput(ns("reactable_table"))),
    
    actionButton(ns("open_full_editor"), "🖋️ Modifier la feuille", class = "btn btn-secondary mt-2"),
    downloadButton(ns("download_table"), "💾 Exporter le tableau modifié")
  )
}


# Server module
mod_mesures_cat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(readxl)
    library(rhandsontable)
    library(openxlsx2)
    library(reactable)
    
    rv_table <- reactiveVal()
    rv_sheets <- reactiveVal()
    rv_path <- reactiveVal()
    
    # 1️⃣ Lecture du fichier et extraction des feuilles
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
    })
    
    # 2️⃣ Sélecteur de feuille
    output$sheet_selector <- renderUI({
      req(rv_sheets())
      selectInput(ns("selected_sheet"), "🗂️ Choisir une feuille", choices = rv_sheets())
    })
    
    # 3️⃣ Lecture de la feuille sélectionnée
    observeEvent(input$selected_sheet, {
      req(rv_path(), input$selected_sheet)
      df <- read_excel(rv_path(), sheet = input$selected_sheet, skip = 4)
      rv_table(as.data.frame(df))
    })
    
    # 4️⃣ Affichage reactable
    output$reactable_table <- reactable::renderReactable({
      req(rv_table())
      df <- rv_table()
      reactable(
        df,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        pagination = FALSE,  # ✅ On désactive la pagination
        defaultColDef = colDef(
          minWidth = 100,
          style = list(
            whiteSpace = "pre-wrap",
            overflow = "hidden",
            textOverflow = "ellipsis",
            fontSize = "12px",
            padding = "4px"
          )
        ),
        theme = reactableTheme(
          borderColor = "#ddd",
          stripedColor = "#f6f8fa",
          highlightColor = "#eaeaea"
        ),
        style = list(maxHeight = "70vh", overflowY = "auto")  # ✅ Scroll vertical
      )
    })
    
    
    
    
    # 5️⃣ Edition complète en plein écran simulé
    observeEvent(input$open_full_editor, {
      showModal(modalDialog(
        title = "🖋️ Édition plein écran",
        easyClose = TRUE,
        footer = tagList(
          modalButton("❌ Fermer"),
          actionButton(ns("save_edits"), "💾 Enregistrer")
        ),
        rhandsontable::rHandsontableOutput(ns("hot_table"), height = "70vh"),
        size = "l",
        style = "width: 95vw; max-width: none;"
      ))
    })
    
    
    output$hot_table <- rhandsontable::renderRHandsontable({
      req(rv_table())
      df <- rv_table()
      nb_cols <- ncol(df)
      col_widths <- rep(120, nb_cols)  # 120px de largeur fixe pour chaque colonne (tu peux adapter)
      
      rhandsontable(df, rowHeaders = NULL, height = "calc(100vh - 150px)", width = "100%") %>%
        hot_cols(colWidths = col_widths) %>%
        hot_table(stretchH = "none")  # "none" désactive l'étirement, évite d'écraser les largeurs
    })
    
    observeEvent(input$save_edits, {
      rv_table(hot_to_r(input$hot_table))
      removeModal()
    })
    
    # 6️⃣ Export modifié
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