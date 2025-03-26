# Données réactives
mesures_cat_data <- reactiveVal()

observeEvent(input$upload_file, {
  req(input$upload_file)
  
  # Lecture du fichier (1re feuille ou nommée)
  path <- input$upload_file$datapath
  df <- readxl::read_excel(path, sheet = 1)
  
  # ⚠️ Optionnel : nettoyage/renommage si besoin
  df <- as.data.frame(df)
  
  mesures_cat_data(df)
})

output$table_cat <- renderRHandsontable({
  req(mesures_cat_data())
  rhandsontable(mesures_cat_data(), rowHeaders = NULL)
})

observeEvent(input$table_cat, {
  mesures_cat_data(hot_to_r(input$table_cat))
})
