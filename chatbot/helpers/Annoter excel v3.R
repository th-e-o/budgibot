# app_mesures_cat.R (version enrichie avec annotations + LLM + sélection interactive)

library(shiny)
library(readxl)
library(rhandsontable)
library(openxlsx2)
library(reactable)
library(httr)
library(jsonlite)

# 🔁 Conversion numéro -> lettre de colonne Excel
num_to_excel_col <- function(n) {
  div <- n
  col <- ""
  while (div > 0) {
    mod <- (div - 1) %% 26
    col <- paste0(LETTERS[mod + 1], col)
    div <- (div - mod - 1) %/% 26
  }
  return(col)
}

# 🔁 Conversion lettre -> numéro de colonne Excel
excel_col_to_num <- function(col_str) {
  chars <- strsplit(col_str, "")[[1]]
  sum((match(chars, LETTERS)) * 26^(rev(seq_along(chars)) - 1))
}

ui <- fluidPage(
  titlePanel("📊 Annotateur Excel"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload_file", "📂 Charger un fichier Excel (.xlsx)"),
      uiOutput("sheet_selector"),
      actionButton("open_full_editor", "🕋️ Modifier la feuille", class = "btn btn-secondary mt-2"),
      actionButton("auto_annotate_btn", "🔍 Pré-annoter automatiquement", class = "btn btn-info mt-2"),
      downloadButton("download_table", "💾 Exporter le tableau modifié"),
      downloadButton("download_tags", "📤 Exporter les tags"),
      hr(),
      uiOutput("annotation_view")
    ),
    mainPanel(
      uiOutput("cell_info"),
      rHandsontableOutput("excel_table")
    )
  )
)

server <- function(input, output, session) {
  rv_table <- reactiveVal()
  rv_sheets <- reactiveVal()
  rv_path <- reactiveVal()
  rv_annotations <- reactiveVal(list())
  rv_selected <- reactiveVal(NULL)
  
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
  
  output$sheet_selector <- renderUI({
    req(rv_sheets())
    selectInput("selected_sheet", "📂 Choisir une feuille", choices = rv_sheets())
  })
  
  observeEvent(input$selected_sheet, {
    req(rv_path(), input$selected_sheet)
    df <- read_excel(rv_path(), sheet = input$selected_sheet, skip = 4)
    rv_table(as.data.frame(df))
  })
  
  output$excel_table <- renderRHandsontable({
    req(rv_table())
    df <- rv_table()
    rh <- rhandsontable(df, selectCallback = TRUE, readOnly = FALSE) %>%
      hot_table(stretchH = "all")
    
    anns <- rv_annotations()
    if (length(anns) > 0) {
      for (ann in anns) {
        if (!is.null(ann$range)) {
          col <- gsub("\\d", "", ann$range)
          row <- as.numeric(gsub("\\D", "", ann$range))
          col_index <- excel_col_to_num(col)
          rh <- hot_cell(rh, row = row, col = col_index, className = ifelse(ann$obligatoire, "required", "tagged"))
        }
      }
    }
    rh
  })
  
  output$cell_info <- renderUI({
    if (!is.null(input$excel_table_select$select$r)) {
      r <- input$excel_table_select$select$r + 1
      c <- input$excel_table_select$select$c + 1
      cell_ref <- paste0(num_to_excel_col(c), r)
      tagList(
        tags$h4("🔎 Cellule sélectionnée :"),
        tags$p(paste("Ligne:", r, "| Colonne:", c, "| Réf:", cell_ref)),
        textInput("annotation_type", "Type"),
        textInput("annotation_comment", "Commentaire"),
        checkboxInput("annotation_required", "Obligatoire", FALSE),
        actionButton("save_annotation", "💬 Annoter", class = "btn btn-warning")
      )
    }
  })
  
  observeEvent(input$save_annotation, {
    sel <- input$excel_table_select$select
    if (!is.null(sel)) {
      r <- sel$r + 1
      c <- sel$c + 1
      cell_ref <- paste0(num_to_excel_col(c), r)
      new_ann <- list(
        range = cell_ref,
        type = input$annotation_type,
        commentaire = input$annotation_comment,
        obligatoire = input$annotation_required
      )
      anns <- rv_annotations()
      rv_annotations(append(anns, list(new_ann)))
      showNotification(paste("Annotation ajoutée à", cell_ref))
    }
  })
  
  output$annotation_view <- renderUI({
    anns <- rv_annotations()
    if (length(anns) == 0) return(tags$p("Aucune annotation"))
    
    tagList(
      h4("📌 Annotations enregistrées :"),
      lapply(seq_along(anns), function(i) {
        a <- anns[[i]]
        div(style = "border: 1px solid #ccc; padding: 8px; margin-bottom: 6px; border-radius: 5px;",
            tags$b(paste0("🔹 Plage : ", a$range)),
            tags$p(paste0("Type : ", a$type, " | Obligatoire : ", ifelse(a$obligatoire, "Oui", "Non"))),
            tags$p(paste0("💬 ", a$commentaire)),
            actionButton(paste0("del_annotation_", i), "🔚 Supprimer", class = "btn-danger btn-sm")
        )
      })
    )
  })
  
  observe({
    anns <- rv_annotations()
    for (i in seq_along(anns)) {
      local({
        ii <- i
        observeEvent(input[[paste0("del_annotation_", ii)]], {
          anns <- rv_annotations()
          anns <- anns[-ii]
          rv_annotations(anns)
          showNotification("❌ Annotation supprimée", type = "warning")
        }, ignoreInit = TRUE)
      })
    }
  })
  
  real_llm_annotation <- function(df) {
    api_url <- "https://api.mistral.ai/v1/chat/completions"
    api_key <- Sys.getenv("MISTRAL_API_KEY")
    
    prompt <- paste0(
      "Voici un tableau extrait d'un fichier Excel. Propose une liste d'annotations utiles ",
      "sous forme de liste JSON. Chaque annotation doit inclure : range (ex: B5), type, obligatoire (TRUE/FALSE), commentaire.\n\n",
      "Extrait du tableau :\n",
      paste(capture.output(utils::head(df, 10)), collapse = "\n")
    )
    
    body <- list(
      model = "mistral-small",
      messages = list(list(role = "user", content = prompt))
    )
    
    response <- httr::POST(
      url = api_url,
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      encode = "json",
      body = body
    )
    
    parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    json_output <- parsed$choices[[1]]$message$content
    jsonlite::fromJSON(json_output, simplifyVector = FALSE)
  }
  
  observeEvent(input$auto_annotate_btn, {
    req(rv_table())
    tryCatch({
      annotations <- real_llm_annotation(rv_table())
      rv_annotations(annotations)
      showNotification("✅ Pré-annotation LLM terminée", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Erreur LLM:", e$message), type = "error")
    })
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste0("mesures_cat_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- wb_workbook()
      wb_add_worksheet(wb, "Mesures")
      wb_add_data(wb, sheet = 1, x = rv_table())
      wb_save(wb, file)
    }
  )
  
  output$download_tags <- downloadHandler(
    filename = function() paste0("annotations_", Sys.Date(), ".csv"),
    content = function(file) {
      anns <- rv_annotations()
      if (length(anns) > 0) {
        df <- do.call(rbind, lapply(anns, as.data.frame))
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)