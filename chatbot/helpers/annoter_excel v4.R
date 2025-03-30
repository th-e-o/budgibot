# app_mesures_cat.R (version enrichie avec fusions + détection structurelle + pré-annotation LLM améliorée)

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

# 🔍 Étend les en-têtes fusionnées à leurs cellules cibles
expand_merged_titles <- function(wb, sheet_name, df) {
  sheet_index <- which(names(wb$worksheets) == sheet_name)
  
  # ✅ Toujours initialiser la liste d'abord
  merged_map <- list()
  
  merged_ranges <- tryCatch({
    wb$worksheets[[sheet_index]]$mergeCells
  }, error = function(e) NULL)
  
  if (is.null(merged_ranges) || length(merged_ranges) == 0) return(merged_map)
  
  for (range in merged_ranges) {
    coords <- strsplit(range, ":")[[1]]
    start <- coords[1]
    end <- coords[2]
    
    start_col <- gsub("\\d", "", start)
    start_row <- as.numeric(gsub("\\D", "", start))
    end_col <- gsub("\\d", "", end)
    end_row <- as.numeric(gsub("\\D", "", end))
    
    val <- tryCatch(
      df[start_row, excel_col_to_num(start_col)],
      error = function(e) NA
    )
    
    for (r in start_row:end_row) {
      for (c in excel_col_to_num(start_col):excel_col_to_num(end_col)) {
        merged_map[[paste0(num_to_excel_col(c), r)]] <- val
      }
    }
  }
  
  return(merged_map)
}


ui <- fluidPage(
  titlePanel("📊 Annotateur Excel"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload_file", "📂 Charger un fichier Excel (.xlsx)"),
      uiOutput("sheet_selector"),
      actionButton("open_full_editor", "🕋️ Modifier la feuille", class = "btn btn-secondary mt-2"),
      actionButton("auto_annotate_btn", "🔍 Pré-annoter automatiquement", class = "btn btn-info mt-2"),
      actionButton("annotate_selected_zone", "💡 Pré-annoter cette zone", class = "btn-success")
      
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
  rv_merged_map <- reactiveVal(list())
  
  input$excel_table_select$select$r1  # ligne min
  input$excel_table_select$select$r2  # ligne max
  input$excel_table_select$select$c1  # colonne min
  input$excel_table_select$select$c2  # colonne max
  
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
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
    df <- read_excel(rv_path(), sheet = input$selected_sheet, skip = 0)
    rv_table(as.data.frame(df))
    wb <- wb_load(rv_path())
    map <- expand_merged_titles(wb, input$selected_sheet, df)
    rv_merged_map(map)
  })
  
  output$excel_table <- renderRHandsontable({
    req(rv_table())
    df <- rv_table()
    rh <- rhandsontable(df, selectCallback = TRUE, readOnly = FALSE) %>%
      hot_table(stretchH = "all")
    
    anns <- rv_annotations()
    if (length(anns) > 0) {
      for (ann in anns) {
        col <- gsub("\\d", "", ann$range)
        row <- as.numeric(gsub("\\D", "", ann$range))
        col_index <- excel_col_to_num(col)
        rh <- hot_cell(rh, row = row, col = col_index, className = ifelse(ann$obligatoire, "required", "tagged"))
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
  
  observeEvent(input$annotate_selected_zone, {
    sel <- input$excel_table_select$select
    req(sel$r1, sel$r2, sel$c1, sel$c2)
    
    df <- rv_table()
    annotations <- list()
    
    # coordonnées
    rows <- (sel$r1+1):(sel$r2+1)
    cols <- (sel$c1+1):(sel$c2+1)
    
    is_vertical <- length(cols) == 1
    is_horizontal <- length(rows) == 1
    is_matrix <- length(cols) > 1 && length(rows) > 1
    
    for (r in rows) {
      for (c in cols) {
        cell_ref <- paste0(num_to_excel_col(c), r)
        
        label <- ""
        if (is_vertical) {
          label <- paste0("Remplir ", df[1, c])
        } else if (is_matrix) {
          row_title <- df[r, sel$c1]     # première colonne comme titre
          col_title <- df[sel$r1, c]     # première ligne comme titre
          label <- paste0(row_title, " / ", col_title)
        }
        
        annotations[[length(annotations)+1]] <- list(
          range = cell_ref,
          type = "auto",
          commentaire = label,
          obligatoire = FALSE
        )
      }
    }
    
    rv_annotations(append(rv_annotations(), annotations))
    showNotification("✨ Zone annotée automatiquement", type = "message")
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

