# app_mesures_cat.R (mise à jour avec sélection de feuille + détection de type de zone + visualisation des en-têtes)

library(shiny)
library(readxl)
library(rhandsontable)
library(openxlsx2)
library(reactable)
library(httr)
library(jsonlite)

num_to_excel_col <- function(n) {
  if (is.null(n) || length(n) == 0 || is.na(n)) return("?")
  div <- n
  col <- ""
  while (div > 0) {
    mod <- (div - 1) %% 26
    col <- paste0(LETTERS[mod + 1], col)
    div <- (div - mod - 1) %/% 26
  }
  return(col)
}

excel_col_to_num <- function(col_str) {
  chars <- strsplit(col_str, "")[[1]]
  sum((match(chars, LETTERS)) * 26^(rev(seq_along(chars)) - 1))
}

# Détection améliorée des en-têtes
is_header <- function(x) {
  !is.na(x) & (
    (is.character(x) & nchar(x) > 0 & nchar(x) < 50 & !grepl("^\\d{5,}$", x)) |
      (is.numeric(x) & x >= 1900 & x <= 2100)
  )
}

get_zone_type <- function(df, selected_rows, selected_cols) {
  if (length(selected_rows) == 0 || length(selected_cols) == 0) {
    return("indéterminée")
  }
  header_row <- max(1, min(selected_rows) - 1)
  header_col <- max(1, min(selected_cols) - 1)
  horizontal_headers <- df[header_row, selected_cols, drop = FALSE]
  vertical_headers <- df[selected_rows, header_col, drop = FALSE]
  
  has_horizontal <- any(apply(horizontal_headers, 2, is_header))
  has_vertical <- any(sapply(vertical_headers, is_header))
  
  if (has_horizontal && has_vertical) return("matricielle")
  if (has_vertical) return("verticale")
  if (has_horizontal) return("horizontale")
  return("indéterminée")
}

ui <- fluidPage(
  titlePanel("📊 Annotateur Excel"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload_file", "📂 Charger un fichier Excel (.xlsx)"),
      uiOutput("sheet_selector"),
      actionButton("load_sheet", "📄 Charger la feuille"),
      actionButton("auto_detect_zone", "🔍 Détecter zone + type", class = "btn btn-info mt-2"),
      actionButton("tag_zone", "➕ Taguer cette zone", class = "btn btn-success mt-2"),
      actionButton("manual_tag", "📝 Ajouter comme tag manuel", class = "btn btn-warning mt-2"),
      
      verbatimTextOutput("zone_type"),
      tags$div(
        style = "margin-top: 10px;",
        textOutput("selection_info"),
        textOutput("zone_stats")
      ),
      downloadButton("download_tags", "📤 Exporter les tags")
    ),
    mainPanel(
      rHandsontableOutput("excel_table"),
      hr(),
      h4("📋 Tags enregistrés"),
      reactableOutput("tag_table")
      
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    all_sheets = NULL,
    df = NULL,
    selected = NULL,
    tags = list(),
    zone_type = NULL,
    current_rows = NULL,
    current_cols = NULL,
    header_highlights = list()
  )
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    rv$all_sheets <- readxl::excel_sheets(input$upload_file$datapath)
  })
  
  output$sheet_selector <- renderUI({
    req(rv$all_sheets)
    selectInput("selected_sheet", "Feuilles disponibles", choices = rv$all_sheets)
  })
  
  observeEvent(input$load_sheet, {
    req(input$upload_file, input$selected_sheet)
    rv$df <- read_excel(input$upload_file$datapath, sheet = input$selected_sheet, col_names = FALSE)
  })
  
  observe({
    sel <- input$excel_table_select$select
    if (!is.null(sel) &&
        !is.null(sel$r) && !is.null(sel$r2) &&
        !is.null(sel$c) && !is.null(sel$c2)) {
      
      rv$selected <- list(
        startRow = sel$r,
        endRow = sel$r2,
        startCol = sel$c,
        endCol = sel$c2
      )
    } else {
      rv$selected <- NULL
    }
  })
  
  
  
  observe({
    print("Sélection captée :")
    print(rv$selected)
  })
  
  
  output$excel_table <- renderRHandsontable({
    req(rv$df)
    df <- rv$df
    df_char <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
    
    # 🔷 Marquer la zone sélectionnée
    # 🔷 Marquer la zone sélectionnée
    if (!is.null(rv$current_rows) && !is.null(rv$current_cols)) {
      for (r in rv$current_rows) {
        for (c in rv$current_cols) {
          if (r <= nrow(df_char) && c <= ncol(df_char)) {
            val <- df_char[r, c]
            if (!is.na(val)) {
              val <- as.character(val)
              if (!startsWith(val, "🔷")) {
                df_char[r, c] <- paste0("🔷 ", val)
              }
            } else {
              # Si NA, on affiche juste "🔷"
              df_char[r, c] <- "🔷"
            }
          }
        }
      }
    }
    
    
    # 🏷️ Marquer les cellules d'en-tête détectées
    if (!is.null(rv$header_highlights)) {
      for (cell in rv$header_highlights) {
        r <- cell$row
        c <- cell$col
        if (r <= nrow(df_char) && c <= ncol(df_char)) {
          val <- as.character(df_char[r, c])
          if (!is.na(val) && nzchar(val) && !startsWith(val, "🏷️")) {
            df_char[r, c] <- paste0("🏷️ ", val)
          }
        }
      }
    }
    
    if (length(rv$tags) > 0) {
      tag_df <- do.call(rbind, rv$tags)
      for (i in seq_len(nrow(tag_df))) {
        r <- tag_df$row[i]
        c <- tag_df$col[i]
        emoji <- switch(
          tag_df$type[i],
          "matricielle" = "🏷️",
          "verticale" = "🏷️",
          "horizontale" = "🏷️",
          "manuel" = "📝",
          "🔷"  # fallback si inconnu
        )
        if (r <= nrow(df_char) && c <= ncol(df_char)) {
          val <- df_char[r, c]
          if (!startsWith(val, emoji)) {
            df_char[r, c] <- paste0(emoji, " ", val)
          }
        }
      }
    }
    
    rhandsontable(df_char, selectCallback = TRUE) %>%
      hot_table(selection = list(mode = "range"))
    

  })
  
  
  
  output$selection_info <- renderText({
    sel <- rv$selected
    if (is.null(sel)) return("Aucune sélection active")
    
    if (any(sapply(sel, is.null))) return("Sélection partielle détectée")
    
    paste("Sélection : Lignes", sel$startRow, "à", sel$endRow,
          "| Colonnes", num_to_excel_col(sel$startCol), "à", num_to_excel_col(sel$endCol))
  })
  
  
  output$zone_stats <- renderText({
    req(rv$current_rows, rv$current_cols)
    paste("Cellules analysées :", length(rv$current_rows) * length(rv$current_cols))
  })
  
  observeEvent(input$auto_detect_zone, {
    req(rv$df, rv$selected)
    sel <- rv$selected
    
    rows <- sel$startRow:sel$endRow
    cols <- sel$startCol:sel$endCol
    rv$current_rows <- rows
    rv$current_cols <- cols
    
    if (any(rows < 1)) {
      showNotification("⚠️ Lignes sélectionnées invalides (hors limites)", type = "error")
      return()
    }
    if (any(cols < 1)) {
      showNotification("⚠️ Colonnes sélectionnées invalides (hors limites)", type = "error")
      return()
    }
    
    # 🧠 Détection du type
    rv$zone_type <- get_zone_type(rv$df, rows, cols)
    
    # 🎯 Surlignage des en-têtes utilisés
    header_row <- max(1, min(rows) - 1)
    header_col <- max(1, min(cols) - 1)
    
    header_cells <- list()
    
    # Surligner la ligne d'en-tête si elle est en dehors de la sélection
    if (header_row < min(rows)) {
      header_cells <- c(header_cells, lapply(cols, function(c) list(row = header_row, col = c)))
    }
    
    # Surligner la colonne d'en-tête si elle est en dehors de la sélection
    if (header_col < min(cols)) {
      header_cells <- c(header_cells, lapply(rows, function(r) list(row = r, col = header_col)))
    }
    
    rv$header_highlights <- header_cells
    
  })
  
  observeEvent(input$manual_tag, {
    req(rv$df, rv$selected)
    
    sel <- rv$selected
    rows <- sel$startRow:sel$endRow
    cols <- sel$startCol:sel$endCol
    df <- rv$df
    
    tag_entries <- list()
    
    for (r in rows) {
      for (c in cols) {
        val <- df[r, c][[1]]
        if (!is.na(val) && nzchar(as.character(val))) {
          tag_entries[[length(tag_entries) + 1]] <- data.frame(
            row = r,
            col = c,
            tag = as.character(val),
            type = "manuel",
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(tag_entries) == 0) {
      showNotification("Aucune cellule remplie sélectionnée 📭", type = "warning")
    } else {
      rv$tags <- c(rv$tags, tag_entries)
      showNotification(paste(length(tag_entries), "tag(s) manuels ajoutés 📝"), type = "message")
    }
  })
  
  
  observeEvent(input$tag_zone, {
    req(rv$df, rv$current_rows, rv$current_cols, rv$zone_type)
    
    df <- rv$df
    rows <- rv$current_rows
    cols <- rv$current_cols
    type <- rv$zone_type
    
    header_row <- max(1, min(rows) - 1)
    header_col <- max(1, min(cols) - 1)
    
    tag_entries <- list()
    
    if (type == "matricielle") {
      row_labels <- df[rows, header_col, drop = TRUE]
      col_labels <- df[header_row, cols, drop = TRUE]
      
      for (i in seq_along(rows)) {
        for (j in seq_along(cols)) {
          r <- rows[i]
          c <- cols[j]
          row_label <- as.character(row_labels[i])
          col_label <- as.character(col_labels[j])
          tag <- paste(row_label, col_label, sep = " - ")
          tag_entries[[length(tag_entries) + 1]] <- data.frame(
            row = r,
            col = c,
            tag = tag,
            type = type,
            stringsAsFactors = FALSE
          )
        }
      }
    } else if (type == "verticale") {
      row_labels <- df[rows, header_col, drop = TRUE]
      for (i in seq_along(rows)) {
        r <- rows[i]
        tag <- as.character(row_labels[i])
        for (c in cols) {
          tag_entries[[length(tag_entries) + 1]] <- data.frame(
            row = r,
            col = c,
            tag = tag,
            type = type,
            stringsAsFactors = FALSE
          )
        }
      }
    } else if (type == "horizontale") {
      col_labels <- df[header_row, cols, drop = TRUE]
      for (j in seq_along(cols)) {
        c <- cols[j]
        tag <- as.character(col_labels[j])
        for (r in rows) {
          tag_entries[[length(tag_entries) + 1]] <- data.frame(
            row = r,
            col = c,
            tag = tag,
            type = type,
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      showNotification("❌ Zone indéterminée : taggage impossible", type = "error")
      return()
    }
    
    rv$tags <- c(rv$tags, tag_entries)
    showNotification(paste(length(tag_entries), "tags ajoutés ✅"), type = "message")
  })
  
  
  output$zone_type <- renderText({
    req(rv$zone_type, rv$current_rows, rv$current_cols)
    text <- paste("🧠 Type de zone détecté :", rv$zone_type)
    
    if (rv$zone_type %in% c("matricielle", "horizontale")) {
      header_row <- max(1, min(rv$current_rows) - 1)
      col_labels <- rv$df[header_row, rv$current_cols, drop = TRUE]
      text <- paste0(text, "\n🔹 En-têtes colonnes : ", paste(na.omit(col_labels), collapse = " | "))
    }
    if (rv$zone_type %in% c("matricielle", "verticale")) {
      header_col <- max(1, min(rv$current_cols) - 1)
      row_labels <- rv$df[rv$current_rows, header_col, drop = TRUE]
      text <- paste0(text, "\n🔸 En-têtes lignes : ", paste(na.omit(row_labels), collapse = " | "))
    }
    
    text
  })
  
  
  output$download_tags <- downloadHandler(
    filename = function() paste0("annotations_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(do.call(rbind, lapply(rv$tags, as.data.frame)), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
