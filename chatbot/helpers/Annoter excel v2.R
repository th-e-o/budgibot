library(shiny)
library(readxl)
library(rhandsontable)
library(openxlsx2)
library(httr)
library(jsonlite)
library(DT)

ui <- fluidPage(
  titlePanel("📊 Excel Template Builder with AI Assistant"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "workflow_tabs",
        tabPanel("Template Setup",
                 fileInput("upload_file", "1. Upload Excel Template", accept = ".xlsx"),
                 uiOutput("sheet_selector"),
                 actionButton("detect_template", "2. Detect Placeholders", icon = icon("search")),
                 hr(),
                 h4("Template Fields"),
                 DTOutput("field_mapping_table"),
                 actionButton("save_template", "Save Template Config", class = "btn-success")
        ),
        tabPanel("AI Completion",
                 selectInput("llm_provider", "AI Provider:", 
                             choices = c("Mistral" = "mistral", "OpenAI" = "openai")),
                 textAreaInput("completion_prompt", "Instructions for AI:", 
                               placeholder = "Extract the following fields from the document..."),
                 fileInput("source_doc", "Source Document (text/PDF)", multiple = FALSE),
                 actionButton("run_completion", "Generate Content", class = "btn-primary"),
                 hr(),
                 downloadButton("download_result", "Download Completed File")
        )
      )
    ),
    mainPanel(
      rHandsontableOutput("excel_preview"),
      hr(),
      h4("AI Completion Results"),
      verbatimTextOutput("completion_log")
    )
  ),
  tags$head(tags$style("
    .placeholder { background-color: #FFF3CD !important; }
    .detected-field { border: 2px solid #28a745 !important; }
  "))
)

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    workbook = NULL,
    sheets = NULL,
    current_sheet = NULL,
    field_mapping = data.frame(
      field_name = character(),
      cell_ref = character(),
      data_type = character(),
      description = character(),
      sample_value = character(),
      stringsAsFactors = FALSE
    ),
    completion_results = NULL
  )
  
  # 1. File upload and sheet selection
  observeEvent(input$upload_file, {
    req(input$upload_file)
    tryCatch({
      rv$sheets <- excel_sheets(input$upload_file$datapath)
      rv$workbook <- wb_load(input$upload_file$datapath)
      updateSelectInput(session, "selected_sheet", choices = rv$sheets)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  output$sheet_selector <- renderUI({
    req(rv$sheets)
    selectInput("selected_sheet", "Select Sheet:", choices = rv$sheets)
  })
  
  # 2. Detect template placeholders
  observeEvent(input$detect_template, {
    req(rv$workbook, input$selected_sheet)
    
    tryCatch({
      sheet_data <- wb_to_df(rv$workbook, sheet = input$selected_sheet)
      
      # Safely handle empty sheets or invalid data
      if (is.null(sheet_data) || ncol(sheet_data) == 0 || nrow(sheet_data) == 0) {
        showNotification("Sheet is empty or invalid", type = "warning")
        return()
      }
      
      placeholder_pattern <- "\\{(.*?)\\}"
      rv$field_mapping <- rv$field_mapping[0, ]  # Reset mapping
      
      # Safely detect placeholders with proper NA handling
      for (col in colnames(sheet_data)) {
        # Ensure column exists and is character type
        if (!col %in% names(sheet_data)) next
        
        for (row in seq_len(nrow(sheet_data))) {
          cell_value <- sheet_data[row, col]
          
          # Robust condition checking:
          if (!is.null(cell_value) && 
              !is.na(cell_value) && 
              is.character(cell_value) && 
              nzchar(trimws(cell_value)) && 
              grepl(placeholder_pattern, cell_value)) {
            
            matches <- regmatches(cell_value, regexec(placeholder_pattern, cell_value))[[1]]
            if (length(matches) > 1) {
              field_name <- matches[2]
              cell_ref <- paste0(col, row)
              
              new_field <- data.frame(
                field_name = field_name,
                cell_ref = cell_ref,
                data_type = detect_data_type(sheet_data, col),
                description = "",
                sample_value = "",
                stringsAsFactors = FALSE
              )
              
              rv$field_mapping <- rbind(rv$field_mapping, new_field)
            }
          }
        }
      }
      
      if (nrow(rv$field_mapping) > 0) {
        showNotification(paste("Found", nrow(rv$field_mapping), "template fields"))
      } else {
        showNotification("No template placeholders found (use {field_name} format)", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(paste("Detection failed:", e$message), type = "error")
    })
  })
  
    # 3. Display and edit field mapping
      output$field_mapping_table <- renderDT({
        datatable(rv$field_mapping, 
                  editable = list(target = 'cell', disable = list(columns = c(0,1))),
                  options = list(scrollX = TRUE))
      })
      
      observeEvent(input$field_mapping_table_cell_edit, {
        info <- input$field_mapping_table_cell_edit
        rv$field_mapping[info$row, info$col] <- info$value
      })
      
      # 4. Preview Excel with visual cues
      # In the server function, replace the excel_preview render with this:
      
      output$excel_preview <- renderRHandsontable({
        req(rv$workbook, input$selected_sheet)
        
        tryCatch({
          sheet_data <- wb_to_df(rv$workbook, sheet = input$selected_sheet)
          
          # Ensure we have a data frame with at least one column
          if (ncol(sheet_data) == 0) {
            sheet_data <- data.frame(Message = "Empty sheet or no data found")
          }
          
          # Convert all columns to character to avoid rendering issues
          sheet_data <- as.data.frame(lapply(sheet_data, as.character), stringsAsFactors = FALSE)
          
          rh <- rhandsontable(sheet_data, 
                              readOnly = TRUE, 
                              width = '100%',
                              height = '400px') %>%
            hot_table(stretchH = "all", overflow = "visible")
          
          # Only try to highlight if we have fields and valid data
          if (nrow(rv$field_mapping) > 0 && ncol(sheet_data) > 0) {
            for (i in seq_len(nrow(rv$field_mapping))) {
              cell_ref <- rv$field_mapping$cell_ref[i]
              col <- gsub("\\d", "", cell_ref)
              row <- as.numeric(gsub("\\D", "", cell_ref))
              
              # Validate the cell reference exists
              if (col %in% colnames(sheet_data) && row <= nrow(sheet_data)) {
                rh <- hot_cell(rh, row = row, col = which(colnames(sheet_data) == col), 
                               className = "detected-field")
              }
            }
          }
          
          rh
        }, error = function(e) {
          # Fallback display if error occurs
          rhandsontable(data.frame(Error = paste("Could not display sheet:", e$message)),
                        readOnly = TRUE)
        })
      })
      
      # 5. AI-powered completion
      observeEvent(input$run_completion, {
        req(input$source_doc, nrow(rv$field_mapping) > 0)
        
        showNotification("Processing document with AI...", duration = NULL)
        
        tryCatch({
          # Prepare the prompt
          fields <- paste(rv$field_mapping$field_name, collapse = ", ")
          instructions <- paste(
            "Extract the following fields from the document:",
            fields,
            "\n\nReturn a JSON object with these fields. If a field is missing, use null.",
            "\n\nAdditional instructions:", input$completion_prompt
          )
          
          # Read source document
          doc_text <- if (grepl("\\.pdf$", input$source_doc$name)) {
            pdf_text(input$source_doc$datapath) %>% paste(collapse = "\n")
          } else {
            readLines(input$source_doc$datapath) %>% paste(collapse = "\n")
          }
          
          # Call LLM (example with Mistral - similar to your original)
          completion <- call_llm(
            provider = input$llm_provider,
            prompt = paste(instructions, "\n\nDOCUMENT:\n", doc_text),
            fields = rv$field_mapping$field_name
          )
          
          rv$completion_results <- completion
          updateTabsetPanel(session, "workflow_tabs", selected = "AI Completion")
          showNotification("AI completion successful!")
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
      
      # LLM calling function
      call_llm <- function(provider, prompt, fields) {
        if (provider == "mistral") {
          api_url <- "https://api.mistral.ai/v1/chat/completions"
          api_key <- Sys.getenv("MISTRAL_API_KEY")
          
          body <- list(
            model = "mistral-small",
            messages = list(list(role = "user", content = prompt)),
            response_format = list(type = "json_object")
          )
          
          response <- POST(
            url = api_url,
            add_headers(Authorization = paste("Bearer", api_key)),
            encode = "json",
            body = body
          )
          
          content <- content(response, "text", encoding = "UTF-8")
          fromJSON(content)$choices[[1]]$message$content
        } else {
          print("Pas d'API")
        }
      }
      
      # 6. Save and export functionality
      output$download_result <- downloadHandler(
        filename = function() paste0("completed_", Sys.Date(), ".xlsx"),
        content = function(file) {
          req(rv$completion_results, rv$workbook)
          
          # Apply completion results to workbook
          for (field in names(rv$completion_results)) {
            cell_ref <- rv$field_mapping$cell_ref[rv$field_mapping$field_name == field]
            if (length(cell_ref)) {
              col <- gsub("[0-9]", "", cell_ref)
              row <- as.numeric(gsub("[A-Za-z]", "", cell_ref))
              value <- rv$completion_results[[field]]
              
              # Write to workbook
              wb_add_data(rv$workbook, 
                          sheet = input$selected_sheet,
                          x = value,
                          startCol = col,
                          startRow = row)
            }
          }
          
          wb_save(rv$workbook, file)
        }
      )
      
      # Display completion results
      output$completion_log <- renderPrint({
        req(rv$completion_results)
        cat("AI Extracted Values:\n")
        str(rv$completion_results)
      })
}

shinyApp(ui, server)