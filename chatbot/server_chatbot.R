# server.R

server <- function(input, output, session) {
  mod_mesures_cat_server("cat1")
  mod_outil_bpss_server("bpss1")
  
  dernier_fichier_contenu <- reactiveVal(NULL)
  outil_process <- reactiveVal(NULL)
  chat_history <- reactiveVal(list())
  bpss_prompt_active <- reactiveVal(FALSE)
  typing <- reactiveVal(FALSE)
  
  donnees_extraites <- reactiveVal(data.frame(
    Axe = character(),
    Description = character(),
    Montant = numeric(),
    Unité = character(),
    Probabilite = numeric(),
    Feuille_excel = character(),
    SourcePhrase = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$send_btn, {
    handle_user_input(input, session, chat_history, typing, bpss_prompt_active)
  })
  
  observeEvent(input$extract_budget_under_bot_clicked, {
    index_of_bot <- input$extract_budget_under_bot_clicked
    user_message <- get_user_message_for_extraction(index_of_bot, chat_history, dernier_fichier_contenu)
    
    if (is.null(user_message)) {
      showNotification("Le contenu du message est introuvable.", type = "error")
      return(NULL)
    }
    
    showNotification("Recherche de données budgétaires en cours...", type = "message")
    
    future({ get_budget_data(user_message) }) %...>% (function(budget_data) {
      if (is.null(budget_data) || nrow(budget_data) == 0) {
        showNotification("Aucune donnée détectée.", type = "warning")
        return(NULL)
      }
      
      showNotification("Recherche des passages sources...", type = "message")
      
      budget_data$SourcePhrase <- sapply(1:nrow(budget_data), function(i) {
        get_source_passage(
          content_text = user_message,
          axe = budget_data$Axe[i],
          description = budget_data$Description[i],
          montant = budget_data$Montant[i]
        )
      })
      
      current_data <- donnees_extraites()
      donnees_extraites(rbind(current_data, budget_data))
      
      showNotification("Données budgétaires extraites avec les sources !", type = "message")
      show_budget_modal()
    }) %...!% (function(err) {
      showNotification(paste("Erreur durant l'extraction :", err$message), type = "error")
    })
  })
  
  observeEvent(input$file_input, {
    req(input$file_input)
    handle_file_message(input$file_input, chat_history, dernier_fichier_contenu, session, typing)
  })
  
  # observeEvent(input$lancer_outil_bpss, {
  #   tryCatch({
  #     # Debug logging
  #     message("BPSS button clicked at: ", Sys.time())
  #     
  #     port <- find_free_port()
  #     message("Port found: ", port)
  #     
  #     app_path <- normalizePath("~/work/budgibot/outil_bpss/app_outilBPSS.R")
  #     message("App path verified: ", app_path)
  #     
  #     # Create temp log file
  #     log_file <- tempfile(fileext = ".log")
  #     
  #     # Launch process with error handling
  #     proc <- processx::process$new(
  #       command = Sys.which("Rscript"),
  #       args = c("-e", sprintf(
  #         'options(shiny.sanitize.errors = FALSE);
  #       tryCatch({
  #         source("%s")
  #         shiny::runApp(
  #           shinyApp(ui, server),
  #           port = %d,
  #           host = "127.0.0.1",
  #           launch.browser = FALSE,
  #           display.mode = "normal"
  #         )
  #       }, error = function(e) {
  #         message("STARTUP ERROR: ", e$message)
  #         writeLines(as.character(e), "error.log")
  #         Sys.sleep(10) # Keep process alive to capture error
  #       })',
  #         app_path,
  #         port
  #       )),
  #       stdout = log_file,
  #       stderr = log_file,
  #       supervise = TRUE
  #     )
  #     
  #     message("Process launched (PID: ", proc$get_pid(), ")")
  #     outil_process(proc)
  #     
  #     # Check status
  #     later::later(function() {
  #       if (proc$is_alive()) {
  #         message("App running at: http://127.0.0.1:", port)
  #         showNotification(
  #           paste("Outil BPSS disponible à l'adresse :", 
  #                 tags$a(href = paste0("http://127.0.0.1:", port), 
  #                        paste0("http://127.0.0.1:", port), 
  #                        target = "_blank")),
  #           type = "message",
  #           duration = NULL
  #         )
  #         utils::browseURL(paste0("http://127.0.0.1:", port))
  #       } else {
  #         log_content <- tryCatch(readLines(log_file), error = function(e) "No log")
  #         message("Launch failed. Log:\n", paste(log_content, collapse = "\n"))
  #         showNotification(
  #           "Échec du lancement. Voir les logs pour détails.",
  #           type = "error"
  #         )
  #       }
  #     }, delay = 3)
  #     
  #   }, error = function(e) {
  #     message("Launch error: ", e$message)
  #     showNotification(
  #       paste("Erreur de lancement :", e$message),
  #       type = "error"
  #     )
  #   })
  # })
  # 
  
  output$budget_table <- renderDT({
    datatable(
      donnees_extraites(),
      options = list(scrollX = TRUE),
      editable = TRUE
    )
  })
  
  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- donnees_extraites()
    df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
    donnees_extraites(df)
  })
  
  observeEvent(input$save_budget_changes, {
    removeModal()
  })
  
  observeEvent(input$toggle_bpss_ui, {
    showModal(modalDialog(
      title = "🛠️ Outil BPSS",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fermer"),
      mod_outil_bpss_ui("bpss1")
    ))
  })
  
  observeEvent(input$lancer_outil_bpss, {
    session$sendCustomMessage(type = "jsCode", message = list(code = "$('#toggle_bpss_ui').click();"))
  })
  
  
  
  output$download_chat <- downloadHandler(
    filename = function() {
      paste("chat_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      messages <- chat_history()
      writeLines(sapply(messages, function(m) paste0(m$role, ": ", m$content)), con = file)
    }
  )
  
  output$chat_history <- renderUI({
    messages <- chat_history()
    if (length(messages) == 0) return(NULL)
    
    rendered_messages <- lapply(seq_along(messages), function(i) {
      msg <- messages[[i]]
      
      if (!is.null(msg$meta) && msg$meta == "fichier_content") return(NULL)
      
      if (!is.null(msg$type) && msg$type == "bpss_prompt") {
        return(
          div(
            class = "chat-bubble-container bot-message-container",
            div(class = "chat-sender", "BudgiBot"),
            div(class = "chat-message bot-message", "Souhaitez-vous lancer l'outil BPSS ?"),
            div(class = "quick-replies",
                actionButton("lancer_outil_bpss", "🛠️ Lancer l’outil Excel BPSS", class = "btn btn-success")
            )
          )
        )
      }
      
      if (msg$role == "assistant") {
        return(
          div(
            class = "chat-bubble-container bot-message-container",
            div(class = "chat-sender", "BudgiBot"),
            div(class = "chat-message bot-message", HTML(msg$content)),
            div(class = "quick-replies",
                actionButton(paste0("btn_detail_", i), "Peux-tu détailler ?",
                             onclick = "Shiny.setInputValue('user_input', 'Peux-tu détailler ?'); $('#send_btn').click();"),
                actionButton(paste0("btn_example_", i), "Donne-moi un exemple",
                             onclick = "Shiny.setInputValue('user_input', 'Donne-moi un exemple'); $('#send_btn').click();"),
                actionButton(paste0("btn_resume_", i), "Résume",
                             onclick = "Shiny.setInputValue('user_input', 'Résume'); $('#send_btn').click();"),
                actionButton(paste0("btn_extract_budget_", i),
                             "Extrait les données budgétaires",
                             onclick = paste0("Shiny.setInputValue('extract_budget_under_bot_clicked', ", i, ", {priority: 'event'});"))
            )
          )
        )
      }
      
      if (msg$role == "user") {
        return(
          div(
            class = "chat-bubble-container user-message-container",
            div(class = "chat-sender", "Utilisateur"),
            div(class = "chat-message user-message", msg$content)
          )
        )
      }
      
      NULL
    })
    
    tagList(Filter(Negate(is.null), rendered_messages))
  })
} 
