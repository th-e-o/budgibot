preprompt <- list(
  role = "system",
  content = "Vous êtes BudgiBot, un assistant intelligent dédié à la Direction du Budget de l'État français. Vos réponses doivent être concises, professionnelles et adaptées à un public expert. Si l'utilisateur envoie un fichier, proposez une synthèse en deux lignes et demandez ce qu'il attend de cet envoi. Tu peux également suggérer d'utiliser l'outil Excel Budgétaire (outil BPSS) si l'utilisateur parle de remplir un fichier, utiliser un PPES, BUD45 ou produire un fichier final. Si besoin, mentionne qu'un bouton est disponible. Enfin, tu as la possibilité de faire des extractions de données budgétaires via un bouton disponible dans le chat."
)

get_mistral_response <- function(chat_history) {
  api_url <- "https://api.mistral.ai/v1/chat/completions"
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  
  messages <- append(list(preprompt), chat_history)
  
  response <- POST(
    url = api_url,
    add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = list(
      model = "mistral-small-latest",
      messages = messages
    ),
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    return(content$choices[[1]]$message$content)
  } else {
    error_content <- content(response, "text")
    print(paste("Error response:", error_content))
    return("Bien pris, je t'en remercie vivement !\nMes équipes te reviennent au plus vite,\nBien à toi.")
  }
}

# 📌 Fonction pour interroger le LLM et extraire des données budgétaires
get_budget_data <- function(content_text) {
  api_url <- "https://api.mistral.ai/v1/chat/completions"
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  
  extraction_prompt <- list(
    list(
      role = "system",
      content = paste(
        "Tu es un assistant budgétaire.",
        "Analyse le texte fourni et retourne UNIQUEMENT un tableau JSON avec les données budgétaires détectées,",
        "au format suivant :",
        "[",
        "{",
        "\"Axe\": \"\",",
        "\"Description\": \"\",",
        "\"Montant\": 0,",
        "\"Unité\": \"€\",",
        "\"Probabilite\": 0.9",
        "\"Nature\":", 
      
        "}",
        "]",
        "La nature peut prendre deux valeurs : Flux d'effectifs ou Mesure catégorielle.",
        "NE FOURNIS PAS D'EXPLICATION EN DEHORS DU JSON."
      )
    ),
    list(
      role = "user",
      content = paste("Analyse ce texte budgétaire :\n\n", content_text)
    )
  )
  
  # ➡️ Appel API
  response <- httr::POST(
    url = api_url,
    httr::add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = list(
      model = "mistral-small-latest",
      messages = extraction_prompt
    ),
    encode = "json"
  )
  
  if (httr::status_code(response) != 200) {
    message("Erreur API LLM 1 : ", httr::content(response, "text"))
    return(NULL)
  }
  
  raw_output <- httr::content(response, "parsed")$choices[[1]]$message$content
  cleaned_text <- raw_output %>%
    gsub("```json", "", ., fixed = TRUE) %>%
    gsub("```", "", ., fixed = TRUE) %>%
    trimws()
  
  json_candidate <- extract_json_array(cleaned_text)
  
  if (is.null(json_candidate)) {
    message("Aucune donnée budgétaire détectée.")
    return(NULL)
  }
  
  parsed_data <- jsonlite::fromJSON(json_candidate)
  
  return(parsed_data)
}


get_source_passage <- function(content_text, axe, description, montant) {
  api_url <- "https://api.mistral.ai/v1/chat/completions"
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  
  sourcing_prompt <- list(
    list(
      role = "system",
      content = paste(
        "Tu es un assistant budgétaire rigoureux et précis.",
        "Ta mission est de RETROUVER le PASSAGE EXACT du texte ci-dessous qui JUSTIFIE la donnée suivante :",
        "- Axe : ", axe, "\n",
        "- Description : ", description, "\n",
        "- Montant : ", montant, "\n",
        "",
        "⚠️ IMPORTANT :",
        "- TU DOIS COPIER-COLLER le passage EXACT tel qu'il apparaît dans le texte.",
        "- NE RÉSUME PAS. NE RÉÉCRIS PAS. NE PARAPHRASE PAS.",
        "- Si tu ne retrouves pas le passage exact, retourne : \"NON TROUVÉ\".",
        "",
        "EXEMPLE DE FORMAT ATTENDU :",
        "\"Le ministre veut du catégoriel pour les militaires du rang genre 55M€ en année pleine au 1er mars 2025.\"",
        "",
        "NE FOURNIS AUCUNE EXPLICATION NI CONTEXTE. JUSTE LA PHRASE OU LE PASSAGE EXACT.",
        "⚠️ TA RÉPONSE SERA VÉRIFIÉE AUTOMATIQUEMENT.",
        "Si le passage que tu donnes n'est pas **copié-collé** du texte fourni, il sera REJETÉ."
        
      )
    ),
    list(
      role = "user",
      content = paste("Voici le texte budgétaire :\n\n", content_text)
    )
  )
  
  response <- httr::POST(
    url = api_url,
    httr::add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = list(
      model = "mistral-small-latest",
      messages = sourcing_prompt
    ),
    encode = "json"
  )
  
  if (httr::status_code(response) != 200) {
    message("Erreur API sourcing : ", httr::content(response, "text"))
    return(NULL)
  }
  
  raw_output <- httr::content(response, "parsed")$choices[[1]]$message$content
  
  cleaned_output <- trimws(raw_output)
  
  
  
  
  # Si la réponse est vide ou "NON TROUVÉ", on le gère
  if (tolower(cleaned_output) %in% c("non trouvé", "non trouve", "not found", "")) {
    return(NA)
  }
  
  if (!verify_llm_passage(content_text, cleaned_output)) {
    message("❌ Passage halluciné détecté — rejeté.")
    return(NA)
  }
  
  
  return(cleaned_output)
}




