# mod_mobile_rx.R

mobile_rx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("rx_main_container"))
  )
}

mobile_rx_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Local Reactive State ---
    rx_master <- reactiveVal(data.frame())
    rx_meds <- reactiveValues(df = data.frame(
      brand_name = character(), generic = character(), dose = character(), 
      freq = character(), route = character(), duration = character(),
      stringsAsFactors = FALSE
    ))
    rx_meta <- reactiveValues(is_history = FALSE, history_date = NULL)
    
    # --- Load Drug Library ---
    load_rx_master <- function() { 
      if (is.null(pool)) return(NULL)
      tryCatch({
        data <- dbGetQuery(pool, "SELECT * FROM drug_master")
        rx_master(data)
      }, error = function(e) message("Error loading drug master: ", e$message))
    }
    
    observe({ req(pool); load_rx_master() })
    rx_search_val <- reactive({ input$rx_q }) %>% debounce(300)
    
    # --- Function to fetch the Last Prescription ---
    load_rx_history <- function(pt_id) {
      req(pt_id, pool)
      tryCatch({
        res <- dbGetQuery(pool, 
                          "SELECT meds_json, visit_date FROM prescriptions 
                          WHERE patient_id::text = $1 
                          ORDER BY visit_date DESC LIMIT 1", 
                          list(as.character(pt_id)))
        
        if(nrow(res) > 0 && nzchar(res$meds_json[1])) {
          rx_meta$is_history <- TRUE
          rx_meta$history_date <- as.character(res$visit_date[1])
          
          raw_data <- fromJSON(res$meds_json[1])
          df <- if(!is.data.frame(raw_data)) as.data.frame(raw_data) else raw_data
          
          # Fix column name if saved as 'brand'
          if("brand" %in% names(df)) names(df)[names(df) == "brand"] <- "brand_name"
          
          required_cols <- c("brand_name", "generic", "dose", "freq", "route", "duration")
          for(col in required_cols) { if(!(col %in% names(df))) df[[col]] <- "" }
          
          rx_meds$df <- df[, required_cols, drop = FALSE]
        } else {
          rx_meta$is_history <- FALSE
          rx_meds$df <- data.frame(brand_name=character(), generic=character(), 
                                   dose=character(), freq=character(), 
                                   route=character(), duration=character(), 
                                   stringsAsFactors = FALSE)
        }
      }, error = function(e) message("Rx History Load Error: ", e$message))
    }
    
    # =========================================================
    # AUTO-LOAD TRIGGER: This ensures the last Rx shows up immediately
    # =========================================================
    observeEvent(current_pt(), {
      req(current_pt()$id)
      load_rx_history(current_pt()$id)
    }, priority = 10)
    
    # --- UI Container ---
    output$rx_main_container <- renderUI({
      req(current_pt())
      is_h <- isTRUE(rx_meta$is_history)
      
      card(
        card_header(
          div(class="d-flex justify-content-between align-items-center",
              span(icon("prescription"), " Medication Builder"),
              if(is_h) span(class="badge bg-warning text-dark", paste("Last Rx:", rx_meta$history_date))
          )
        ),
        card_body(
          div(class = "mb-3",
              textInput(ns("rx_q"), "Search Drug Library", placeholder = "Enter brand name"),
              uiOutput(ns("rx_search_results"))),
          hr(),
          tags$label(strong("Current Prescription")),
          div(class = "rx-list-container mb-3", uiOutput(ns("rx_list"))),
          accordion(accordion_panel("Edit Table View", icon = icon("table"), rHandsontableOutput(ns("rx_hot"))))
        ),
        card_footer(div(class="d-grid gap-2", actionButton(ns("save_rx"), "Save Prescription", class="btn-success btn-lg")))
      )
    })
    
    # --- Search & List Logic ---
    output$rx_search_results <- renderUI({
      term <- rx_search_val(); req(term); req(nchar(term) >= 2)
      matches <- rx_master() %>% filter(grepl(term, brand_name, ignore.case=T)) %>% head(5)
      if(nrow(matches) == 0) return(actionButton(ns("new_drug_modal"), "Register New Drug", class="btn-warning w-100"))
      lapply(1:nrow(matches), function(i) {
        d <- matches[i, ]
        div(class="search-item", style = "border-bottom: 1px solid #eee; padding: 10px; cursor: pointer; background: white;",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("add_rx_id"), d$id),
            strong(d$brand_name), span(paste0(" (", d$generic, ")")))
      })
    })
    
    # --- Corrected Medication List (Cards) ---
    output$rx_list <- renderUI({
      df <- rx_meds$df
      if (is.null(df) || nrow(df) == 0) return(div(class="text-center p-4 text-muted", "Empty"))
      
      lapply(1:nrow(df), function(i) {
        div(class="d-flex justify-content-between align-items-center p-3 mb-2 border rounded bg-white",
            div(
              strong(df$brand_name[i]), 
              # FIX: Use tags$small instead of just small()
              div(tags$small(paste(df$dose[i], "|", df$freq[i], "|", df$duration[i])))
            ),
            actionButton(ns(paste0("del_", i)), NULL, icon("trash-can"), class="btn-outline-danger border-0",
                         onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("del_rx_idx"), i)))
      })
    })
    
    # --- Handlers ---
    observeEvent(input$add_rx_id, {
      new_d <- rx_master() %>% filter(id == input$add_rx_id) %>% select(brand_name, generic, dose, freq, route, duration)
      rx_meds$df <- bind_rows(rx_meds$df, new_d); updateTextInput(session, "rx_q", value = "")
    })
    observeEvent(input$del_rx_idx, { rx_meds$df <- rx_meds$df[-as.numeric(input$del_rx_idx), ] })
    output$rx_hot <- renderRHandsontable({ req(nrow(rx_meds$df) > 0); rhandsontable(rx_meds$df, stretchH = "all") })
    
    # Inside mod_mobile_rx_server
    observeEvent(input$save_rx, {
      req(current_pt(), user_info())
      
      # 1. Setup Auth and Identity
      curr_user <- user_info()$username
      pt_id <- as.character(current_pt()$id)
      
      # 2. Prepare JSON Payload from the reactive meds dataframe
      # This matches your existing logic but prepares it for the new tracking columns
      json_str <- as.character(jsonlite::toJSON(rx_meds$df, auto_unbox = TRUE))
      
      .with_conn(pool, {
        tryCatch({
          # 3. INSERT the new prescription record
          # We always INSERT new records for prescriptions to maintain a historical timeline
          res <- DBI::dbGetQuery(con, glue::glue_sql("
        INSERT INTO prescriptions (patient_id, meds_json, created_by, created_at)
        VALUES ({pt_id}, {json_str}, {curr_user}, NOW())
        RETURNING id
      ", .con = con))
          
          # 4. Log the Audit Action
          log_audit(
            con = con, 
            user_id = curr_user, 
            action = "CREATE", 
            table = "prescriptions", 
            record_id = res$id
          )
          
          showNotification("Prescription Saved Successfully", type = "message")
          
          # Optional: Reset UI state after save
          rx_meta$is_history <- FALSE
          
        }, error = function(e) {
          showNotification(paste("Prescription Save Failed:", e$message), type = "error")
        })
      })
    })
  })
}