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
    
    empty_rx_df <- function() {
      data.frame(
        brand_name = character(), generic = character(), dose = character(), 
        freq = character(), route = character(), duration = character(),
        stringsAsFactors = FALSE
      )
    }
    
    rx_meds <- reactiveValues(df = empty_rx_df())
    rx_meta <- reactiveValues(is_history = FALSE, history_date = NULL)
    
    # --- Load Drug Library ---
    load_rx_master <- function() { 
      req(pool)
      tryCatch({
        data <- dbGetQuery(pool, "SELECT * FROM drug_master ORDER BY brand_name ASC")
        rx_master(data)
      }, error = function(e) message("Error loading drug master: ", e$message))
    }
    
    observe({ load_rx_master() })
    rx_search_val <- reactive({ input$rx_q }) %>% debounce(300)
    
    # --- Function to fetch the Last Prescription ---
    load_rx_history <- function(pt_id) {
      req(pt_id, pool)
      print(paste("Attempting to load Rx for ID:", pt_id, "Type:", class(pt_id)))
      
      tryCatch({
        res <- dbGetQuery(pool, 
                          "SELECT meds_json, visit_date FROM prescriptions 
                          WHERE TRIM(patient_id::text) = TRIM($1::text) 
                          ORDER BY visit_date DESC LIMIT 1", 
                          list(as.character(pt_id)))
        
        if (nrow(res) > 0) {
          # DATE SAFETY: Prevent coercion errors
          raw_date <- res$visit_date[1]
          clean_date <- tryCatch({
            as.character(as.Date(raw_date)) 
          }, error = function(e) "Unknown Date")
          
          json_content <- res$meds_json[1]
          is_valid_string <- !is.na(json_content) && nchar(trimws(json_content)) > 2
          
          if (is_valid_string) {
            raw_data <- tryCatch(jsonlite::fromJSON(json_content), error = function(e) NULL)
            
            if (!is.null(raw_data)) {
              rx_meta$is_history <- TRUE
              rx_meta$history_date <- clean_date
              
              df <- if (!is.data.frame(raw_data)) as.data.frame(raw_data) else raw_data
              if ("brand" %in% names(df)) names(df)[names(df) == "brand"] <- "brand_name"
              
              required_cols <- c("brand_name", "generic", "dose", "freq", "route", "duration")
              for (col in required_cols) { if (!(col %in% names(df))) df[[col]] <- "" }
              
              rx_meds$df <- df[, required_cols, drop = FALSE]
            } else {
              rx_meds$df <- empty_rx_df()
              rx_meta$is_history <- FALSE
            }
          }
        } else {
          rx_meta$is_history <- FALSE
          rx_meta$history_date <- NULL
          rx_meds$df <- empty_rx_df()
        }
      }, error = function(e) {
        message("Database Error: ", e$message)
        rx_meds$df <- empty_rx_df()
      })
    }
    
    observeEvent(current_pt(), {
      req(current_pt()$id)
      load_rx_history(current_pt()$id)
    }, priority = 10)
    
    observeEvent(input$rx_hot, {
      # Sync table edits back to reactive state
      rx_meds$df <- rhandsontable::hot_to_r(input$rx_hot)
    })
    
    # --- Main UI Container ---
    output$rx_main_container <- renderUI({
      req(current_pt())
      card(
        card_header(
          div(class="d-flex justify-content-between align-items-center",
              span(icon("prescription"), " Medication Builder"),
              if(rx_meta$is_history) span(class="badge bg-info", paste("Loaded:", rx_meta$history_date))
          )
        ),
        card_body(
          textInput(ns("rx_q"), "Search & Add Drug", placeholder = "Start typing..."),
          uiOutput(ns("rx_search_results")),
          hr(),
          
          tags$label(strong("Prescription Summary")),
          uiOutput(ns("rx_list")), 
          
          hr(),
          
          tags$label(strong("Edit Details (Table View)")),
          rHandsontableOutput(ns("rx_hot"))
        ),
        card_footer(
          actionButton(ns("save_rx"), "Save Prescription", class="btn-success w-100 btn-lg")
        )
      )
    })
    
    # --- Search Results UI ---
    output$rx_search_results <- renderUI({
      term <- rx_search_val(); req(term); req(nchar(term) >= 2)
      matches <- rx_master() %>% 
        filter(grepl(term, brand_name, ignore.case=T) | grepl(term, generic, ignore.case=T)) %>% 
        head(5)
      
      if(nrow(matches) == 0) return(actionButton(ns("new_drug_modal"), "Add New to Library", class="btn-sm btn-warning w-100"))
      
      lapply(1:nrow(matches), function(i) {
        d <- matches[i, ]
        div(style = "border: 1px solid #eee; border-radius: 8px; padding: 12px; margin-bottom: 8px; cursor: pointer; background: white;",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("add_rx_id"), d$id),
            div(strong(d$brand_name), tags$small(style="color:#666;", paste0(" (", d$generic, ")"))),
            div(style = "font-size: 0.8rem; color: #0d6efd; margin-top: 4px; display: flex; gap: 8px; flex-wrap: wrap;",
                span(icon("pills"), d$dose),
                span(icon("clock"), d$freq),
                span(icon("route"), d$route),
                span(icon("calendar-day"), d$duration)
            )
        )
      })
    })
    
    # --- Card Summary UI ---
    output$rx_list <- renderUI({
      df <- rx_meds$df
      if(nrow(df) == 0) return(div(class="text-center text-muted p-3 border rounded bg-light", "No medications added."))
      
      lapply(1:nrow(df), function(i) {
        div(class="p-3 mb-2 border rounded shadow-sm bg-white",
            div(class="d-flex justify-content-between align-items-start",
                div(
                  div(strong(df$brand_name[i]), span(class="text-muted", style="font-size:0.8rem;", paste0(" (", df$generic[i], ")"))),
                  div(style="font-size: 0.85rem; margin-top: 8px;",
                      span(class="badge bg-primary-subtle text-primary border me-1", icon("pills"), df$dose[i]),
                      span(class="badge bg-info-subtle text-info border me-1", icon("clock"), df$freq[i]),
                      span(class="badge bg-success-subtle text-success border me-1", icon("route"), df$route[i]),
                      span(class="badge bg-warning-subtle text-warning border", icon("calendar-day"), df$duration[i])
                  )
                ),
                actionButton(ns(paste0("del_", i)), NULL, icon("trash"), 
                             class="btn-outline-danger btn-sm border-0",
                             onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("del_rx_idx"), i))
            )
        )
      })
    })
    
    # --- RHandsontable Logic ---
    output$rx_hot <- renderRHandsontable({
      df_to_show <- rx_meds$df
      req(nrow(df_to_show) >= 0)
      
      rhandsontable(df_to_show, 
                    stretchH = "all", 
                    rowHeaders = FALSE, 
                    width = "100%", 
                    manualColumnResize = TRUE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    # --- Event Handlers ---
    observeEvent(input$add_rx_id, {
      new_d <- rx_master() %>% filter(id == input$add_rx_id) %>% 
        select(brand_name, generic, dose, freq, route, duration)
      rx_meds$df <- rbind(rx_meds$df, new_d)
      updateTextInput(session, "rx_q", value = "")
    })
    
    observeEvent(input$del_rx_idx, {
      rx_meds$df <- rx_meds$df[-as.numeric(input$del_rx_idx), ]
    })
    
    refresh_val <- reactiveVal(0)
    
    observeEvent(input$save_rx, {
      req(current_pt(), user_info())
      refresh_val(refresh_val() + 1)
      
      if (!is.null(input$rx_hot)) {
        rx_meds$df <- rhandsontable::hot_to_r(input$rx_hot)
      }
      
      pt_id <- as.character(current_pt()$id)
      # Ensure JSON is a single string for the SQL query
      json_data <- as.character(jsonlite::toJSON(rx_meds$df, auto_unbox = TRUE))
      
      tryCatch({
        dbExecute(pool, glue::glue_sql("
      INSERT INTO prescriptions (patient_id, meds_json, created_by, created_at, visit_date)
      VALUES ({pt_id}, {json_data}, {user_info()$username}, NOW(), CURRENT_DATE)
      ON CONFLICT (patient_id, visit_date) 
      DO UPDATE SET 
        meds_json = EXCLUDED.meds_json,
        created_by = EXCLUDED.created_by,
        created_at = NOW()
    ", .con = pool))
        
        # --- ADDED: Trigger the timeline refresh ---
        if (exists("timeline_trigger") && is.reactiveVal(timeline_trigger)) {
          timeline_trigger(timeline_trigger() + 1)
        }
        
        rx_meta$is_history <- TRUE
        rx_meta$history_date <- as.character(Sys.Date())
        showNotification("Prescription Saved Successfully.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Save Failed:", e$message), type = "error")
      })
    })
    
    # --- Register New Drug Modal ---
    observeEvent(input$new_drug_modal, {
      showModal(modalDialog(
        title = "Register New Medication",
        textInput(ns("n_brand"), "Brand Name", value = input$rx_q),
        textInput(ns("n_generic"), "Generic Name"),
        fluidRow(
          column(6, textInput(ns("n_dose"), "Dose")),
          column(6, textInput(ns("n_freq"), "Frequency"))
        ),
        textInput(ns("n_route"), "Route", value = "Oral"),
        textInput(ns("n_dur"), "Duration"),
        footer = tagList(
          modalButton("Cancel"), 
          actionButton(ns("save_new_drug_lib"), "Save to Library", class="btn-primary")
        )
      ))
    })
    
    observeEvent(input$save_new_drug_lib, {
      req(input$n_brand, pool)
      dbExecute(pool, 
                "INSERT INTO drug_master (brand_name, generic, dose, freq, route, duration) 
                 VALUES ($1, $2, $3, $4, $5, $6)",
                list(input$n_brand, input$n_generic, input$n_dose, input$n_freq, input$n_route, input$n_dur))
      load_rx_master()
      removeModal()
      showNotification("Drug Added to Library.")
    })
  })
}