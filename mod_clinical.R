clinical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      sidebar = sidebar(
        title = "Visit History",
        width = 300,
        
        # Action button to clear fields and start fresh
        actionButton(ns("new_visit_btn"), "Start New Empty Note", 
                     class="btn-success w-100 mb-2", 
                     icon = icon("plus")),
        
        # Delete button appears only when a visit is selected
        uiOutput(ns("delete_visit_ui")),
        
        hr(),
        
        # Scrollable container for the past visit tiles
        uiOutput(ns("visit_tiles_ui"))
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          div(class="d-flex justify-content-between align-items-center",
              textOutput(ns("note_header")),
              uiOutput(ns("pid_badge_ui")))
        ),
        
        card_body(
          # Vitals Section
          div(class = "vitals-container mb-4", 
              layout_column_wrap(
                width = 1/5,
                textInput(ns("v_bp"), "BP (mmHg)", placeholder = "120/80"),
                numericInput(ns("v_hr"), "Pulse (bpm)", value = NA),
                numericInput(ns("v_weight"), "Weight (kg)", value = NA),
                textInput(ns("v_temp"), "Temp (F)", placeholder = "98.4"),
                dateInput(ns("v_followup"), "Follow-up Date", value = NA)
              )
          ),
          
          tags$hr(style = "clear: both; margin: 20px 0; border-top: 2px solid #eee;"),
          
          # Notes and PMHx Section
          layout_column_wrap(
            width = NULL,
            style = htmltools::css(grid_template_columns = "1fr 1fr"), 
            
            # Left Side: Clinical Notes
            div(style = "display: flex; flex-direction: column;",
                textAreaInput(ns("clinic_notes"), "Examination & Plan", 
                              rows = 15, width = "100%", 
                              placeholder = "Enter findings, assessment, and treatment plan...")
            ),
            
            # Right Side: PMHx Table
            div(
              tags$label("Past Medical History", style="font-weight: bold;"),
              rHandsontableOutput(ns("past_medical_history_table")),
              div(class = "mt-2 d-flex gap-2",
                  actionButton(ns("add_past_medical_history_row"), "Add Row", class="btn-sm btn-outline-secondary"),
                  actionButton(ns("del_past_medical_history_row"), "Del Row", class="btn-sm btn-outline-danger")
              )
            )
          )
        ),
        
        card_footer(
          actionButton(ns("save_note"), "Save Visit Record", class="btn-primary btn-lg w-100")
        )
      )
    )
  )
}

clinical_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Local Module State ---
    note_state <- reactiveValues(
      active_visit_id = NULL,
      active_visit_date = NULL
    )
    pmh_data <- reactiveVal(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))
    
    # --- INTERNAL HELPER: RESET UI ---
    reset_clinical_ui <- function() {
      note_state$active_visit_id <- NULL
      note_state$active_visit_date <- as.character(Sys.Date())
      
      updateTextInput(session, "v_bp", value = "")
      updateNumericInput(session, "v_hr", value = NA)
      updateNumericInput(session, "v_weight", value = NA)
      updateTextInput(session, "v_temp", value = "")
      updateTextAreaInput(session, "clinic_notes", value = "")
      updateDateInput(session, "v_followup", value = NA)
      
      # Reset PMH to a clean template
      pmh_data(data.frame(Condition = "", Year = "", stringsAsFactors = FALSE))
    }
    
    # --- UI OUTPUTS ---
    output$pid_badge_ui <- renderUI({
      req(current_pt())
      span(class="badge bg-secondary", paste("PID:", current_pt()$id))
    })
    
    output$note_header <- renderText({
      if (is.null(note_state$active_visit_id)) {
        paste("New Clinical Note -", format(Sys.Date(), "%d %b %Y"))
      } else {
        paste("Viewing Record:", note_state$active_visit_date)
      }
    })
    
    output$visit_tiles_ui <- renderUI({
      req(current_pt())
      df <- dbGetQuery(pool, 
                       "SELECT id, visit_date FROM visitsmodule WHERE patient_id::text = $1 ORDER BY visit_date DESC", 
                       list(as.character(current_pt()$id)))
      
      if(nrow(df) == 0) return(p("No previous visits.", class="text-muted p-3 text-center"))
      
      tagList(
        lapply(1:nrow(df), function(i) {
          visit_id <- df$id[i]
          v_date <- as.Date(df$visit_date[i])
          is_active <- identical(as.integer(visit_id), as.integer(note_state$active_visit_id))
          
          div(style = "margin-bottom: 8px;",
              actionButton(
                inputId = ns(paste0("tile_trigger_", visit_id)),
                label = tagList(
                  div(style="display: flex; justify-content: space-between; width: 100%;",
                      span(icon("calendar-day"), style="margin-right: 10px;"),
                      span(format(v_date, "%d %b %Y"), style="font-weight: 600; flex-grow: 1;"),
                      if(is_active) icon("chevron-right") else ""
                  )
                ),
                class = if(is_active) "btn btn-primary w-100 shadow-sm" else "btn btn-outline-dark w-100 text-start",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("select_visit"), visit_id)
              )
          )
        })
      )
    })
    
    output$delete_visit_ui <- renderUI({
      req(note_state$active_visit_id)
      actionButton(ns("confirm_delete_btn"), "Delete Selected Visit", class="btn-outline-danger w-100 btn-sm")
    })
    
    # --- EVENT HANDLERS ---
    
    # Triggered when global patient selection changes
    observeEvent(current_pt(), {
      reset_clinical_ui() # Clear everything first
      req(current_pt())
      
      # Load PMH for the specific patient
      pt_id <- as.character(current_pt()$id)
      res <- tryCatch({
        dbGetQuery(pool, "SELECT condition_text, onset_date FROM past_medical_history WHERE registration_id::text = $1", list(pt_id))
      }, error = function(e) NULL)
      
      if(!is.null(res) && nrow(res) > 0) {
        colnames(res) <- c("Condition", "Year")
        pmh_data(res)
      }
    }, ignoreNULL = FALSE)
    
    # Start New Empty Note
    observeEvent(input$new_visit_btn, {
      reset_clinical_ui()
      showNotification("Ready for new entry.", type = "message")
    })
    
    # Load Existing Visit
    # Load Existing Visit
    observeEvent(input$select_visit, {
      vid <- input$select_visit
      req(vid)
      
      res <- dbGetQuery(pool, "SELECT visit_date, visit_json FROM visitsmodule WHERE id = $1::int", list(as.integer(vid)))
      req(nrow(res) > 0)
      
      note_state$active_visit_id <- vid
      note_state$active_visit_date <- format(as.Date(res$visit_date[1]), "%d %b %Y")
      
      # Use an empty list as fallback if JSON is empty/null
      data <- tryCatch({ 
        val <- jsonlite::fromJSON(res$visit_json[1]) 
        if(is.null(val)) list() else val
      }, error = function(e) list())
      
      # Update UI Inputs with null-coalescing
      updateTextInput(session, "v_bp", value = data$vitals$bp %||% "")
      updateNumericInput(session, "v_hr", value = data$vitals$hr %||% NA)
      updateNumericInput(session, "v_weight", value = data$vitals$weight %||% NA)
      updateTextInput(session, "v_temp", value = data$vitals$temp %||% "")
      updateTextAreaInput(session, "clinic_notes", value = data$clinic_notes %||% "")
      
      # FIXED: Robust check for follow-up date
      f_date <- data$followup_date
      if(!is.null(f_date) && !is.na(f_date) && is.character(f_date) && nzchar(f_date)) {
        updateDateInput(session, "v_followup", value = as.Date(f_date))
      } else {
        updateDateInput(session, "v_followup", value = NA)
      }
    })
    
    # SAVE VISIT RECORD
    observeEvent(input$save_note, {
      req(current_pt(), user_info())
      curr_user <- user_info()$username
      pt_id <- as.integer(current_pt()$id)
      
      payload <- list(
        vitals = list(bp = input$v_bp, hr = input$v_hr, weight = input$v_weight, temp = input$v_temp),
        clinic_notes = input$clinic_notes,
        followup_date = as.character(input$v_followup)
      )
      json_data <- jsonlite::toJSON(payload, auto_unbox = TRUE)
      
      tryCatch({
        poolWithTransaction(pool, function(con) {
          if (is.null(note_state$active_visit_id)) {
            # INSERT NEW
            new_id <- DBI::dbGetQuery(con, glue::glue_sql("
              INSERT INTO visitsmodule (patient_id, visit_date, visit_json, created_by, updated_at) 
              VALUES ({pt_id}, CURRENT_DATE, {json_data}, {curr_user}, NOW()) 
              RETURNING id", .con = con))$id
            note_state$active_visit_id <- new_id
          } else {
            # UPDATE EXISTING
            DBI::dbExecute(con, glue::glue_sql("
              UPDATE visitsmodule SET visit_json = {json_data}, updated_by = {curr_user}, updated_at = NOW() 
              WHERE id = {note_state$active_visit_id}::int", .con = con))
          }
          
          # Handle PMHx Table Save
          if (!is.null(input$past_medical_history_table)) {
            final_pmhx <- hot_to_r(input$past_medical_history_table)
            DBI::dbExecute(con, "DELETE FROM past_medical_history WHERE registration_id = $1", list(pt_id))
            final_pmhx <- final_pmhx[nzchar(trimws(as.character(final_pmhx$Condition))), ]
            if(nrow(final_pmhx) > 0) {
              for(i in 1:nrow(final_pmhx)) {
                DBI::dbExecute(con, "INSERT INTO past_medical_history (registration_id, condition_text, onset_date, created_by) VALUES ($1, $2, $3, $4)",
                               list(pt_id, as.character(final_pmhx$Condition[i]), as.character(final_pmhx$Year[i]), curr_user))
              }
            }
          }
        })
        showNotification("Visit Saved Successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Save Error:", e$message), type = "error")
      })
    })
    
    # PMHx HANDSONTABLE
    output$past_medical_history_table <- renderRHandsontable({
      df <- pmh_data()
      req(df)
      rhandsontable(df, stretchH = "all", height = 300) %>%
        hot_col("Condition", type = "text") %>%
        hot_col("Year", type = "text")
    })
    
    observeEvent(input$add_past_medical_history_row, {
      df <- if(!is.null(input$past_medical_history_table)) hot_to_r(input$past_medical_history_table) else pmh_data()
      pmh_data(rbind(df, data.frame(Condition = "", Year = "")))
    })
    
    observeEvent(input$del_past_medical_history_row, {
      df <- if(!is.null(input$past_medical_history_table)) hot_to_r(input$past_medical_history_table) else pmh_data()
      sel <- input$past_medical_history_table_select
      if(!is.null(sel$r) && nrow(df) > 0) {
        pmh_data(df[-sel$r, ])
      }
    })
    
    # DELETION LOGIC
    observeEvent(input$confirm_delete_btn, {
      showModal(modalDialog(title = "Delete Visit", "Permanently delete this record?", 
                            footer = tagList(modalButton("Cancel"), actionButton(ns("execute_delete"), "Delete", class="btn-danger"))))
    })
    
    observeEvent(input$execute_delete, {
      dbExecute(pool, "DELETE FROM visitsmodule WHERE id = $1::int", list(as.integer(note_state$active_visit_id)))
      reset_clinical_ui()
      removeModal()
      showNotification("Deleted.", type = "warning")
    })
  })
}