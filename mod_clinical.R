library(shiny)
library(bslib)
library(rhandsontable)
library(shinyjs)

clinical_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Essential for the locking/disabling functionality
    useShinyjs(),
    
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
              div(class="d-flex align-items-center gap-2",
                  # The Edit Button only appears when a record is locked
                  uiOutput(ns("edit_button_ui")),
                  uiOutput(ns("pid_badge_ui"))
              )
          )
        ),
        
        card_body(
          # Message that appears when fields are locked
          uiOutput(ns("lock_message")),
          
          # Container for all clinical inputs
          div(id = ns("clinical_input_form"),
              
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
                  div(id = ns("pmhx_controls"), class = "mt-2 d-flex gap-2",
                      actionButton(ns("add_past_medical_history_row"), "Add Row", class="btn-sm btn-outline-secondary"),
                      actionButton(ns("del_past_medical_history_row"), "Del Row", class="btn-sm btn-outline-danger")
                  )
                )
              )
          )
        ),
        
        card_footer(
          # This button will be hidden via shinyjs when the record is locked
          actionButton(ns("save_note"), "Save Visit Record", class="btn-primary btn-lg w-100")
        )
      )
    )
  )
}

clinical_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_visits <- reactiveVal(0)
    
    # --- Local Module State ---
    note_state <- reactiveValues(
      active_visit_id = NULL,
      active_visit_date = NULL
    )
    pmh_data <- reactiveVal(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))
    
    # Inside clinical_server
    is_locked <- reactiveVal(FALSE)
    
    # Helper to toggle UI state
    observe({
      if (is_locked()) {
        shinyjs::disable("v_bp")
        shinyjs::disable("v_hr")
        shinyjs::disable("v_weight")
        shinyjs::disable("v_temp")
        shinyjs::disable("v_followup")
        shinyjs::disable("clinic_notes")
      #  shinyjs::disable("add_past_medical_history_row")
       # shinyjs::disable("del_past_medical_history_row")
        shinyjs::hide("save_note")
      } else {
        shinyjs::enable("v_bp")
        shinyjs::enable("v_hr")
        shinyjs::enable("v_weight")
        shinyjs::enable("v_temp")
        shinyjs::enable("v_followup")
        shinyjs::enable("clinic_notes")
       # shinyjs::enable("add_past_medical_history_row")
       # shinyjs::enable("del_past_medical_history_row")
        shinyjs::show("save_note")
      }
    })
    
    # Display message when locked
    output$lock_message <- renderUI({
      req(is_locked())
      div(class="alert alert-info py-2", icon("info-circle"), 
          " This record is finalized. Click 'Edit Record' in the header to make changes.")
    })
    
    # Edit Button UI
    output$edit_button_ui <- renderUI({
      req(note_state$active_visit_id, is_locked())
      actionButton(ns("unlock_btn"), "Edit Record", class="btn-sm btn-warning me-2", icon = icon("edit"))
    })
    
    # Unlock Handler
    observeEvent(input$unlock_btn, { is_locked(FALSE) })
    
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
      refresh_visits() 
      
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
    # Start New Empty Note
    observeEvent(input$new_visit_btn, {
      # 1. Clear all inputs (BP, HR, Notes, PMHx table, etc.)
      reset_clinical_ui()
      
      # 2. UNLOCK THE RECORD
      # This enables all inputs and shows the 'Save' button via the reactive observer
      is_locked(FALSE)
      
      # 3. Reset the state to "New Note" mode
      note_state$active_visit_id <- NULL
      note_state$active_visit_date <- format(Sys.Date(), "%d %b %Y")
      
      showNotification("Ready for new entry. Fields unlocked.", type = "message")
    })
    
    # Load Existing Visit
    # Load Existing Visit
    # Load Existing Visit
    observeEvent(input$select_visit, {
      vid <- input$select_visit
      req(vid)
      
      # 1. Fetch record from DB
      res <- dbGetQuery(pool, 
                        "SELECT visit_date, visit_json, patient_id FROM visitsmodule WHERE id = $1::int", 
                        list(as.integer(vid)))
      req(nrow(res) > 0)
      
      # 2. Update local state
      note_state$active_visit_id <- vid
      note_state$active_visit_date <- format(as.Date(res$visit_date[1]), "%d %b %Y")
      
      # 3. Parse JSON data
      data <- tryCatch({ 
        val <- jsonlite::fromJSON(res$visit_json[1]) 
        if(is.null(val)) list() else val
      }, error = function(e) list())
      
      # 4. Update UI Inputs
      updateTextInput(session, "v_bp", value = data$vitals$bp %||% "")
      updateNumericInput(session, "v_hr", value = data$vitals$hr %||% NA)
      updateNumericInput(session, "v_weight", value = data$vitals$weight %||% NA)
      updateTextInput(session, "v_temp", value = data$vitals$temp %||% "")
      updateTextAreaInput(session, "clinic_notes", value = data$clinic_notes %||% "")
      
      # Handle Follow-up Date
      f_date <- data$followup_date
      if(!is.null(f_date) && !is.na(f_date) && is.character(f_date) && nzchar(f_date)) {
        updateDateInput(session, "v_followup", value = as.Date(f_date))
      } else {
        updateDateInput(session, "v_followup", value = NA)
      }
      
      # 5. Load PMHx for this patient (Syncing the table with the record)
      pt_id <- as.character(res$patient_id[1])
      pmh_res <- tryCatch({
        dbGetQuery(pool, 
                   "SELECT condition_text, onset_date FROM past_medical_history WHERE registration_id::text = $1", 
                   list(pt_id))
      }, error = function(e) NULL)
      
      if(!is.null(pmh_res) && nrow(pmh_res) > 0) {
        colnames(pmh_res) <- c("Condition", "Year")
        pmh_data(pmh_res)
      } else {
        pmh_data(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))
      }
      
      # 6. LOCK THE RECORD
      # This triggers the shinyjs disabling and shows the 'Edit Record' button
      is_locked(TRUE)
      
      showNotification(paste("Loaded record for", note_state$active_visit_date), type = "message")
    })
    
    # SAVE VISIT RECORD
    # SAVE VISIT RECORD
    observeEvent(input$save_note, {
      req(current_pt(), user_info())
      pt_id <- as.integer(current_pt()$id)
      
      # Check if a record already exists for TODAY for this patient
      # (Only check if we aren't already editing a specific record)
      existing_record <- dbGetQuery(pool, 
                                    "SELECT id FROM visitsmodule WHERE patient_id::text = $1 AND visit_date = CURRENT_DATE", 
                                    list(as.character(pt_id)))
      
      if (is.null(note_state$active_visit_id) && nrow(existing_record) > 0) {
        # Conflict found: A record exists for today, but user clicked 'Save' on a 'New Note'
        showModal(modalDialog(
          title = "Duplicate Visit Date",
          span("A visit record already exists for today. Would you like to update the existing record or create a second entry for today?"),
          footer = tagList(
            actionButton(ns("save_as_new"), "Save as New Entry", class = "btn-info"),
            actionButton(ns("save_overwrite"), "Update Existing Record", class = "btn-warning"),
            modalButton("Cancel")
          )
        ))
      } else {
        # No conflict or already in "Edit Mode": Proceed with standard save
        execute_save_logic()
      }
    })
    
    # Helper function to encapsulate the actual DB writing
    # ---------------------------------------------------------
    # INTERNAL HELPER: Execute Save Logic
    # ---------------------------------------------------------
    # ---------------------------------------------------------
    # INTERNAL HELPER: Execute Save Logic
    # ---------------------------------------------------------
    execute_save_logic <- function(force_new = FALSE) {
      req(current_pt(), user_info())
      
      curr_user <- user_info()$username
      pt_id     <- as.integer(current_pt()$id)
      
      # 1. Prepare Data Payload
      payload <- list(
        vitals = list(
          bp     = input$v_bp, 
          hr     = input$v_hr, 
          weight = input$v_weight, 
          temp   = input$v_temp
        ),
        clinic_notes  = input$clinic_notes,
        followup_date = as.character(input$v_followup)
      )
      json_data <- jsonlite::toJSON(payload, auto_unbox = TRUE)
      
      tryCatch({
        # 2. Database Transaction
        poolWithTransaction(pool, function(con) {
          
          # Decide between INSERT (New) or UPDATE (Existing)
          if (is.null(note_state$active_visit_id) || force_new) {
            
            # --- ACTION: INSERT ---
            new_id <- DBI::dbGetQuery(con, glue::glue_sql("
              INSERT INTO visitsmodule (patient_id, visit_date, visit_json, created_by, updated_at) 
              VALUES ({pt_id}, CURRENT_DATE, {json_data}, {curr_user}, NOW()) 
              RETURNING id", .con = con))$id
            
            # Update local state so we know we are now viewing a specific record
            note_state$active_visit_id   <- new_id
            note_state$active_visit_date <- format(Sys.Date(), "%d %b %Y")
            
          } else {
            
            # --- ACTION: UPDATE ---
            DBI::dbExecute(con, glue::glue_sql("
              UPDATE visitsmodule 
              SET visit_json = {json_data}, 
                  updated_by = {curr_user}, 
                  updated_at = NOW() 
              WHERE id = {note_state$active_visit_id}::int", .con = con))
          }
          
          # --- HANDLE PMHx TABLE ---
          if (!is.null(input$past_medical_history_table)) {
            final_pmhx <- hot_to_r(input$past_medical_history_table)
            
            # Wipe existing PMHx for this patient to prevent duplicates
            DBI::dbExecute(con, "DELETE FROM past_medical_history WHERE registration_id = $1", list(pt_id))
            
            # Filter rows to avoid saving empty entries
            final_pmhx <- final_pmhx[nzchar(trimws(as.character(final_pmhx$Condition))), ]
            
            if(nrow(final_pmhx) > 0) {
              for(i in 1:nrow(final_pmhx)) {
                DBI::dbExecute(con, "
                  INSERT INTO past_medical_history (registration_id, condition_text, onset_date, created_by) 
                  VALUES ($1, $2, $3, $4)",
                               list(
                                 pt_id, 
                                 as.character(final_pmhx$Condition[i]), 
                                 as.character(final_pmhx$Year[i]), 
                                 curr_user
                               )
                )
              }
            }
          }
        })
        
        # 3. Post-Save UI State Synchronization
        removeModal()           # Close conflict modal if it was open
        is_locked(TRUE)         # Disable input fields
        
        # This line forces the sidebar renderUI to re-run and show the new/updated date
        refresh_visits(refresh_visits() + 1) 
        
        showNotification("Visit Record Saved and Finalized.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Save Error:", e$message), type = "error")
      })
    }
    
    # Listeners for the Modal Buttons
    observeEvent(input$save_as_new, { execute_save_logic(force_new = TRUE) })
    observeEvent(input$save_overwrite, { 
      # Find the ID for today's record to ensure we update the right one
      existing_id <- dbGetQuery(pool, "SELECT id FROM visitsmodule WHERE patient_id::text = $1 AND visit_date = CURRENT_DATE", 
                                list(as.character(current_pt()$id)))$id[1]
      note_state$active_visit_id <- existing_id
      execute_save_logic(force_new = FALSE) 
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
      
      refresh_visits(refresh_visits() + 1) # Side panel removes the deleted date immediately
      reset_clinical_ui()
      removeModal()
      showNotification("Deleted.", type = "warning")
    })
  })
}