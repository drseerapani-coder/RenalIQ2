library(shiny)
library(bslib)
library(rhandsontable)
library(shinyjs)

clinical_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        /* Compact Vitals & Followup Cards */
        .vitals-card { border-left: 4px solid #007bff; background-color: #f8f9fa; padding: 10px !important; margin-bottom: 8px !important; }
        .followup-card { border-left: 4px solid #6f42c1; background-color: #f3f0ff; padding: 8px 12px !important; margin-bottom: 10px !important; }
        
        /* Compact Radio Buttons */
        .radio-group-container .shiny-options-group { 
            display: flex; flex-wrap: wrap; gap: 4px; background: white; 
            padding: 4px 8px !important; border-radius: 6px; border: 1px solid #dee2e6;
        }
        .radio-group-container label { 
            font-weight: 600; font-size: 0.75rem; cursor: pointer; 
            padding: 2px 6px; border-radius: 4px; margin-bottom: 0px !important;
        }
        
        /* Layout Adjustments */
        .card-header { background-color: white !important; border-bottom: 1px solid #eee; padding: 10px 15px !important; }
        .badge-pid { font-size: 0.85rem; padding: 0.4em 0.6em; }
        .handsontable { border-radius: 6px; overflow: hidden; font-size: 13px; }
        
        /* Compact Form Elements */
        .form-group { margin-bottom: 0px !important; }
        .control-label { font-size: 0.8rem; font-weight: bold; margin-bottom: 2px !important; }
        .form-control { padding: 4px 8px; height: auto; }
        
        /* Visit Tile Styling */
        .visit-tile { font-size: 0.9rem; cursor: pointer; transition: 0.2s; }
        .visit-tile:hover { background-color: #f0f4f8; }
      ")))
    ),
    
    page_sidebar(
      sidebar = sidebar(
        title = div(icon("hospital-user"), "Clinical Portal"),
        width = 280,
        actionButton(ns("new_visit_btn"), "New Empty Note", 
                     class="btn-success w-100 mb-2", 
                     icon = icon("plus")),
        uiOutput(ns("delete_visit_ui")),
        hr(style="margin: 10px 0;"),
        tags$label("Visit History", style="font-weight: bold; color: #666; font-size: 0.8rem; margin-left: 5px;"),
        uiOutput(ns("visit_tiles_ui"))
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          div(class="d-flex justify-content-between align-items-center",
              div(style="font-size: 1.1rem; font-weight: 600; color: #2c3e50;",
                  textOutput(ns("note_header"), inline = TRUE)
              ),
              div(class="d-flex align-items-center gap-2",
                  # Header Action Area: Edit and Save buttons live here now
                  uiOutput(ns("edit_button_ui")),
                  uiOutput(ns("save_button_header_ui")),
                  uiOutput(ns("pid_badge_ui"))
              )
          )
        ),
        
        card_body(
          uiOutput(ns("lock_message")),
          
          div(id = ns("clinical_input_form"),
              
              # ROW 1: COMPACT VITALS
              div(class = "vitals-card rounded-3", 
                  layout_column_wrap(
                    width = 1/4, gap = "10px",
                    textInput(ns("v_bp"), div(icon("heart-pulse"), " BP"), placeholder = "120/80"),
                    numericInput(ns("v_hr"), div(icon("gauge-high"), " HR"), value = NA),
                    numericInput(ns("v_weight"), div(icon("weight-scale"), " Wt (kg)"), value = NA),
                    textInput(ns("v_temp"), div(icon("temperature-half"), " Temp"), placeholder = "98.4")
                  )
              ),
              
              # ROW 2: TIGHT FOLLOW-UP SECTION
              div(class = "followup-card rounded-3",
                  div(class="d-flex flex-row gap-3 align-items-center",
                      div(class="radio-group-container", style="flex-grow: 1;",
                          tags$label(icon("clock"), " Quick Interval (Skips Sundays)", 
                                     style="font-weight: bold; font-size: 0.8rem; color: #6f42c1; display: block;"),
                          radioButtons(ns("quick_interval"), NULL,
                                       choices = c("10D" = "10", "1W" = "7", "2W" = "14", "3W" = "21", 
                                                   "1M" = "30", "6W" = "42", "3M" = "90", "4M" = "120", 
                                                   "6M" = "180", "1Y" = "365"),
                                       inline = TRUE, selected = character(0))
                      ),
                      div(style="min-width: 160px;",
                          tags$label(icon("calendar-day"), " Follow-up Date *", 
                                     style="font-weight: bold; font-size: 0.8rem; display: block;"),
                          dateInput(ns("v_followup"), label = NULL, value = NA, width = "100%")
                      )
                  )
              ),
              
              # ROW 3: NOTES AND GLOBAL PMHx
              layout_column_wrap(
                width = NULL,
                style = htmltools::css(grid_template_columns = "1.2fr 0.8fr"), 
                gap = "15px",
                
                # Left Side: Examination & Plan
                div(class="d-flex flex-column",
                    tags$label(icon("file-lines"), " Examination & Plan", 
                               style="font-weight: bold; font-size: 0.85rem; margin-bottom: 5px;"),
                    textAreaInput(ns("clinic_notes"), NULL, 
                                  rows = 14, width = "100%", 
                                  placeholder = "Findings, assessment, plan...")
                ),
                
                # Right Side: Global PMHx (Decoupled from visits)
                div(
                  div(class="d-flex justify-content-between align-items-center mb-1",
                      tags$label(icon("history"), " Past Medical History", 
                                 style="font-weight: bold; font-size: 0.85rem;"),
                      div(id = ns("pmhx_controls"), class = "d-flex gap-1",
                          actionButton(ns("add_past_medical_history_row"), "", icon = icon("plus"), class="btn-xs btn-outline-primary"),
                          actionButton(ns("del_past_medical_history_row"), "", icon = icon("trash"), class="btn-xs btn-outline-danger")
                      )
                  ),
                  rHandsontableOutput(ns("past_medical_history_table"))
                )
              )
          )
        )
      )
    )
  )
}

clinical_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive States ---
    refresh_visits <- reactiveVal(0)
    is_locked <- reactiveVal(FALSE)
    note_state <- reactiveValues(
      active_visit_id = NULL,
      active_visit_date = NULL
    )
    
    # PMH is global to the patient - loaded once per patient selection
    pmh_data <- reactiveVal(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))
    
    # Render the Save button in the header only when unlocked
    output$save_button_header_ui <- renderUI({
      if (!is_locked()) {
        actionButton(ns("save_note"), "Save Visit Record", 
                     class = "btn-sm btn-primary shadow-sm", 
                     icon = icon("floppy-disk"))
      }
    })
    
    # --- 1. RESET UI HELPER ---
    reset_clinical_ui <- function() {
      note_state$active_visit_id <- NULL
      note_state$active_visit_date <- format(Sys.Date(), "%d %b %Y")
      
      updateTextInput(session, "v_bp", value = "")
      updateNumericInput(session, "v_hr", value = NA)
      updateNumericInput(session, "v_weight", value = NA)
      updateTextInput(session, "v_temp", value = "")
      updateTextAreaInput(session, "clinic_notes", value = "")
      updateDateInput(session, "v_followup", value = NA)
      updateRadioButtons(session, "quick_interval", selected = character(0))
      # Note: pmh_data is NOT reset here to keep it persistent for the patient
    }
    
    # --- 2. GLOBAL PATIENT OBSERVER (PMHx Loading) ---
    observeEvent(current_pt(), {
      req(current_pt())
      reset_clinical_ui()
      
      # Fetch PMH for the patient (Global record)
      pt_id <- as.character(current_pt()$id)
      res <- tryCatch({
        dbGetQuery(pool, "SELECT condition_text, onset_date FROM past_medical_history WHERE registration_id::text = $1", list(pt_id))
      }, error = function(e) {
        message("PMHx Load Error: ", e$message)
        NULL
      })
      
      if(!is.null(res) && nrow(res) > 0) {
        colnames(res) <- c("Condition", "Year")
        pmh_data(res)
      } else {
        pmh_data(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))
      }
    }, ignoreNULL = FALSE)
    
    # --- 3. UI LOCKING CONTROL ---
    observe({
      toggle_state <- !is_locked()
      fields <- c("v_bp", "v_hr", "v_weight", "v_temp", "v_followup", "clinic_notes", "quick_interval")
      
      for (field in fields) {
        shinyjs::toggleState(field, condition = toggle_state)
      }
      
      shinyjs::toggle("save_note", condition = toggle_state)
      shinyjs::toggle("pmhx_controls", condition = toggle_state)
    })
    
    # --- 4. FOLLOW-UP INTERVAL LOGIC (Skip Sundays) ---
    observeEvent(input$quick_interval, {
      req(input$quick_interval)
      days_to_add <- as.numeric(input$quick_interval)
      target_date <- Sys.Date() + days_to_add
      
      if (format(target_date, "%w") == "0") {
        target_date <- target_date + 1 
        showNotification("Date adjusted to Monday to avoid Sunday.", type = "message")
      }
      updateDateInput(session, "v_followup", value = target_date)
    })
    
    # --- 5. VISIT HISTORY SIDEBAR ---
    output$visit_tiles_ui <- renderUI({
      refresh_visits() 
      req(current_pt())
      
      visits <- dbGetQuery(pool, 
                           "SELECT id, visit_date FROM visitsmodule WHERE patient_id = $1 ORDER BY visit_date DESC", 
                           list(as.integer(current_pt()$id)))
      
      if(nrow(visits) == 0) return(p("No past visits found.", class="text-muted ps-2 mt-2"))
      
      lapply(1:nrow(visits), function(i) {
        div(class="mb-2",
            actionButton(ns(paste0("load_visit_", visits$id[i])), 
                         label = div(class="d-flex justify-content-between w-100",
                                     span(icon("calendar-day"), " ", format(as.Date(visits$visit_date[i]), "%d %b %Y"))),
                         class="btn btn-outline-secondary w-100 text-start shadow-sm py-2")
        )
      })
    })
    
    # Dynamic Observer for Sidebar Links
    observe({
      req(current_pt())
      visits <- dbGetQuery(pool, "SELECT id FROM visitsmodule WHERE patient_id = $1", list(as.integer(current_pt()$id)))
      for (vid in visits$id) {
        local({
          this_vid <- vid
          observeEvent(input[[paste0("load_visit_", this_vid)]], {
            res <- dbGetQuery(pool, "SELECT visit_date, visit_json FROM visitsmodule WHERE id = $1", list(as.integer(this_vid)))
            req(nrow(res) > 0)
            
            note_state$active_visit_id <- this_vid
            note_state$active_visit_date <- format(as.Date(res$visit_date[1]), "%d %b %Y")
            
            data <- jsonlite::fromJSON(res$visit_json[1])
            
            # Populate Vitals and Notes
            updateTextInput(session, "v_bp", value = data$vitals$bp %||% "")
            updateNumericInput(session, "v_hr", value = data$vitals$hr %||% NA)
            updateNumericInput(session, "v_weight", value = data$vitals$weight %||% NA)
            updateTextInput(session, "v_temp", value = data$vitals$temp %||% "")
            updateTextAreaInput(session, "clinic_notes", value = data$clinic_notes %||% "")
            
            # Handle Follow-up Date (Safety check for nulls)
            f_date <- data$followup_date
            if(!is.null(f_date) && nzchar(f_date)) {
              updateDateInput(session, "v_followup", value = as.Date(f_date))
            } else {
              updateDateInput(session, "v_followup", value = NA)
            }
            
            is_locked(TRUE)
          })
        })
      }
    })
    
    # --- 6. SAVE LOGIC (Consolidated) ---
    execute_save_logic <- function(force_new = FALSE) {
      req(current_pt(), user_info())
      curr_user <- user_info()$username
      pt_id     <- as.integer(current_pt()$id)
      
      payload <- list(
        vitals = list(bp = input$v_bp, hr = input$v_hr, weight = input$v_weight, temp = input$v_temp),
        clinic_notes  = input$clinic_notes,
        followup_date = as.character(input$v_followup)
      )
      json_data <- jsonlite::toJSON(payload, auto_unbox = TRUE)
      
      tryCatch({
        poolWithTransaction(pool, function(con) {
          # A. Save Visit Data
          if (is.null(note_state$active_visit_id) || force_new) {
            new_id <- DBI::dbGetQuery(con, glue::glue_sql("
              INSERT INTO visitsmodule (patient_id, visit_date, visit_json, created_by, updated_at) 
              VALUES ({pt_id}, CURRENT_DATE, {json_data}, {curr_user}, NOW()) 
              RETURNING id", .con = con))$id
            note_state$active_visit_id <- new_id
          } else {
            DBI::dbExecute(con, glue::glue_sql("
              UPDATE visitsmodule SET visit_json = {json_data}, updated_by = {curr_user}, updated_at = NOW() 
              WHERE id = {note_state$active_visit_id}::int", .con = con))
          }
          
          # B. Save PMHx Table (Global Update)
          if (!is.null(input$past_medical_history_table)) {
            final_pmhx <- hot_to_r(input$past_medical_history_table)
            final_pmhx <- final_pmhx[nzchar(trimws(as.character(final_pmhx$Condition))), ]
            
            DBI::dbExecute(con, "DELETE FROM past_medical_history WHERE registration_id = $1", list(pt_id))
            
            if(nrow(final_pmhx) > 0) {
              for(i in 1:nrow(final_pmhx)) {
                DBI::dbExecute(con, "INSERT INTO past_medical_history (registration_id, condition_text, onset_date, created_by) VALUES ($1, $2, $3, $4)",
                               list(pt_id, as.character(final_pmhx$Condition[i]), as.character(final_pmhx$Year[i]), curr_user))
              }
            }
            pmh_data(final_pmhx) # Sync local reactive
          }
        })
        removeModal()
        is_locked(TRUE)
        refresh_visits(refresh_visits() + 1)
        showNotification("Record and Global PMHx Saved.", type = "message")
      }, error = function(e) { showNotification(paste("Save Error:", e$message), type = "error") })
    }
    
    observeEvent(input$save_note, {
      if (!shiny::isTruthy(input$v_followup)) {
        showNotification("Follow-up Date is required.", type = "error")
        return()
      }
      
      pt_id <- as.integer(current_pt()$id)
      existing <- dbGetQuery(pool, "SELECT id FROM visitsmodule WHERE patient_id = $1 AND visit_date = CURRENT_DATE", list(pt_id))
      
      if (is.null(note_state$active_visit_id) && nrow(existing) > 0) {
        showModal(modalDialog(
          title = "Existing Record Found",
          "A record already exists for today. Overwrite it or create a new entry?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_overwrite"), "Overwrite", class="btn-warning"),
            actionButton(ns("save_as_new"), "Save as New", class="btn-primary")
          )
        ))
      } else { execute_save_logic() }
    })
    
    observeEvent(input$save_as_new, { execute_save_logic(force_new = TRUE) })
    observeEvent(input$save_overwrite, { execute_save_logic(force_new = FALSE) })
    
    # --- 7. PMHx TABLE LOGIC (Fixed Delete) ---
    output$past_medical_history_table <- renderRHandsontable({
      df <- pmh_data()
      rhandsontable(df, stretchH = "all", height = 300) %>%
        hot_col("Condition", type = "text") %>%
        hot_col("Year", type = "text")
    })
    
    observeEvent(input$add_past_medical_history_row, {
      df <- if(!is.null(input$past_medical_history_table)) hot_to_r(input$past_medical_history_table) else pmh_data()
      pmh_data(rbind(df, data.frame(Condition = "", Year = "")))
    })
    
    observeEvent(input$del_past_medical_history_row, {
      # 1. Get current data from the table (not the reactiveVal, to capture unsaved typing)
      df <- if(!is.null(input$past_medical_history_table)) {
        hot_to_r(input$past_medical_history_table)
      } else {
        pmh_data()
      }
      
      # 2. Capture the selection
      sel <- input$past_medical_history_table_select
      
      # 3. Robust Deletion Logic
      if (!is.null(sel) && !is.null(sel$r) && nrow(df) > 0) {
        # Translate JavaScript 0-index to R 1-index
        # If the UI sends 0, it means Row 1. So we add 1.
        row_index <- sel$r 
        
        # Safety check: ensure index is within data frame bounds
        if(row_index > 0 && row_index <= nrow(df)) {
          pmh_data(df[-row_index, ])
          showNotification("Row removed. Click Save to apply to database.", type = "message")
        }
      } else {
        # FALLBACK: If selection is lost/null, delete the last row
        if (nrow(df) > 0) {
          pmh_data(df[-nrow(df), ])
          showNotification("No row selected. Removed last row by default.", type = "warning")
        } else {
          showNotification("Table is already empty.", type = "error")
        }
      }
    })
    
    # --- 8. DELETION AND HEADER ACTIONS ---
    output$delete_visit_ui <- renderUI({
      req(note_state$active_visit_id)
      actionButton(ns("confirm_delete_btn"), "Delete Selected Visit", class="btn-outline-danger w-100 btn-sm mt-2")
    })
    
    observeEvent(input$confirm_delete_btn, {
      showModal(modalDialog(
        title = "Confirm Deletion", "Permanently delete this visit record?",
        footer = tagList(modalButton("Cancel"), actionButton(ns("execute_delete"), "Delete", class="btn-danger"))
      ))
    })
    
    observeEvent(input$execute_delete, {
      dbExecute(pool, "DELETE FROM visitsmodule WHERE id = $1::int", list(as.integer(note_state$active_visit_id)))
      refresh_visits(refresh_visits() + 1)
      reset_clinical_ui()
      removeModal()
      showNotification("Record Deleted", type = "warning")
    })
    
    output$edit_button_ui <- renderUI({
      req(note_state$active_visit_id, is_locked())
      actionButton(ns("unlock_btn"), "Edit Record", class="btn-sm btn-warning", icon = icon("edit"))
    })
    
    observeEvent(input$unlock_btn, { is_locked(FALSE) })
    
    observeEvent(input$new_visit_btn, {
      reset_clinical_ui()
      is_locked(FALSE)
    })
    
    # Header and Badge outputs
    output$pid_badge_ui <- renderUI({
      req(current_pt())
      span(class="badge bg-secondary badge-pid", paste("PID:", current_pt()$id))
    })
    
    output$note_header <- renderText({
      if (is.null(note_state$active_visit_id)) {
        paste("New Clinical Note -", format(Sys.Date(), "%d %b %Y"))
      } else {
        paste("Viewing Record:", note_state$active_visit_date)
      }
    })
  })
}