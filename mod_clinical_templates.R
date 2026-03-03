library(shiny)
library(bslib)
library(DBI)
library(pool)
library(jsonlite)
library(rhandsontable)
library(readxl)
library(glue)

# --- FIXED HELPER: NULL/EMPTY COALESCING ---
# This version uses is.null() and length() checks safely to avoid the coercion error.
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) > 1) return(a) # Return the vector/list immediately if it has content
  if (length(a) == 0) return(b)
  val <- trimws(as.character(a))
  if (val == "" || val == "NA" || val == "Not recorded") return(b)
  return(a)
}

# --- UI FUNCTION ---
clinical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      sidebar = sidebar(
        title = "Visit Management",
        width = 320,
        actionButton(ns("new_visit_btn"), "Start New Empty Note", 
                     class="btn-success w-100 mb-2", icon = icon("plus")),
        uiOutput(ns("delete_visit_ui")),
        hr(),
        tags$label("Clinical Templates", style="font-weight: bold;"),
        selectInput(ns("template_select"), NULL, choices = c("Loading..." = ""), selected = ""),
        actionButton(ns("refresh_templates"), "Refresh Excel Templates", 
                     class="btn-sm btn-outline-info w-100 mb-3"),
        hr(),
        tags$label("Visit History", style="font-weight: bold;"),
        uiOutput(ns("visit_tiles_ui"))
      ),
      navset_card_pill(
        nav_panel("Clinical Entry",
                  card_body(
                    layout_column_wrap(
                      width = 1/5,
                      textInput(ns("v_bp"), "BP", placeholder = "120/80"),
                      numericInput(ns("v_hr"), "Pulse", value = NA),
                      numericInput(ns("v_weight"), "Weight (kg)", value = NA),
                      textInput(ns("v_temp"), "Temp", placeholder = "98.4"),
                      dateInput(ns("v_followup"), "Follow-up", value = Sys.Date())
                    ),
                    tags$hr(style = "margin: 20px 0; border-top: 2px solid #eee;"),
                    layout_column_wrap(
                      width = NULL,
                      style = htmltools::css(grid_template_columns = "1fr 1fr"), 
                      div(style = "display: flex; flex-direction: column; gap: 10px;",
                          tags$label("Examination Findings", style="font-weight: bold; font-size: 1.1rem;"),
                          uiOutput(ns("dynamic_template_ui")), 
                          textAreaInput(ns("clinic_notes"), "Assessment & Plan Summary", 
                                        rows = 10, width = "100%", 
                                        placeholder = "Final assessment and management plan...")
                      ),
                      div(
                        tags$label("Past Medical History", style="font-weight: bold;"),
                        rHandsontableOutput(ns("past_medical_history_table")),
                        div(class = "mt-2 d-flex gap-2",
                            actionButton(ns("add_pmhx_row"), "Add Condition", class="btn-sm btn-outline-secondary"),
                            actionButton(ns("del_pmhx_row"), "Remove Row", class="btn-sm btn-outline-danger")
                        )
                      )
                    )
                  )
        ),
        nav_panel("Consolidated Summary",
                  card_body(
                    div(class="d-flex justify-content-between mb-2",
                        tags$h5("Final Visit Record"),
                        tags$small("Showing only valid recorded findings.")),
                    verbatimTextOutput(ns("clinical_summary_view"), placeholder = TRUE)
                  )
        ),
        footer = card_footer(
          actionButton(ns("save_note"), "Save Visit Record", class="btn-primary btn-lg w-100")
        )
      )
    )
  )
}

# --- SERVER FUNCTION ---
clinical_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    target_file <- "templates.xlsx"
    
    note_state <- reactiveValues(
      active_visit_id = NULL,
      active_visit_date = NULL,
      loaded_template_data = list(), 
      active_templates = character(0) 
    )
    pmh_data <- reactiveVal(data.frame(Condition = "", Year = "", stringsAsFactors = FALSE))
    
    perform_total_reset <- function() {
      note_state$active_visit_id <- NULL
      note_state$active_visit_date <- format(Sys.Date(), "%d %b %Y")
      note_state$loaded_template_data <- list()
      note_state$active_templates <- character(0)
      updateSelectInput(session, "template_select", selected = "")
      updateTextInput(session, "v_bp", value = ""); updateNumericInput(session, "v_hr", value = NA)
      updateNumericInput(session, "v_weight", value = NA); updateTextInput(session, "v_temp", value = "")
      updateTextAreaInput(session, "clinic_notes", value = "")
      updateDateInput(session, "v_followup", value = Sys.Date())
    }
    
    observe({ 
      if (file.exists(target_file)) {
        updateSelectInput(session, "template_select", choices = c("Select Template" = "", readxl::excel_sheets(target_file)))
      }
    })
    
    output$dynamic_template_ui <- renderUI({
      t_name <- input$template_select
      req(t_name != "")
      df <- readxl::read_excel(target_file, sheet = t_name)
      lapply(1:nrow(df), function(i) {
        field_id <- paste0(t_name, "_field_", i) 
        val <- note_state$loaded_template_data[[field_id]] %||% ""
        textInput(ns(field_id), as.character(df[i, 1]), value = val, width = "100%")
      })
    })
    
    # LINKING LOGIC: Namespaced tracking to prevent data leakage
    observe({
      t_name <- input$template_select
      req(t_name != "")
      df <- tryCatch({ readxl::read_excel(target_file, sheet = t_name) }, error = function(e) NULL)
      req(!is.null(df))
      
      for (i in 1:nrow(df)) {
        field_id <- paste0(t_name, "_field_", i)
        raw_val <- input[[field_id]]
        val <- trimws(as.character(raw_val %||% ""))
        
        if (val != "" && val != "NA" && val != "Not recorded") {
          note_state$loaded_template_data[[field_id]] <- raw_val
          if (!(t_name %in% note_state$active_templates)) {
            note_state$active_templates <- unique(c(note_state$active_templates, t_name))
          }
        }
      }
    })
    
    # CONSOLIDATED SUMMARY: Filtered & Grouped
    output$clinical_summary_view <- renderText({
      res <- paste0("CLINICAL SUMMARY: ", note_state$active_visit_date %||% format(Sys.Date(), "%d %b %Y"), "\n")
      res <- paste0(res, "===========================================\n\n")
      
      res <- paste0(res, sprintf("VITALS: BP %s | HR %s | WT %s | T %s\n", 
                                 input$v_bp %||% "-", input$v_hr %||% "-", 
                                 input$v_weight %||% "-", input$v_temp %||% "-"))
      res <- paste0(res, "-------------------------------------------\n\n")
      
      all_data <- note_state$loaded_template_data
      used_t <- note_state$active_templates
      
      if (length(used_t) > 0) {
        for (t_name in used_t) {
          t_keys <- names(all_data)[grepl(paste0("^", t_name, "_field_"), names(all_data))]
          valid_entries <- list()
          labels_df <- tryCatch({ readxl::read_excel(target_file, sheet = t_name) }, error = function(e) NULL)
          
          if (!is.null(labels_df)) {
            for (f_id in t_keys) {
              val <- trimws(as.character(all_data[[f_id]] %||% ""))
              if (val != "" && val != "NA" && val != "Not recorded") {
                idx <- as.numeric(gsub(paste0(t_name, "_field_"), "", f_id))
                label <- as.character(labels_df[idx, 1])
                valid_entries[[label]] <- all_data[[f_id]]
              }
            }
          }
          
          if (length(valid_entries) > 0) {
            res <- paste0(res, "--- ", toupper(t_name), " FINDINGS ---\n")
            for (lbl in names(valid_entries)) {
              res <- paste0(res, sprintf("  • %s: %s\n", lbl, valid_entries[[lbl]]))
            }
            res <- paste0(res, "\n")
          }
        }
      }
      
      res <- paste0(res, "ASSESSMENT & PLAN:\n")
      res <- paste0(res, "  ", input$clinic_notes %||% "No summary recorded.")
      return(res)
    })
    
    # SAVE RECORD
    observeEvent(input$save_note, {
      req(current_pt())
      payload <- list(
        vitals = list(bp = input$v_bp, hr = input$v_hr, weight = input$v_weight, temp = input$v_temp),
        template_fields = note_state$loaded_template_data,
        active_templates = note_state$active_templates,
        clinic_notes = input$clinic_notes,
        followup_date = as.character(input$v_followup)
      )
      json_data <- toJSON(payload, auto_unbox = TRUE)
      
      tryCatch({
        poolWithTransaction(pool, function(con) {
          if (is.null(note_state$active_visit_id)) {
            dbExecute(con, glue_sql("INSERT INTO visitsmodule (patient_id, visit_date, visit_json, created_by, updated_at) VALUES ({as.integer(current_pt()$id)}, CURRENT_DATE, {json_data}, {user_info()$username}, NOW())", .con = con))
          } else {
            dbExecute(con, glue_sql("UPDATE visitsmodule SET visit_json = {json_data}, updated_by = {user_info()$username}, updated_at = NOW() WHERE id = {as.integer(note_state$active_visit_id)}", .con = con))
          }
        })
        showNotification("Visit Saved Successfully")
      }, error = function(e) showNotification(e$message, type = "error"))
    })
    
    # LOAD VISIT
    observeEvent(input$select_visit, {
      vid <- input$select_visit; req(vid)
      res <- dbGetQuery(pool, "SELECT visit_json, visit_date FROM visitsmodule WHERE id = $1::int", list(as.integer(vid)))
      req(nrow(res) > 0)
      
      # Safer JSON Parsing
      data <- fromJSON(res$visit_json[1], simplifyVector = FALSE)
      
      note_state$active_visit_id <- vid
      note_state$active_visit_date <- format(as.Date(res$visit_date[1]), "%d %b %Y")
      note_state$loaded_template_data <- data$template_fields %||% list()
      note_state$active_templates <- unlist(data$active_templates %||% character(0))
      
      # Standard UI Updates
      updateTextInput(session, "v_bp", value = data$vitals$bp %||% "")
      updateNumericInput(session, "v_hr", value = data$vitals$hr %||% NA)
      updateNumericInput(session, "v_weight", value = data$vitals$weight %||% NA)
      updateTextInput(session, "v_temp", value = data$vitals$temp %||% "")
      updateTextAreaInput(session, "clinic_notes", value = data$clinic_notes %||% "")
    })
    
    observeEvent(current_pt(), { perform_total_reset() }, ignoreNULL = FALSE)
  })
}