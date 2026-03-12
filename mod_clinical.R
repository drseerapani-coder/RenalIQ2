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
                  uiOutput(ns("edit_button_ui")),
                  uiOutput(ns("save_all_ui")),   # "Save All" — shown only when unlocked
                  uiOutput(ns("pid_badge_ui"))
              )
          )
        ),

        card_body(
          uiOutput(ns("lock_message")),

          div(id = ns("clinical_input_form"),

              # ROW 1: COMPACT VITALS + Save Vitals button
              div(class = "vitals-card rounded-3",
                  layout_column_wrap(
                    width = 1/4, gap = "10px",
                    textInput(ns("v_bp"), div(icon("heart-pulse"), " BP"), placeholder = "120/80"),
                    numericInput(ns("v_hr"), div(icon("gauge-high"), " HR"), value = NA),
                    numericInput(ns("v_weight"), div(icon("weight-scale"), " Wt (kg)"), value = NA),
                    textInput(ns("v_temp"), div(icon("temperature-half"), " Temp"), placeholder = "98.4")
                  ),
                  div(class = "d-flex justify-content-end mt-1",
                      actionButton(ns("save_vitals_btn"), "Save Vitals",
                                   class = "btn-xs btn-outline-primary", icon = icon("floppy-disk"))
                  )
              ),

              # ROW 2: FOLLOW-UP SECTION (no default — prompted on save)
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
                          tags$label(icon("calendar-day"), " Follow-up Date",
                                     style="font-weight: bold; font-size: 0.8rem; display: block;"),
                          dateInput(ns("v_followup"), label = NULL, value = NULL, width = "100%")
                      )
                  )
              ),

              # ROW 3: NOTES AND GLOBAL PMHx
              layout_column_wrap(
                width = NULL,
                style = htmltools::css(grid_template_columns = "1.2fr 0.8fr"),
                gap = "15px",

                # Left Side: Examination & Plan + Save Notes button
                div(class="d-flex flex-column",
                    tags$label(icon("file-lines"), " Examination & Plan",
                               style="font-weight: bold; font-size: 0.85rem; margin-bottom: 5px;"),
                    textAreaInput(ns("clinic_notes"), NULL,
                                  rows = 14, width = "100%",
                                  placeholder = "Findings, assessment, plan..."),
                    div(class = "d-flex justify-content-end mt-1",
                        actionButton(ns("save_notes_btn"), "Save Notes",
                                     class = "btn-sm btn-outline-primary", icon = icon("floppy-disk"))
                    )
                ),

                # Right Side: Global PMHx (Decoupled from visits)
                div(
                  div(class="d-flex justify-content-between align-items-center mb-1",
                      tags$label(icon("history"), " Past Medical History",
                                 style="font-weight: bold; font-size: 0.85rem;"),
                      div(id = ns("pmhx_controls"), class = "d-flex gap-1",
                          actionButton(ns("add_past_medical_history_row"), "", icon = icon("plus"), class="btn-xs btn-outline-primary"),
                          actionButton(ns("del_past_medical_history_row"), "", icon = icon("trash"), class="btn-xs btn-outline-danger"),
                          actionButton(ns("save_pmhx_btn"), "Save PMHx", class="btn-xs btn-outline-success", icon = icon("floppy-disk"))
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
    save_count <- reactiveVal(0)

    # --- Reactive States ---
    refresh_visits     <- reactiveVal(0)
    is_locked          <- reactiveVal(FALSE)
    pending_save_config <- reactiveVal(NULL)   # carries include_pmhx + followup_override across modal steps
    note_state <- reactiveValues(
      active_visit_id   = NULL,
      active_visit_date = NULL
    )

    # PMH is global to the patient - loaded once per patient selection
    pmh_data <- reactiveVal(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))

    # "Save All" in header — only visible when unlocked
    output$save_all_ui <- renderUI({
      locked <- if (is.null(is_locked())) TRUE else is_locked()
      if (!locked) {
        actionButton(ns("save_all_btn"), "Save All",
                     class = "btn-sm btn-primary shadow-sm",
                     icon  = icon("floppy-disk"))
      }
    })

    # JS helper: clears the date input DOM value directly, bypassing Shiny's
    # updateDateInput(value=NA) which triggers an rlang::warn() that suppressWarnings()
    # cannot catch in newer Shiny versions.
    clear_followup <- function() {
      shinyjs::runjs(paste0(
        "var el = document.getElementById('", ns("v_followup"), "');",
        "if (el) { el.value = ''; el.dispatchEvent(new Event('change', {bubbles:true})); }"
      ))
    }

    # --- 1. RESET UI HELPER ---
    reset_clinical_ui <- function() {
      note_state$active_visit_id   <- NULL
      note_state$active_visit_date <- format(Sys.Date(), "%d %b %Y")
      is_locked(FALSE)

      updateTextInput(session,    "v_bp",         value = "")
      updateNumericInput(session, "v_hr",          value = NA)
      updateNumericInput(session, "v_weight",      value = NA)
      updateTextInput(session,    "v_temp",        value = "")
      updateTextAreaInput(session, "clinic_notes", value = "")
      clear_followup()
      updateRadioButtons(session, "quick_interval", selected = character(0))
    }

    # --- 2. GLOBAL PATIENT OBSERVER (PMHx Loading) ---
    observeEvent(current_pt(), {
      req(current_pt())
      reset_clinical_ui()

      pt_id <- as.character(current_pt()$id)
      res <- tryCatch({
        dbGetQuery(pool, "SELECT condition_text, onset_date FROM past_medical_history WHERE registration_id::text = $1", list(pt_id))
      }, error = function(e) {
        message("PMHx Load Error: ", e$message)
        NULL
      })

      if (!is.null(res) && nrow(res) > 0) {
        colnames(res) <- c("Condition", "Year")
        pmh_data(res)
      } else {
        pmh_data(data.frame(Condition = character(), Year = character(), stringsAsFactors = FALSE))
      }
    }, ignoreNULL = FALSE)

    # --- 3. UI LOCKING CONTROL ---
    observe({
      state <- is_locked()
      req(!is.null(state))
      toggle_state <- !state

      for (field in c("v_bp", "v_hr", "v_weight", "v_temp", "v_followup", "clinic_notes", "quick_interval")) {
        shinyjs::toggleState(field, condition = toggle_state)
      }
      shinyjs::toggle("pmhx_controls", condition = toggle_state)
      shinyjs::toggleState("save_vitals_btn", condition = toggle_state)
      shinyjs::toggleState("save_notes_btn",  condition = toggle_state)
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
      if (nrow(visits) == 0) return(p("No past visits found.", class = "text-muted ps-2 mt-2"))
      lapply(1:nrow(visits), function(i) {
        div(class = "mb-2",
            tags$button(
              class   = "btn btn-outline-secondary w-100 text-start shadow-sm py-2",
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                                ns("load_visit_id"), visits$id[i]),
              div(class = "d-flex justify-content-between w-100",
                  span(icon("calendar-day"), " ",
                       format(as.Date(visits$visit_date[i]), "%d %b %Y")))
            )
        )
      })
    })

    observeEvent(input$load_visit_id, {
      this_vid <- input$load_visit_id
      res <- dbGetQuery(pool, "SELECT visit_date, visit_json FROM visitsmodule WHERE id = $1",
                        list(as.integer(this_vid)))
      req(nrow(res) > 0)

      note_state$active_visit_id   <- this_vid
      note_state$active_visit_date <- format(as.Date(res$visit_date[1]), "%d %b %Y")

      data <- jsonlite::fromJSON(res$visit_json[1])

      updateTextInput(session,     "v_bp",         value = data$vitals$bp     %||% "")
      updateNumericInput(session,  "v_hr",          value = data$vitals$hr     %||% NA)
      updateNumericInput(session,  "v_weight",      value = data$vitals$weight %||% NA)
      updateTextInput(session,     "v_temp",        value = data$vitals$temp   %||% "")
      updateTextAreaInput(session, "clinic_notes",  value = data$clinic_notes  %||% "")

      f_date <- data$followup_date
      if (shiny::isTruthy(f_date)) {
        f_date_str <- as.character(unlist(f_date)[1])
        tryCatch(
          updateDateInput(session, "v_followup", value = as.Date(f_date_str)),
          error = function(e) clear_followup()
        )
      } else {
        clear_followup()
      }

      is_locked(TRUE)
    })

    # --- 6. SAVE LOGIC ---

    # 6a. Save PMHx only (no visit record needed)
    do_save_pmhx <- function() {
      req(current_pt(), user_info())
      curr_user <- user_info()$username
      pt_id     <- as.integer(current_pt()$id)
      tryCatch({
        if (!is.null(input$past_medical_history_table)) {
          final_pmhx <- hot_to_r(input$past_medical_history_table)
          final_pmhx <- final_pmhx[nzchar(trimws(as.character(final_pmhx$Condition))), ]
          dbExecute(pool, "DELETE FROM past_medical_history WHERE registration_id = $1", list(pt_id))
          if (nrow(final_pmhx) > 0) {
            for (i in seq_len(nrow(final_pmhx))) {
              dbExecute(pool,
                "INSERT INTO past_medical_history (registration_id, condition_text, onset_date, created_by) VALUES ($1, $2, $3, $4)",
                list(pt_id, as.character(final_pmhx$Condition[i]), as.character(final_pmhx$Year[i]), curr_user))
            }
          }
          pmh_data(final_pmhx)
        }
        showNotification("PMHx saved.", type = "message")
      }, error = function(e) { showNotification(paste("PMHx Save Error:", e$message), type = "error") })
    }

    # 6b. Execute visit save (vitals + notes + optional PMHx)
    do_execute_save <- function(force_new = FALSE, include_pmhx = TRUE, followup_override = NULL) {
      req(current_pt(), user_info())
      curr_user    <- user_info()$username
      pt_id        <- as.integer(current_pt()$id)
      followup_val <- if (!is.null(followup_override)) as.character(followup_override) else as.character(input$v_followup)

      payload   <- list(
        vitals        = list(bp = input$v_bp, hr = input$v_hr, weight = input$v_weight, temp = input$v_temp),
        clinic_notes  = input$clinic_notes,
        followup_date = followup_val
      )
      json_data <- jsonlite::toJSON(payload, auto_unbox = TRUE)

      tryCatch({
        poolWithTransaction(pool, function(con) {
          if (is.null(note_state$active_visit_id) || force_new) {
            new_id <- DBI::dbGetQuery(con,
              "INSERT INTO visitsmodule (patient_id, visit_date, visit_json, created_by, updated_at)
               VALUES ($1, CURRENT_DATE, $2, $3, NOW()) RETURNING id",
              list(pt_id, as.character(json_data), curr_user))$id
            note_state$active_visit_id <- new_id
          } else {
            DBI::dbExecute(con,
              "UPDATE visitsmodule SET visit_json = $1, updated_by = $2, updated_at = NOW()
               WHERE id = $3::int",
              list(as.character(json_data), curr_user, as.integer(note_state$active_visit_id)))
          }
          if (include_pmhx && !is.null(input$past_medical_history_table)) {
            final_pmhx <- hot_to_r(input$past_medical_history_table)
            final_pmhx <- final_pmhx[nzchar(trimws(as.character(final_pmhx$Condition))), ]
            DBI::dbExecute(con, "DELETE FROM past_medical_history WHERE registration_id = $1", list(pt_id))
            if (nrow(final_pmhx) > 0) {
              for (i in seq_len(nrow(final_pmhx))) {
                DBI::dbExecute(con,
                  "INSERT INTO past_medical_history (registration_id, condition_text, onset_date, created_by) VALUES ($1, $2, $3, $4)",
                  list(pt_id, as.character(final_pmhx$Condition[i]), as.character(final_pmhx$Year[i]), curr_user))
              }
            }
            pmh_data(final_pmhx)
          }
        })
        # If followup was set via modal, update the date input to reflect the chosen date
        if (!is.null(followup_override) && nchar(trimws(followup_val)) > 0) {
          tryCatch(updateDateInput(session, "v_followup", value = as.Date(followup_val)), error = function(e) NULL)
        }
        removeModal()
        is_locked(TRUE)
        refresh_visits(refresh_visits() + 1)
        save_count(save_count() + 1)
        msg <- if (include_pmhx) "Visit and PMHx saved." else "Visit saved."
        showNotification(msg, type = "message")
      }, error = function(e) { showNotification(paste("Save Error:", e$message), type = "error") })
    }

    # 6c. Check for duplicate date, then execute (or show overwrite modal)
    proceed_to_save <- function(include_pmhx, followup_override = NULL) {
      pt_id    <- as.integer(current_pt()$id)
      existing <- dbGetQuery(pool,
        "SELECT id FROM visitsmodule WHERE patient_id = $1 AND visit_date = CURRENT_DATE", list(pt_id))
      if (is.null(note_state$active_visit_id) && nrow(existing) > 0) {
        pending_save_config(list(include_pmhx = include_pmhx, followup_override = followup_override))
        showModal(modalDialog(
          title = "Existing Record Found",
          "A record already exists for today. Overwrite it or create a new entry?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_overwrite"), "Overwrite",    class = "btn-warning"),
            actionButton(ns("save_as_new"),    "Save as New", class = "btn-primary")
          )
        ))
      } else {
        do_execute_save(force_new = FALSE, include_pmhx = include_pmhx, followup_override = followup_override)
      }
    }

    # 6d. Entry point: check for followup (if required), then proceed
    followup_modal_ui <- function() {
      modalDialog(
        title = "Set Follow-up Date",
        p("A follow-up date is required to save the note."),
        dateInput(ns("modal_followup_date"), "Follow-up date", value = Sys.Date() + 14, width = "100%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("modal_confirm_save"), "Set & Save", class = "btn-primary")
        ), size = "s", easyClose = TRUE
      )
    }

    check_and_save <- function(include_pmhx, require_followup = TRUE) {
      if (require_followup && !shiny::isTruthy(input$v_followup)) {
        pending_save_config(list(include_pmhx = include_pmhx, followup_override = NULL))
        showModal(followup_modal_ui())
        return()
      }
      proceed_to_save(include_pmhx = include_pmhx, followup_override = NULL)
    }

    # --- Save button observers ---
    observeEvent(input$save_vitals_btn, {
      # Vitals: no followup required
      proceed_to_save(include_pmhx = FALSE, followup_override = NULL)
    })

    observeEvent(input$save_notes_btn, {
      # Notes: followup required
      check_and_save(include_pmhx = FALSE, require_followup = TRUE)
    })

    observeEvent(input$save_pmhx_btn, {
      do_save_pmhx()
    })

    observeEvent(input$save_all_btn, {
      # Save All: followup required, includes PMHx
      check_and_save(include_pmhx = TRUE, require_followup = TRUE)
    })

    # Followup modal confirm
    observeEvent(input$modal_confirm_save, {
      cfg <- pending_save_config()
      req(cfg)
      followup_override <- as.character(input$modal_followup_date)
      removeModal()
      proceed_to_save(include_pmhx = cfg$include_pmhx, followup_override = followup_override)
      pending_save_config(NULL)
    })

    # Duplicate-date modal confirm
    observeEvent(input$save_overwrite, {
      cfg <- pending_save_config()
      do_execute_save(force_new = FALSE, include_pmhx = cfg$include_pmhx %||% TRUE, followup_override = cfg$followup_override)
      pending_save_config(NULL)
    })
    observeEvent(input$save_as_new, {
      cfg <- pending_save_config()
      do_execute_save(force_new = TRUE, include_pmhx = cfg$include_pmhx %||% TRUE, followup_override = cfg$followup_override)
      pending_save_config(NULL)
    })

    # --- 7. PMHx TABLE LOGIC ---
    # minSpareRows = 1: Tab on last cell of last row adds a new row
    output$past_medical_history_table <- renderRHandsontable({
      df <- pmh_data()
      suppressWarnings(
        rhandsontable(df, stretchH = "all", height = 300, useTypes = FALSE, minSpareRows = 1)
      )
    })

    observeEvent(input$add_past_medical_history_row, {
      df <- if (!is.null(input$past_medical_history_table)) hot_to_r(input$past_medical_history_table) else pmh_data()
      pmh_data(rbind(df, data.frame(Condition = "", Year = "")))
    })

    observeEvent(input$del_past_medical_history_row, {
      df <- if (!is.null(input$past_medical_history_table)) {
        hot_to_r(input$past_medical_history_table)
      } else {
        pmh_data()
      }
      sel <- input$past_medical_history_table_select
      if (!is.null(sel) && !is.null(sel$r) && nrow(df) > 0) {
        row_to_del <- as.numeric(sel$r) + 1
        if (row_to_del <= nrow(df)) {
          pmh_data(df[-row_to_del, ])
          showNotification("Row removed.", type = "message")
        }
      } else if (nrow(df) > 0) {
        pmh_data(df[-nrow(df), ])
        showNotification("Removed last row.", type = "warning")
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
      save_count(save_count() + 1)
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

    return(list(saved = reactive({ save_count() })))
  })
}
