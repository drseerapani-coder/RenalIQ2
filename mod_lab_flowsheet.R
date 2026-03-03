# mod_lab_flowsheet.R
# Fully corrected version — all 6 bugs fixed

library(rhandsontable)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(bslib)

# ════════════════════════════════════════════════════════════
#  UI
# ════════════════════════════════════════════════════════════
lab_flowsheet_ui <- function(id, lab_config) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .datepicker-dropdown { z-index: 9999 !important; }
        .modal-body { overflow-y: visible !important; }
      "))
    ),
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          span("Clinical Lab Flowsheet"),
          div(
            actionButton(ns("refresh_data"),    "Refresh",          class = "btn-outline-primary me-2", icon = icon("sync")),
            actionButton(ns("open_add_lab"),    "Add New Date",     class = "btn-success me-2",         icon = icon("plus")),
            actionButton(ns("save_flowsheet"),  "Save All Changes", class = "btn-primary",              icon = icon("save"))
          )
        )
      ),
      card_body(
        div(
          class = "d-flex justify-content-between align-items-center mb-1",
          helpText("Right-click any date column header or cell to remove that date's entire record."),
          uiOutput(ns("save_status"))
        ),
        rHandsontableOutput(ns("history_table")) %>% withSpinner(color = "#26A69A")
      )
    )
  )
}

# ════════════════════════════════════════════════════════════
#  SERVER
# ════════════════════════════════════════════════════════════
lab_flowsheet_server <- function(id, pool, current_pt, lab_targets_raw,
                                 parent_nav, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    save_count <- reactiveVal(0)
    
    # ── Reactive state ───────────────────────────────────────
    refresh_trigger      <- reactiveVal(0)
    pending_delete_date  <- reactiveVal(NULL)   # FIX #5: lift out of nested observer
    save_msg             <- reactiveVal(NULL)    # drives the inline "Data saved" banner
    
    output$save_status <- renderUI({
      req(save_msg())
      div(
        style = paste(
          "display:inline-flex; align-items:center; gap:6px;",
          "background:#d1e7dd; color:#0a3622; border:1px solid #a3cfbb;",
          "border-radius:6px; padding:4px 12px; font-size:0.85rem; font-weight:600;"
        ),
        icon("circle-check"), save_msg()
      )
    })
    
    # ── Refresh triggers ─────────────────────────────────────
    observeEvent(input$refresh_data, {
      refresh_trigger(refresh_trigger() + 1)
      showNotification("Flowsheet updated from database.", type = "message")
    })
    
    observeEvent(parent_nav(), {
      if (parent_nav() == "Labs") {
        refresh_trigger(refresh_trigger() + 1)
      }
    })
    
    # ════════════════════════════════════════════════════════
    #  1. RENDER FLOWSHEET
    # ════════════════════════════════════════════════════════
    output$history_table <- renderRHandsontable({
      req(current_pt())
      refresh_trigger()
      
      query <- "
        SELECT test_name, test_date, num_val, value_text
        FROM   labs
        WHERE  patient_id::text = $1
        ORDER  BY test_date DESC
      "
      res <- dbGetQuery(pool, query, list(as.character(current_pt()$id)))
      
      df_base <- data.frame(test_name = lab_targets_raw$test_name,
                            stringsAsFactors = FALSE)
      
      if (nrow(res) > 0) {
        res$display_val <- ifelse(!is.na(res$num_val),
                                  as.character(res$num_val),
                                  res$value_text)
        # FIX #4: use unambiguous date format in column names (YYYY-MM-DD)
        # so parsing is locale-safe on any server
        res$fmt_date <- format(as.Date(res$test_date), "%Y-%m-%d")
        
        df_wide <- res %>%
          select(test_name, fmt_date, display_val) %>%
          pivot_wider(names_from = fmt_date, values_from = display_val)
        
        df_final <- left_join(df_base, df_wide, by = "test_name")
      } else {
        df_final <- df_base
      }
      
      colnames(df_final)[1] <- "Parameter"
      
      # FIX #2: inject ns() into the JS callback so the namespace is always correct
      delete_js <- htmlwidgets::JS(paste0("
        function (key, options) {
          var sel = this.getSelected();
          if (sel && sel.length > 0) {
            // sel[0] = [startRow, startCol, endRow, endCol]
            // Pass the column index directly as a plain number
            Shiny.setInputValue(
              '", ns("delete_request"), "',
              sel[0][1],
              {priority: 'event'}
            );
          }
        }
      "))
      
      suppressWarnings(
        rhandsontable(df_final,
                      height   = 500,
                      width    = "100%",
                      stretchH = "none",
                      useTypes = FALSE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols(fixedColumnsLeft = 1) %>%
          hot_col("Parameter", width = 200, readOnly = TRUE) %>%
          hot_cols(colWidths = 100) %>%
          hot_context_menu(
            allowRowEdit = FALSE,
            allowColEdit = TRUE,
            customOpts = list(
              delete_column = list(
                name     = "Delete this entire date column",
                callback = delete_js
              )
            )
          )
      )
    })
    
    # ════════════════════════════════════════════════════════
    #  2. HANDLE DELETE REQUEST
    #     FIX #5: pending_delete_date is a module-level reactiveVal,
    #     not defined inside this observer — no more stale closures.
    # ════════════════════════════════════════════════════════
    observeEvent(input$delete_request, {
      col_idx <- as.integer(input$delete_request)
      
      if (is.na(col_idx) || col_idx < 1) {
        showNotification("Action Denied: Cannot delete the Parameter column.",
                         type = "error")
        return()
      }
      
      # Query distinct dates in same DESC order as the rendered table
      date_res <- dbGetQuery(
        pool,
        "SELECT DISTINCT test_date FROM labs WHERE patient_id::text = $1 ORDER BY test_date DESC",
        list(as.character(current_pt()$id))
      )
      
      if (nrow(date_res) < col_idx) {
        showNotification("Could not identify the date for this column.",
                         type = "warning")
        return()
      }
      
      target_date <- as.character(date_res$test_date[col_idx])
      fmt_date    <- format(as.Date(target_date), "%d %b %Y")
      
      # Store for use in the confirmation observer (FIX #5)
      pending_delete_date(target_date)
      
      showModal(modalDialog(
        title = "Confirm Column Deletion",
        div(
          style = "text-align: center; padding: 20px;",
          icon("exclamation-triangle", class = "text-danger", style = "font-size: 3rem;"),
          h4("Delete Lab Records?", style = "margin-top: 15px;"),
          p(paste("This will permanently delete all lab results for:", fmt_date))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_right_click_delete"),
                       "Confirm Permanent Delete",
                       class = "btn-danger")
        )
      ))
    })
    
    # FIX #5: Confirmation observer is top-level — always uses the current
    # pending_delete_date(), never a stale closure value.
    observeEvent(input$confirm_right_click_delete, {
      req(pending_delete_date())
      
      target_date <- pending_delete_date()
      fmt_date    <- format(as.Date(target_date), "%d %b %Y")
      
      dbExecute(
        pool,
        "DELETE FROM labs WHERE patient_id::text = $1 AND test_date::date = $2::date",
        list(as.character(current_pt()$id), target_date)
      )
      save_count(save_count() + 1)
      pending_delete_date(NULL)           # clear after use
      refresh_trigger(refresh_trigger() + 1)
      removeModal()
      showNotification(paste("Successfully removed labs for", fmt_date),
                       type = "message")
    })
    
    # ════════════════════════════════════════════════════════
    #  3. LAB ENTRY MODAL — open
    # ════════════════════════════════════════════════════════
    
    # Helper: fetch existing lab values for a patient+date from the DB.
    # Returns a named list: list("Haemoglobin" = 12.5, "WBC" = 7.1, ...)
    # with NA for any test that has no record on that date.
    fetch_existing_labs <- function(pt_id, date_str) {
      res <- dbGetQuery(pool, "
        SELECT test_name, num_val, value_text
        FROM   labs
        WHERE  patient_id::text = $1
          AND  test_date::date  = $2::date",
                        list(pt_id, date_str)
      )
      # Build named lookup: prefer num_val, fall back to value_text
      vals <- setNames(
        lapply(seq_len(nrow(res)), function(i) {
          if (!is.na(res$num_val[i])) res$num_val[i] else res$value_text[i]
        }),
        res$test_name
      )
      vals
    }
    
    observeEvent(input$open_add_lab, {
      req(current_pt())
      pt_id      <- as.character(current_pt()$id)
      init_date  <- as.character(Sys.Date())
      prefill    <- fetch_existing_labs(pt_id, init_date)
      is_edit    <- length(prefill) > 0
      modal_title <- if (is_edit) {
        paste0("Edit Lab Entry — ", format(as.Date(init_date), "%d %b %Y"),
               "  ✏️")
      } else {
        "Add New Lab Entry"
      }
      
      showModal(modalDialog(
        title     = modal_title,
        size      = "l",
        easyClose = FALSE,
        div(
          class = "d-flex justify-content-between align-items-center mb-3 p-2 border-bottom",
          div(
            class = "d-flex align-items-center gap-3",
            dateInput(ns("modal_lab_date"), "Test Date:",
                      value = Sys.Date(), width = "150px"),
            # Badge shows whether this date already has data
            uiOutput(ns("modal_date_status"))
          ),
          div(
            actionButton(ns("cancel_top"),      "Cancel",       class = "btn-outline-secondary me-2"),
            actionButton(ns("save_labs_modal"), "Save Records", class = "btn-success",
                         icon = icon("check"))
          )
        ),
        div(
          class = "mb-2",
          actionButton(ns("expand_all"),   "Expand All",   class = "btn-sm btn-link"),
          actionButton(ns("collapse_all"), "Collapse All", class = "btn-sm btn-link")
        ),
        accordion(
          id   = ns("lab_accordion"),
          open = FALSE,
          lapply(unique(lab_targets_raw$category), function(cat) {
            accordion_panel(
              title = cat,
              value = cat,
              layout_column_wrap(
                width = 1/3,
                lapply(
                  lab_targets_raw$test_name[lab_targets_raw$category == cat],
                  function(t) {
                    existing <- prefill[[t]]
                    # numericInput for numeric prefills; if value is text it
                    # shows NA in the spinner (acceptable — text fields are
                    # rare and editable in the flowsheet grid directly)
                    numericInput(
                      ns(paste0("lab_", make.names(t))),
                      t,
                      value = if (!is.null(existing) && is.numeric(existing))
                        existing
                      else
                        NA
                    )
                  }
                )
              )
            )
          })
        ),
        footer = NULL
      ))
    })
    
    # ── Re-prefill all inputs when user changes the date picker ──────────────
    # Uses updateNumericInput so we don't have to rebuild the whole modal.
    observeEvent(input$modal_lab_date, {
      req(current_pt(), input$modal_lab_date)
      pt_id    <- as.character(current_pt()$id)
      date_str <- as.character(input$modal_lab_date)
      prefill  <- fetch_existing_labs(pt_id, date_str)
      
      for (t in lab_targets_raw$test_name) {
        existing <- prefill[[t]]
        updateNumericInput(
          session,
          paste0("lab_", make.names(t)),
          value = if (!is.null(existing) && is.numeric(existing)) existing else NA
        )
      }
    }, ignoreInit = TRUE)   # ignoreInit = don't fire on first render
    
    # ── Status badge: shows "Existing data" or "New entry" next to date ──────
    output$modal_date_status <- renderUI({
      req(current_pt(), input$modal_lab_date)
      pt_id    <- as.character(current_pt()$id)
      date_str <- as.character(input$modal_lab_date)
      prefill  <- fetch_existing_labs(pt_id, date_str)
      
      if (length(prefill) > 0) {
        div(
          style = paste(
            "display:inline-flex; align-items:center; gap:5px;",
            "background:#fff3cd; color:#664d03; border:1px solid #ffecb5;",
            "border-radius:6px; padding:3px 10px; font-size:0.82rem; font-weight:600;"
          ),
          icon("pencil"), paste0(length(prefill), " existing value(s) loaded")
        )
      } else {
        div(
          style = paste(
            "display:inline-flex; align-items:center; gap:5px;",
            "background:#d1e7dd; color:#0a3622; border:1px solid #a3cfbb;",
            "border-radius:6px; padding:3px 10px; font-size:0.82rem; font-weight:600;"
          ),
          icon("plus"), "New entry"
        )
      }
    })
    
    observeEvent(input$expand_all,   {
      accordion_panel_open("lab_accordion",  values = unique(lab_targets_raw$category))
    })
    observeEvent(input$collapse_all, {
      accordion_panel_close("lab_accordion", values = unique(lab_targets_raw$category))
    })
    observeEvent(input$cancel_top, { removeModal() })
    
    # ════════════════════════════════════════════════════════
    #  4. SAVE NEW DATE (Modal Insert)
    #     FIX #1: use v_date and t/val — not clean_date/df_long
    # ════════════════════════════════════════════════════════
    observeEvent(input$save_labs_modal, {
      req(current_pt(), input$modal_lab_date, user_info())
      
      curr_user <- user_info()$username
      pt_id     <- as.character(current_pt()$id)
      v_date    <- as.character(input$modal_lab_date)   # already YYYY-MM-DD from dateInput
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      tryCatch({
        DBI::dbExecute(con, "BEGIN")
        
        saved_count <- 0L
        
        for (t in lab_targets_raw$test_name) {
          val <- input[[paste0("lab_", make.names(t))]]
          
          if (!is.null(val) && !is.na(val)) {
            # FIX #1: correct variable names — v_date and t, not clean_date/df_long
            DBI::dbExecute(con, "
              INSERT INTO labs
                (patient_id, test_date, test_name, num_val, updated_by, updated_at)
              VALUES
                ($1, $2, $3, $4, $5, NOW())
              ON CONFLICT (patient_id, test_date, test_name)
              DO UPDATE SET
                num_val    = EXCLUDED.num_val,
                updated_by = EXCLUDED.updated_by,
                updated_at = NOW()",
                           list(pt_id, v_date, t, val, curr_user)
            )
            saved_count <- saved_count + 1L
          }
        }
        
        if (saved_count == 0L) {
          DBI::dbExecute(con, "ROLLBACK")
          showNotification("No values entered — nothing was saved.", type = "warning")
          return()
        }
        
        log_audit(con, curr_user, "UPSERT_LAB_ENTRY", "labs",
                  paste0(pt_id, "_", v_date))
        
        DBI::dbExecute(con, "COMMIT")
        save_count(save_count() + 1)
        
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
        showNotification(
          paste0("Saved ", saved_count, " lab value(s) for ", v_date, "."),
          type = "message"
        )
        
      }, error = function(e) {
        DBI::dbExecute(con, "ROLLBACK")
        showNotification(paste("Save Error:", e$message), type = "error")
        message("Modal save error: ", e$message)
      })
    })
    
    # ════════════════════════════════════════════════════════
    #  5. SAVE ALL CHANGES (Bulk Flowsheet Update)
    #     FIX #3: robust cleaning handles all rhandsontable edge cases
    #     FIX #4: date column names are YYYY-MM-DD — no locale risk
    # ════════════════════════════════════════════════════════
    observeEvent(input$save_flowsheet, {
      req(current_pt(), user_info(), input$history_table)
      curr_user <- user_info()$username
      pt_id     <- as.character(current_pt()$id)
      
      # Convert handsontable back to R dataframe
      df <- hot_to_r(input$history_table)
      
      # ── Reshape the handsontable into long form ─────────────────────────────
      # We keep ALL cells — including empty ones — because an empty cell that
      # previously had a value means the user deliberately cleared it and we
      # must DELETE that row from the database.
      df_long <- df %>%
        mutate(across(-Parameter, as.character)) %>%
        pivot_longer(
          cols      = -Parameter,
          names_to  = "test_date_str",
          values_to = "raw_val"
        ) %>%
        mutate(
          # Step 1: strip standard whitespace and U+00A0 non-breaking spaces
          raw_val = trimws(raw_val),
          raw_val = gsub(" ", "", raw_val, fixed = TRUE),
          
          # Step 2: normalise rhandsontable's empty-cell sentinel "NA" -> true NA
          raw_val = ifelse(raw_val %in% c("NA", ""), NA_character_, raw_val),
          
          # Step 3: classify every cell
          #   "numeric" -> write to num_val
          #   "text"    -> write to value_text
          #   "empty"   -> DELETE the row from DB (user cleared the cell)
          num_val    = suppressWarnings(as.numeric(raw_val)),
          value_text = ifelse(is.na(num_val) & !is.na(raw_val), raw_val, NA_character_),
          val_type   = case_when(
            !is.na(num_val)    ~ "numeric",
            !is.na(value_text) ~ "text",
            TRUE               ~ "empty"      # <-- kept, not filtered out
          )
        )
      
      # Parse dates for every row now (needed for both upserts and deletes)
      df_long <- df_long %>%
        mutate(
          clean_date = suppressWarnings(
            format(as.Date(test_date_str), "%Y-%m-%d")
          )
        ) %>%
        filter(!is.na(clean_date))   # drop any column that isn't a real date
      
      if (nrow(df_long) == 0) {
        showNotification("No changes detected to save.", type = "warning")
        return()
      }
      
      df_upsert <- df_long %>% filter(val_type != "empty")
      df_delete <- df_long %>% filter(val_type == "empty")
      
      n_numeric <- sum(df_upsert$val_type == "numeric")
      n_text    <- sum(df_upsert$val_type == "text")
      n_delete  <- nrow(df_delete)
      message(sprintf(
        "Flowsheet save: %d numeric upsert, %d text upsert, %d cell delete(s).",
        n_numeric, n_text, n_delete
      ))
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      tryCatch({
        DBI::dbExecute(con, "BEGIN")
        
        # ── 1. UPSERT rows that have a value ──────────────────────────────────
        for (i in seq_len(nrow(df_upsert))) {
          if (df_upsert$val_type[i] == "numeric") {
            DBI::dbExecute(con, "
              INSERT INTO labs
                (patient_id, test_date, test_name, num_val, value_text,
                 created_by, updated_by, updated_at)
              VALUES ($1, $2, $3, $4, NULL, $5, $5, NOW())
              ON CONFLICT (patient_id, test_date, test_name)
              DO UPDATE SET
                num_val    = EXCLUDED.num_val,
                value_text = NULL,
                updated_by = EXCLUDED.updated_by,
                updated_at = NOW()",
                           list(pt_id, df_upsert$clean_date[i], df_upsert$Parameter[i],
                                df_upsert$num_val[i], curr_user)
            )
          } else {
            DBI::dbExecute(con, "
              INSERT INTO labs
                (patient_id, test_date, test_name, num_val, value_text,
                 created_by, updated_by, updated_at)
              VALUES ($1, $2, $3, NULL, $4, $5, $5, NOW())
              ON CONFLICT (patient_id, test_date, test_name)
              DO UPDATE SET
                num_val    = NULL,
                value_text = EXCLUDED.value_text,
                updated_by = EXCLUDED.updated_by,
                updated_at = NOW()",
                           list(pt_id, df_upsert$clean_date[i], df_upsert$Parameter[i],
                                df_upsert$value_text[i], curr_user)
            )
          }
        }
        
        # ── 2. DELETE rows whose cells were cleared ───────────────────────────
        # Only fires if a cell previously had data and the user blanked it.
        # A DELETE on a non-existent row is a harmless no-op in PostgreSQL.
        for (i in seq_len(nrow(df_delete))) {
          DBI::dbExecute(con, "
            DELETE FROM labs
            WHERE  patient_id::text = $1
              AND  test_date::date  = $2::date
              AND  test_name        = $3",
                         list(pt_id, df_delete$clean_date[i], df_delete$Parameter[i])
          )
        }
        
        DBI::dbExecute(con, "COMMIT")
        save_count(save_count() + 1)   # increment AFTER successful commit

        msg <- paste0("Flowsheet saved: ", n_numeric, " numeric, ",
                      n_text, " text value(s) written.")
        if (n_delete > 0)
          msg <- paste0(msg, " ", n_delete, " cleared cell(s) removed from DB.")
        showNotification(msg, type = "message", duration = 6)

        # Update the inline "Data saved" banner
        save_msg(paste0("Data saved — ", format(Sys.time(), "%H:%M:%S")))
        refresh_trigger(refresh_trigger() + 1)
        
      }, error = function(e) {
        DBI::dbExecute(con, "ROLLBACK")
        showNotification(paste("Database Error:", e$message), type = "error")
        message("Save flowsheet error: ", e$message)
      })
    })
    
    # NOTE — FIX #6: Removed the dead output$lab_hot_table block that referenced
    # a non-existent lab_data() reactive. It caused startup errors and was
    # never referenced in the UI.
    
    return(list(
      saved = reactive({ save_count() })
    ))
    
  })
}
