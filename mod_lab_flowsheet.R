# mod_lab_flowsheet.R
library(rhandsontable)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(bslib)

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
        div(class = "d-flex justify-content-between align-items-center",
            span("Clinical Lab Flowsheet"),
            div(
              actionButton(ns("refresh_data"), "Refresh", class = "btn-outline-primary me-2", icon = icon("sync")),
              actionButton(ns("open_add_lab"), "Add New Date", class = "btn-success me-2", icon = icon("plus")),
              actionButton(ns("save_flowsheet"), "Save All Changes", class = "btn-primary", icon = icon("save"))
            ))
      ),
      card_body(
        helpText("Right-click any date column header or cell to remove that date's entire record."),
        rHandsontableOutput(ns("history_table")) %>% withSpinner(color="#26A69A")
      )
    )
  )
}

lab_flowsheet_server <- function(id, pool, current_pt, lab_targets_raw,parent_nav, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    
    # Inside lab_flowsheet_server
    observeEvent(input$refresh_data, {
      refresh_trigger(refresh_trigger() + 1)
      showNotification("Flowsheet updated from database.", type = "message")
    })
    
    observeEvent(parent_nav(), {
      # Adjust the string below to match the EXACT title of your nav_panel in app.R
      if (parent_nav() == "4. Labs Flowsheet") {
        refresh_trigger(refresh_trigger() + 1)
      }
    })
    
    # 1. RENDER FLOWSHEET
    output$history_table <- renderRHandsontable({
      req(current_pt())
      refresh_trigger() 
      
      query <- "SELECT test_name, test_date, num_val, value_text FROM labs WHERE patient_id::text = $1 ORDER BY test_date DESC"
      res <- dbGetQuery(pool, query, list(as.character(current_pt()$id)))
      
      df_base <- data.frame(test_name = lab_targets_raw$test_name, stringsAsFactors = FALSE)
      
      if(nrow(res) > 0) {
        res$display_val <- ifelse(!is.na(res$num_val), as.character(res$num_val), res$value_text)
        res$fmt_date <- format(as.Date(res$test_date), "%d-%b-%y")
        df_wide <- res %>% 
          select(test_name, fmt_date, display_val) %>% 
          pivot_wider(names_from = fmt_date, values_from = display_val)
        df_final <- left_join(df_base, df_wide, by = "test_name")
      } else {
        df_final <- df_base
      }
      
      colnames(df_final)[1] <- "Parameter"
      
      # useTypes = FALSE allows the custom context menu to trigger column events
      rhandsontable(df_final, height = 500, width = "100%", stretchH = "none", useTypes = FALSE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(fixedColumnsLeft = 1) %>% 
        hot_col("Parameter", width = 200, readOnly = TRUE) %>%
        hot_cols(colWidths = 100) %>%
        hot_context_menu(
          allowRowEdit = FALSE, 
          allowColEdit = TRUE,
          customOpts = list(
            delete_column = list(
              name = "Delete this entire date column", 
              callback = htmlwidgets::JS("function (key, options) { 
                var sel = this.getSelected();
                if(sel && sel.length > 0) { 
                  // sel is [row1, col1, row2, col2]
                  Shiny.setInputValue('lab_mod-delete_request', sel[0], {priority: 'event'}); 
                }
              }")
            )
          )
        )
    })
    
    # 2. HANDLE DELETE REQUEST
    observeEvent(input$delete_request, {
      # The JS now sends just the selection array [r1, c1, r2, c2]
      sel <- input$delete_request
      
      # Column index is the 2nd element (index 1 in 0-based JS, index 2 in 1-based R)
      # col_idx = sel[2] + 1
      col_idx <- as.numeric(sel[2]) + 1
      
      if (is.na(col_idx) || col_idx <= 1) {
        showNotification("Action Denied: The Parameter column is required.", type = "error")
        return()
      }
      
      # Find which date corresponds to this column index
      date_res <- dbGetQuery(pool, 
                             "SELECT DISTINCT test_date FROM labs WHERE patient_id::text = $1 ORDER BY test_date DESC", 
                             list(as.character(current_pt()$id)))
      
      if(nrow(date_res) < (col_idx - 1)) {
        showNotification("Could not identify the date for this column.", type = "warning")
        return()
      }
      
      target_date <- date_res$test_date[col_idx - 1]
      fmt_date <- format(as.Date(target_date), "%d-%b-%y")
      
      showModal(modalDialog(
        title = "Confirm Column Deletion",
        div(style="text-align: center; padding: 20px;",
            icon("exclamation-triangle", class="text-danger", style="font-size: 3rem;"),
            h4("Delete Lab Records?", style="margin-top: 15px;"),
            p("You are about to permanently delete all lab results for:"),
            h3(fmt_date, style="color: #d9534f;")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_right_click_delete"), "Confirm Permanent Delete", class = "btn-danger")
        )
      ))
      
      # Final deletion execution
      observeEvent(input$confirm_right_click_delete, {
        dbExecute(pool, "DELETE FROM labs WHERE patient_id::text = $1 AND test_date = $2",
                  list(as.character(current_pt()$id), as.character(target_date)))
        
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
        showNotification(paste("Successfully removed labs for", fmt_date), type = "message")
      }, once = TRUE)
    })
    
    # 3. LAB ENTRY MODAL (REMAINS SAME)
    observeEvent(input$open_add_lab, {
      showModal(modalDialog(
        title = "Manual Lab Entry", size = "l", easyClose = FALSE,
        div(class = "d-flex justify-content-between align-items-center mb-3 p-2 border-bottom",
            div(class = "d-flex align-items-center", dateInput(ns("modal_lab_date"), "Test Date:", value = Sys.Date(), width = "150px")),
            div(actionButton(ns("cancel_top"), "Cancel", class = "btn-outline-secondary me-2"),
                actionButton(ns("save_labs_modal"), "Save Records", class = "btn-success", icon = icon("check")))
        ),
        div(class = "mb-2",
            actionButton(ns("expand_all"), "Expand All", class = "btn-sm btn-link"),
            actionButton(ns("collapse_all"), "Collapse All", class = "btn-sm btn-link")
        ),
        accordion(
          id = ns("lab_accordion"), open = FALSE, 
          lapply(unique(lab_targets_raw$category), function(cat) {
            accordion_panel(title = cat, value = cat, 
                            layout_column_wrap(width = 1/3,
                                               lapply(lab_targets_raw$test_name[lab_targets_raw$category == cat], function(t) {
                                                 numericInput(ns(paste0("lab_", make.names(t))), t, value = NA)
                                               })
                            )
            )
          })
        ), footer = NULL 
      ))
    })
    
    observeEvent(input$expand_all, { accordion_panel_open("lab_accordion", values = unique(lab_targets_raw$category)) })
    observeEvent(input$collapse_all, { accordion_panel_close("lab_accordion", values = unique(lab_targets_raw$category)) })
    observeEvent(input$cancel_top, { removeModal() })
    
    # Inside lab_flowsheet_server
    # ---------------------------------------------------------
    # HANDLER: Save All Changes from the Flowsheet (Bulk Update)
    # ---------------------------------------------------------
    # ---------------------------------------------------------
    # HANDLER: Save All Changes from the Flowsheet (Bulk Update)
    # ---------------------------------------------------------
    observeEvent(input$save_flowsheet, {
      req(current_pt(), user_info(), input$history_table)
      
      curr_user <- user_info()$username
      pt_id <- as.character(current_pt()$id)
      
      # 1. Convert rhandsontable back to R dataframe
      df <- hot_to_r(input$history_table)
      
      # 2. Reshape data back to long format 
      # FIX: Changed 'Test' to 'Parameter' to match your renderRHandsontable rename
      df_long <- df %>%
        pivot_longer(
          cols = -Parameter, 
          names_to = "test_date_str", 
          values_to = "num_val"
        ) %>%
        filter(!is.na(num_val))
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      tryCatch({
        DBI::dbExecute(con, "BEGIN")
        
        log_audit(con, curr_user, "BULK_UPDATE_START", "labs", pt_id)
        
        for (i in seq_len(nrow(df_long))) {
          # FIX: Convert the "dd-Mon-yy" column header back to "YYYY-MM-DD" for Postgres
          clean_date <- as.Date(df_long$test_date_str[i], format = "%d-%b-%y")
          
          DBI::dbExecute(con, "
            UPDATE labs 
            SET num_val = $1, updated_by = $2, updated_at = NOW() 
            WHERE patient_id = $3 AND test_name = $4 AND test_date = $5",
                         list(
                           as.numeric(df_long$num_val[i]), 
                           curr_user, 
                           pt_id, 
                           df_long$Parameter[i], 
                           as.character(clean_date)
                         )
          )
        }
        
        log_audit(con, curr_user, "BULK_UPDATE_COMPLETE", "labs", pt_id)
        DBI::dbExecute(con, "COMMIT")
        
        showNotification("All lab changes saved and logged.", type = "message")
        refresh_trigger(refresh_trigger() + 1) # Refresh to sync UI with DB
        
      }, error = function(e) {
        DBI::dbExecute(con, "ROLLBACK")
        showNotification(paste("Flowsheet Save Error:", e$message), type = "error")
      })
    })
    
    output$lab_hot_table <- renderRHandsontable({
      df <- lab_data()
      req(df)
      
      rhandsontable(df, 
                    useTypes = FALSE, # This stops the 'column add/delete' warning
                    stretchH = "all",
                    rowHeaders = NULL) %>%
        hot_col("Parameter", width = 200, readOnly = TRUE, type = "text") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) # Better for mobile
    })
    
    # ---------------------------------------------------------
    # HANDLER: Add New Date (Modal Insert)
    # ---------------------------------------------------------
    observeEvent(input$save_labs_modal, {
      req(current_pt(), input$modal_lab_date, user_info())
      
      curr_user <- user_info()$username
      pt_id <- as.character(current_pt()$id)
      v_date <- as.character(input$modal_lab_date)
      
      # Use the manual connection checkout for better stability
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      tryCatch({
        DBI::dbExecute(con, "BEGIN")
        
        for (t in lab_targets_raw$test_name) {
          val <- input[[paste0("lab_", make.names(t))]]
          
          if (!is.na(val)) {
            # THE UPSERT LOGIC: Handles duplicates gracefully
            DBI::dbExecute(con, "
              INSERT INTO labs (patient_id, test_date, test_name, num_val, created_by) 
              VALUES ($1, $2, $3, $4, $5)
              ON CONFLICT (patient_id, test_date, test_name) 
              DO UPDATE SET 
                num_val = EXCLUDED.num_val,
                updated_by = $5,
                updated_at = NOW()",
                           list(pt_id, v_date, t, val, curr_user)
            )
          }
        }
        
        log_audit(con, curr_user, "UPSERT_LAB_ENTRY", "labs", paste0(pt_id, "_", v_date))
        DBI::dbExecute(con, "COMMIT")
        
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
        showNotification("Labs saved (existing records were updated).", type = "message")
        
      }, error = function(e) {
        DBI::dbExecute(con, "ROLLBACK")
        showNotification(paste("Save Error:", e$message), type = "error")
      })
    })
    
  })
}