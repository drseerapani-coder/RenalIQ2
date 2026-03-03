# mod_mobile_rx.R
# %||% is defined in helpers.R (sourced before this file in app.R)

mobile_rx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* High-Contrast Card Styling */
      .rx-card { 
        transition: all 0.2s; border-left: 5px solid #f39c12; background: #fff;
        border-radius: 8px !important; margin-bottom: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .rx-card.selected { border-left: 5px solid #2c3e50; background-color: #fff9f0 !important; }
      .rx-brand-name { font-size: 1.15rem; font-weight: 800; color: #1a252f; text-transform: uppercase; }
      .rx-generic-name { font-size: 0.85rem; color: #e67e22; font-weight: 600; font-style: italic; margin-bottom: 4px; }
      .rx-badge { 
        font-weight: 700; font-size: 0.7rem; padding: 4px 8px; border-radius: 4px; 
        background: #fff; border: 1px solid #f39c12; color: #2c3e50; margin-right: 4px; display: inline-block;
      }
      
      /* Grid for 1-12 buttons - 6 columns wide */
      /* Change within tags$style */
     .stitch-grid { 
  display: grid; 
  grid-template-columns: repeat(6, 1fr); 
  gap: 10px; /* Increased from 4px to 10px */
  margin-bottom: 12px; 
  flex-grow: 1; 
}

.btn-stitch { 
  height: 48px; /* Increased height */
  min-width: 42px; 
  font-weight: 800; 
  border-radius: 6px; 
  padding: 5px; 
  display: flex; 
  align-items: center; 
  justify-content: center; 
}
      .override-row { 
        background: #fef5e7; padding: 10px; border-radius: 0 0 8px 8px; 
        border-top: 1px dashed #f39c12; margin-top: 5px;
      }
      .ampm-col { display: flex; flex-direction: column; gap: 4px; min-width: 55px; }
      .label-mini { font-size: 0.75rem; font-weight: bold; color: #7f8c8d; display: block; margin-bottom: 2px; }
    ")),
    uiOutput(ns("rx_main_container"))
  )
}

mobile_rx_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    save_count <- reactiveVal(0)
    
    # --- State ---
    rx_master <- reactiveVal(data.frame())
    rx_meds <- reactiveValues(
      df = data.frame(brand_name=character(), generic=character(), dose=character(), 
                      freq=character(), route=character(), duration=character(),
                      stringsAsFactors=FALSE),
      selected_idx = NULL, s_cat = "OD", s_num = "8", s_ampm = "AM"
    )
    rx_meta        <- reactiveValues(is_history = FALSE, history_date = NULL)
    master_edit_id <- reactiveVal(NULL)   # stable ID for the drug currently being edited
    master_del_id  <- reactiveVal(NULL)   # stable ID for the drug pending deletion
    
    # Update your clean_df function to handle NA specifically
    clean_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(brand_name=character(), generic=character(), dose=character(), 
                          freq=character(), route=character(), duration=character(),
                          stringsAsFactors=FALSE))
      }
      # Force all columns to character and replace NA with empty string
      df[] <- lapply(df, function(x) {
        x <- as.character(x)
        x[is.na(x) | x == "NA"] <- "" 
        return(x)
      })
      return(as.data.frame(df))
    }
    
    # --- Database Operations ---
    load_rx_master <- function() { 
      req(pool)
      data <- dbGetQuery(pool, "SELECT * FROM drug_master ORDER BY brand_name ASC")
      if("id" %in% names(data)) data$id <- as.character(data$id)
      rx_master(clean_df(data))
    }
    observe({ load_rx_master() })
    
    observeEvent(input$add_custom_drug, {
      showModal(modalDialog(
        title = tags$h4(style="color:#e67e22;", icon("database"), " Add Drug to Master List"),
        div(class="row g-2",
            div(class="col-12", textInput(ns("new_brand"), "Brand Name", value = input$rx_q)),
            div(class="col-12", textInput(ns("new_generic"), "Generic Name")),
            div(class="col-6", textInput(ns("new_dose"), "Dose", value = "1 TAB")),
            div(class="col-6", textInput(ns("new_freq"), "Freq", value = "OD8")),
            div(class="col-12", textInput(ns("new_dur"), "Duration", value = "Until next visit"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_new_to_db"), "Save to Master", class="btn-warning fw-bold")
        ), size = "m", easyClose = TRUE
      ))
    })
    
    observeEvent(input$save_new_to_db, {
      req(input$new_brand)
      dbExecute(pool, "INSERT INTO drug_master (brand_name, generic, dose, freq, route, duration) VALUES ($1, $2, $3, $4, $5, $6)", 
                list(input$new_brand, input$new_generic, input$new_dose, input$new_freq, "Oral", input$new_dur))
      load_rx_master()
      new_row <- data.frame(
        brand_name = input$new_brand, 
        generic    = input$new_generic, 
        dose       = input$new_dose, 
        freq       = input$new_freq, 
        route      = "Oral", 
        duration   = input$new_dur, 
        stringsAsFactors = FALSE
      )
      
      # Ensure columns match exactly
      rx_meds$df <- clean_df(dplyr::bind_rows(rx_meds$df, new_row))
      rx_meds$selected_idx <- nrow(rx_meds$df)
      removeModal()
      updateTextInput(session, "rx_q", value="")
    })

    # ---- Master Drug: Edit ----
    # Capture the drug ID into a stable reactiveVal, then open the pre-filled modal.
    # We do NOT rely on input$edit_master_id inside save_master_edit because
    # Shiny.setInputValue inputs may not persist through the modal render cycle.
    observeEvent(input$edit_master_id, {
      drug <- rx_master() %>% dplyr::filter(id == input$edit_master_id)
      req(nrow(drug) > 0)
      d <- drug[1, ]
      master_edit_id(d$id)          # lock in the ID before the modal renders
      showModal(modalDialog(
        title = tags$h4(style = "color:#e67e22;", icon("pencil-alt"), " Edit Drug in Master List"),
        div(class = "row g-2",
            div(class = "col-12", textInput(ns("edit_brand"),   "Brand Name",  value = d$brand_name)),
            div(class = "col-12", textInput(ns("edit_generic"), "Generic Name", value = d$generic)),
            div(class = "col-6",  textInput(ns("edit_dose"),    "Dose",         value = d$dose)),
            div(class = "col-6",  textInput(ns("edit_freq"),    "Freq",         value = d$freq)),
            div(class = "col-6",  textInput(ns("edit_route"),   "Route",        value = d$route)),
            div(class = "col-6",  textInput(ns("edit_dur"),     "Duration",     value = d$duration))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_master_edit"), "Save Changes", class = "btn-warning fw-bold")
        ),
        size = "m", easyClose = TRUE
      ))
      # Force-populate inputs in case they already exist in Shiny state from a
      # previous modal open (textInput value= only sets the DOM, not R's state)
      updateTextInput(session, "edit_brand",   value = d$brand_name)
      updateTextInput(session, "edit_generic", value = d$generic)
      updateTextInput(session, "edit_dose",    value = d$dose)
      updateTextInput(session, "edit_freq",    value = d$freq)
      updateTextInput(session, "edit_route",   value = d$route)
      updateTextInput(session, "edit_dur",     value = d$duration)
    })

    # Persist edits using master_edit_id() — reliably set above
    observeEvent(input$save_master_edit, {
      req(master_edit_id(), input$edit_brand)
      tryCatch({
        dbExecute(pool,
                  "UPDATE drug_master
                      SET brand_name = $1,
                          generic    = $2,
                          dose       = $3,
                          freq       = $4,
                          route      = $5,
                          duration   = $6
                    WHERE id = $7::int",
                  list(trimws(input$edit_brand),
                       trimws(input$edit_generic),
                       trimws(input$edit_dose),
                       trimws(input$edit_freq),
                       trimws(input$edit_route),
                       trimws(input$edit_dur),
                       as.integer(master_edit_id())))
        load_rx_master()
        removeModal()
        showNotification(paste("Updated:", input$edit_brand), type = "message")
      }, error = function(e) {
        showNotification(paste("Error updating drug:", e$message), type = "error")
      })
    })

    # ---- Master Drug: Delete ----
    # Capture the drug ID into a stable reactiveVal, then show confirmation modal.
    observeEvent(input$del_master_id, {
      drug <- rx_master() %>% dplyr::filter(id == input$del_master_id)
      req(nrow(drug) > 0)
      d <- drug[1, ]
      master_del_id(d$id)           # lock in the ID before the modal renders
      showModal(modalDialog(
        title = tags$h4(style = "color:#c0392b;", icon("exclamation-triangle"), " Delete Drug"),
        p("Permanently remove ", tags$strong(d$brand_name), " from the master drug list?"),
        p(class = "text-muted small", "This does not affect any already-saved prescriptions."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_master_del"), "Delete Permanently", class = "btn-danger fw-bold")
        ),
        easyClose = TRUE
      ))
    })

    # Execute delete using master_del_id() — reliably set above
    observeEvent(input$confirm_master_del, {
      req(master_del_id())
      tryCatch({
        dbExecute(pool, "DELETE FROM drug_master WHERE id = $1::int",
                  list(as.integer(master_del_id())))
        load_rx_master()
        removeModal()
        showNotification("Drug removed from master list.", type = "warning")
      }, error = function(e) {
        showNotification(paste("Error deleting drug:", e$message), type = "error")
      })
    })

    # --- Explicit Sync Helper ---
    # Flushes the edit-panel inputs into rx_meds$df for the given row index.
    # Called at safe, deterministic points (card switch + Done) instead of a
    # live observer, which caused cross-contamination: the live observer would
    # fire with the NEW selected_idx but STALE input values from the old card,
    # overwriting the new card's data with the previous card's values.
    sync_edit_to_df <- function(idx) {
      idx <- as.numeric(idx)
      if (is.na(idx) || idx < 1 || idx > nrow(rx_meds$df)) return()
      if (!is.null(input$ov_dose))                         rx_meds$df$dose[idx]     <- input$ov_dose
      if (!is.null(input$ov_freq) && input$ov_freq != "")  rx_meds$df$freq[idx]     <- input$ov_freq
      if (!is.null(input$ov_route))                        rx_meds$df$route[idx]    <- input$ov_route
      if (!is.null(input$ov_dur))                          rx_meds$df$duration[idx] <- input$ov_dur
    }
    
    # Separate observer for the "Stitch Grid" (OD/BD buttons) 
    # so they don't fight with the manual text input
    # Separate observer for the "Stitch Grid" (OD/BD buttons) 
    # observe({
    #   req(rx_meds$selected_idx)
    #   idx <- as.numeric(rx_meds$selected_idx)
    #   
    #   # Safety check: ensure the index exists in the dataframe
    #   req(nrow(rx_meds$df) >= idx)
    #   
    #   if (!is.null(rx_meds$s_cat) && rx_meds$s_cat %in% c("OD", "BD")) {
    #     num <- as.numeric(rx_meds$s_num)
    #     # Handle potential NA in s_num
    #     req(!is.na(num)) 
    #     
    #     hr <- if(rx_meds$s_ampm == "PM" && num < 12) num + 12 else if(rx_meds$s_ampm == "AM" && num == 12) 24 else num
    #     new_freq <- paste0(rx_meds$s_cat, hr)
    #     
    #     # FIX: Use isTRUE() or %in% to handle NA values safely
    #     current_freq <- rx_meds$df$freq[idx]
    #     
    #     if (!isTRUE(current_freq == new_freq)) {
    #       rx_meds$df$freq[idx] <- new_freq
    #       updateTextInput(session, "ov_freq", value = new_freq)
    #     }
    #   }
    # })
    
    # 2. Sync number grid + AM/PM TO the Text Box (all categories)
    observeEvent(list(rx_meds$s_num, rx_meds$s_ampm), {
      req(rx_meds$selected_idx)
      num <- as.numeric(rx_meds$s_num)
      req(!is.na(num))

      hr <- if (rx_meds$s_ampm == "PM" && num < 12) num + 12
            else if (rx_meds$s_ampm == "AM" && num == 12) 24
            else num

      # Map each UI category to the freq code prefix used in freq_list.csv
      prefix <- switch(rx_meds$s_cat,
        "OD"      = "OD",
        "BD"      = "BD",
        "TDS/QID" = "TDS",
        "Days"    = "OD",   # day-based schedules: grid picks time-of-day
        "Misc"    = "OD",
        "OD"
      )
      updateTextInput(session, "ov_freq", value = paste0(prefix, hr))
    })
    
    # 3. Sync Fixed Codes (TDS/QID/etc) TO the Text Box
    observeEvent(input$direct_freq, {
      updateTextInput(session, "ov_freq", value = input$direct_freq)
    })
    # --- UI Logic Builders ---
    render_quick_edit <- function() {
      idx <- as.numeric(rx_meds$selected_idx)
      #curr <- rx_meds$df[idx, ]
      curr <- isolate(rx_meds$df[idx, ])
      div(class="card border-warning mb-2",
          div(class="card-header bg-warning py-1 d-flex justify-content-between",
              tags$strong("Set Frequency"),
              actionButton(ns("close_edit"), "Done", class="btn btn-xs btn-dark")),
          div(class="card-body p-2",
              div(class="d-flex flex-wrap gap-1 mb-2",
                  lapply(c("OD", "BD", "TDS/QID", "Days", "Misc"), function(cat) {
                    actionButton(ns(paste0("scat_", cat)), cat, 
                                 class = if(rx_meds$s_cat == cat) "btn btn-xs btn-dark" else "btn btn-xs btn-outline-warning", 
                                 onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_cat"), cat))
                  })),
              div(class="p-2 border rounded bg-white",
                  # For TDS/QID, Days, Misc: show preset code buttons above the grid
                  if (!rx_meds$s_cat %in% c("OD", "BD")) {
                    codes <- switch(rx_meds$s_cat,
                      "TDS/QID" = c("TDS","QID","PRN","STAT"),
                      "Days"    = c("MWF","TTS","Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                      "Misc"    = c("ALT","Weekly","RTX","hepb")
                    )
                    div(class="d-flex flex-wrap gap-1 mb-2",
                        lapply(codes, function(c) {
                          actionButton(ns(paste0("fc_", c)), c,
                                       class   = "btn btn-sm btn-outline-warning",
                                       onclick = sprintf("Shiny.setInputValue('%s','%s')", ns("direct_freq"), c))
                        }))
                  },
                  # Number grid + AM/PM shown for ALL categories
                  div(class="d-flex gap-2",
                      div(class="stitch-grid", lapply(1:12, function(n) {
                        actionButton(ns(paste0("sn_", n)), as.character(n),
                                     class   = paste("btn btn-stitch",
                                                     if (rx_meds$s_num == as.character(n)) "btn-warning" else "btn-outline-warning"),
                                     onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_num"), n))
                      })),
                      div(class="ampm-col",
                          actionButton(ns("s_am"), "AM",
                                       class   = if (rx_meds$s_ampm == "AM") "btn btn-xs btn-dark" else "btn btn-xs btn-outline-dark",
                                       onclick = sprintf("Shiny.setInputValue('%s','AM')", ns("change_ampm"))),
                          actionButton(ns("s_pm"), "PM",
                                       class   = if (rx_meds$s_ampm == "PM") "btn btn-xs btn-dark" else "btn btn-xs btn-outline-dark",
                                       onclick = sprintf("Shiny.setInputValue('%s','PM')", ns("change_ampm")))
                      )
                  )
              )),
          div(class="override-row",
              div(class="row g-1",
                  div(class="col-3", span(class="label-mini", "Dose"), textInput(ns("ov_dose"), NULL, value=curr$dose)),
                  div(class="col-3", span(class="label-mini", "Freq"), textInput(ns("ov_freq"), NULL, value=curr$freq)),
                  div(class="col-3", span(class="label-mini", "Route"), textInput(ns("ov_route"), NULL, value=curr$route)),
                  div(class="col-3", span(class="label-mini", "Dur"), textInput(ns("ov_dur"), NULL, value=curr$duration))
              )
          ))
    }
    
    # --- Observers ---
    # --- Robust Observers ---
    observeEvent(input$change_cat, { rx_meds$s_cat <- input$change_cat })
    observeEvent(input$change_num, { rx_meds$s_num <- input$change_num })
    observeEvent(input$change_ampm, { rx_meds$s_ampm <- input$change_ampm })
    
    # Handle clicks on TDS, QID, PRN, MWF, etc.
    observeEvent(input$direct_freq, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      
      # 1. Update the master data frame
      rx_meds$df$freq[idx] <- input$direct_freq
      
      # 2. Crucial: Force the text box to show the new value
      updateTextInput(session, "ov_freq", value = input$direct_freq)
    })
    
    # Sync current card's inputs into df BEFORE closing the edit panel (Done button)
    observeEvent(input$close_edit, {
      if (!is.null(rx_meds$selected_idx)) sync_edit_to_df(rx_meds$selected_idx)
      rx_meds$selected_idx <- NULL
    })

    # Sync PREVIOUS card's inputs into df BEFORE switching selection to the new card.
    # At the moment this observer fires, selected_idx is still the old card's index
    # and the input$ov_* values are still the old card's values — safe to flush.
    observeEvent(input$select_rx_card, {
      if (!is.null(rx_meds$selected_idx)) sync_edit_to_df(rx_meds$selected_idx)
      rx_meds$selected_idx <- input$select_rx_card
    })
    observeEvent(input$del_rx_idx, {
      # 1. Capture the index to delete
      idx_to_remove <- as.numeric(input$del_rx_idx)
      req(idx_to_remove)
      
      # 2. Use isolate to prevent the UI from flickering mid-process
      isolate({
        if (nrow(rx_meds$df) >= idx_to_remove) {
          # Clear selection first to prevent "operator invalid for atomic vectors" 
          # errors if the UI tries to render an edit box for a deleted row
          rx_meds$selected_idx <- NULL 
          
          # Remove the row
          rx_meds$df <- rx_meds$df[-idx_to_remove, ]
          
          showNotification("Medication removed", type = "warning")
        }
      })
    })
    
    # --- Rendering ---
    output$rx_search_results <- renderUI({
      req(input$rx_q)
      if(nchar(input$rx_q) < 2) return(NULL)
      
      # 1. Use explicit dplyr:: filter for the master list
      matches <- rx_master() %>% 
        dplyr::filter(grepl(input$rx_q, brand_name, ignore.case = TRUE)) %>% 
        head(5)
      
      if(nrow(matches) == 0) {
        return(actionButton(ns("add_custom_drug"), paste("Add New:", input$rx_q), 
                            class="btn btn-sm btn-warning w-100 mt-2"))
      }
      
      lapply(1:nrow(matches), function(i) {
        d <- matches[i,]

        meta_vals <- c(d$dose, d$freq, d$route, d$duration)
        meta_vals <- meta_vals[meta_vals != "" & !is.na(meta_vals)]
        meta_sub  <- paste(meta_vals, collapse = " | ")

        # Row: [Add button (flex-grow)] [Edit icon] [Delete icon]
        div(class = "d-flex align-items-stretch mb-1 gap-1",
            tags$button(
              class   = "btn btn-sm btn-outline-secondary flex-grow-1 text-start",
              onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("add_rx_id"), d$id),
              HTML(paste0(
                "<div class='rx-brand-name' style='font-size:0.9rem;'>", d$brand_name, "</div>",
                "<div style='font-size:0.75rem;color:#e67e22;font-weight:600;'>", meta_sub, "</div>"
              ))
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-warning",
              title   = "Edit in master list",
              onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("edit_master_id"), d$id),
              icon("pencil-alt")
            ),
            tags$button(
              class   = "btn btn-sm btn-outline-danger",
              title   = "Delete from master list",
              onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("del_master_id"), d$id),
              icon("trash")
            )
        )
      })
    })
    
    observeEvent(input$add_rx_id, {
      lib <- rx_master() %>% dplyr::filter(id == input$add_rx_id)
      req(nrow(lib) > 0)
      
      # Ensure NO NAs exist right at the moment of creation
      new_row <- data.frame(
        brand_name = as.character(lib$brand_name[1] %||% ""),
        generic    = as.character(lib$generic[1] %||% ""),
        dose       = as.character(lib$dose[1] %||% "1 TAB"), # Default if missing
        freq       = as.character(lib$freq[1] %||% "OD"),
        route      = as.character(lib$route[1] %||% "Oral"), 
        duration   = as.character(lib$duration[1] %||% "Until next visit"),
        stringsAsFactors = FALSE
      )
      
      # Use isolate to prevent the Sync Observer from firing mid-addition
      isolate({
        current_df <- rx_meds$df
        rx_meds$df <- clean_df(dplyr::bind_rows(current_df, new_row))
        rx_meds$selected_idx <- nrow(rx_meds$df)
      })
      
      updateTextInput(session, "rx_q", value="")
    })
    
    output$rx_list <- renderUI({
      df <- rx_meds$df; if(nrow(df) == 0) return(NULL)
      lapply(1:nrow(df), function(i) {
        sel <- !is.null(rx_meds$selected_idx) && rx_meds$selected_idx == i
        tagList(
          div(class = paste("rx-card p-3", if(sel) "selected"), onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("select_rx_card"), i),
              div(class="d-flex justify-content-between align-items-start",
                  div(span(class="rx-brand-name", df$brand_name[i]), div(class="rx-generic-name", df$generic[i]),
                      div(class="mt-1", 
                          span(class="rx-badge", icon("pills"), df$dose[i]), # Added Dose
                          span(class="rx-badge", icon("clock"), df$freq[i]), 
                          span(class="rx-badge", icon("map-marker-alt"), df$route[i]))),
                  actionButton(ns(paste0("del_", i)), NULL, icon("trash"), class="btn btn-sm btn-outline-danger border-0", 
                               onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', %d, {priority:'event'})", ns("del_rx_idx"), i)))),
          if(sel) render_quick_edit())
      })
    })
    
    output$rx_main_container <- renderUI({
      req(current_pt())
      div(div(class="bg-dark text-white p-2 d-flex justify-content-between", span(icon("file-medical"), " PRESCRIPTION"), if(rx_meta$is_history) span(class="badge bg-warning text-dark", rx_meta$history_date)),
          div(class="p-2", textInput(ns("rx_q"), NULL, placeholder = "Search..."), uiOutput(ns("rx_search_results")), hr(class="my-2"), uiOutput(ns("rx_list")),
              actionButton(ns("save_rx"), "SAVE & CLOSE", class="btn-warning w-100 fw-bold mt-2")))
    })
    
    # --- Persistence ---
    observeEvent(current_pt(), {
      req(current_pt()$id, pool)
      # Force visit_date to DATE to strip time, ensuring we get today's record
      res <- dbGetQuery(pool, 
                        "SELECT meds_json, visit_date FROM prescriptions 
         WHERE patient_id::text = $1::text 
         AND visit_date IS NOT NULL
         ORDER BY visit_date DESC NULLS LAST LIMIT 1", 
                        list(as.character(current_pt()$id)))
      
      if (nrow(res) > 0) {
        rx_meta$history_date <- format(as.Date(res$visit_date[1]), "%Y-%m-%d")
        rx_meta$is_history <- TRUE
        # Use clean_df here to handle the conversion from JSON
        rx_meds$df <- clean_df(as.data.frame(jsonlite::fromJSON(res$meds_json[1])))
      } else { 
        rx_meta$is_history <- FALSE
        rx_meds$df <- rx_meds$df[0,] 
      }
    })
    
    observeEvent(input$save_rx, {
      req(current_pt(), current_pt()$id)
      
      # Final Sync
      if (!is.null(rx_meds$selected_idx)) {
        idx <- as.numeric(rx_meds$selected_idx)
        if (idx > 0 && idx <= nrow(rx_meds$df)) {
          if(!is.null(input$ov_dose))  rx_meds$df$dose[idx]     <- input$ov_dose
          if(!is.null(input$ov_freq))  rx_meds$df$freq[idx]     <- input$ov_freq
          if(!is.null(input$ov_route)) rx_meds$df$route[idx]    <- input$ov_route
          if(!is.null(input$ov_dur))   rx_meds$df$duration[idx] <- input$ov_dur
        }
      }
      
      p_id <- as.character(current_pt()$id)
      final_df <- clean_df(rx_meds$df)
      
      # CRITICAL: Force to character string to ensure DB accepts it as TEXT/JSON
      json_text <- as.character(jsonlite::toJSON(final_df, auto_unbox = TRUE))
      
      # DEBUG: Check console to see if Thyronorm is in this string!
      #message("Saving JSON: ", json_text) 
      
      tryCatch({
        # We force the date to a plain DATE type to ensure the conflict triggers correctly
        dbExecute(pool, 
                  "INSERT INTO prescriptions (patient_id, meds_json, visit_date) 
         VALUES ($1, $2, CURRENT_DATE) 
         ON CONFLICT (patient_id, visit_date) 
         DO UPDATE SET meds_json = EXCLUDED.meds_json", 
                  list(p_id, json_text) # Note: Only 2 parameters now, SQL handles the date
        )
        
        rx_meds$selected_idx <- NULL
        save_count(save_count() + 1)
        showNotification("Prescription Saved Successfully", type = "message")
        
      }, error = function(e) {
        message("Database Save Error: ", e$message)
        showNotification(paste("Save Failed:", e$message), type = "error")
      })
    })
    
    return(list(
      saved = reactive({ save_count() }) 
    ))
  })
}