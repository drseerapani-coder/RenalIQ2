# --- UI Function ---
registration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container-fluid p-2",
        # Top Action Row
        div(class = "d-flex gap-2 mb-3",
            actionButton(ns("go_new_pt"), "New Patient", 
                         class="btn-success flex-grow-1 py-3", icon = icon("user-plus")),
            uiOutput(ns("clear_search_ui"))
        ),
        
        # Search Section (Conditional Visibility)
        uiOutput(ns("search_section_ui")),
        
        # Profile Panel (The Editor)
        uiOutput(ns("profile_panel_ui"))
    )
  )
}

# --- Server Function ---
registration_server <- function(id, pool, current_pt_out, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track UI mode explicitly: "search" or "editor"
    # This prevents the flicker when global patient state changes
    pt_state <- reactiveValues(view_mode = "search")
    
    # 1. Clear Search Button Logic
    output$clear_search_ui <- renderUI({
      req(input$reg_q)
      if (nchar(input$reg_q) > 0) {
        actionButton(ns("clear_q"), "Clear", class="btn-outline-secondary py-3")
      }
    })
    
    observeEvent(input$clear_q, {
      updateTextInput(session, "reg_q", value = "")
    })
    
    # 2. Search Section Logic (The Table View)
    output$search_section_ui <- renderUI({
      # Explicitly check if we are in editor mode
      if (pt_state$view_mode == "editor") {
        return(
          actionButton(ns("back_to_search"), "← Back to Search Results", 
                       class="btn-outline-primary w-100 mb-3 py-2")
        )
      }
      
      card(
        card_header(class="bg-primary text-white", "Patient Search"),
        # Adding a class 'search-box-focus' to ensure JS can find it
        textInput(ns("reg_q"), NULL, placeholder="Search Name/Phone/UHID...", width = "100%"),
        div(style = "max-height: 400px; overflow-y: auto;",
            DTOutput(ns("reg_table")))
      )
    })
    
    # 3. Profile Editor Logic (The Form View)
    output$profile_panel_ui <- renderUI({
      # Don't show the form if we are in search mode
      if (pt_state$view_mode != "editor") return(NULL)
      
      # Prepare data for inputs
      pt <- isolate(current_pt_out()) %||% list()
      is_creating <- is.null(pt$id)
      
      card(
        class = "shadow-sm border-0 mt-2",
        card_header(class = "bg-dark text-white d-flex justify-content-between align-items-center", 
                    if(is_creating) "New Registration" else paste("Edit:", pt$first_name),
                    actionButton(ns("close_profile_top"), icon("times"), class = "btn-sm btn-outline-light")),
        card_body(
          class = "p-3",
          layout_column_wrap(
            width = 1, # Vertical stack for mobile
            textInput(ns("hospital_number"), "UHID (Hospital Number)", value = pt$hospital_number %||% ""),
            
            layout_column_wrap(
              width = 1/2,
              textInput(ns("first_name"), "First Name *", value = pt$first_name %||% ""),
              textInput(ns("last_name"), "Last Name *", value = pt$last_name %||% "")
            ),
            
            layout_column_wrap(
              width = 1/2,
              dateInput(ns("dob"), "Date of Birth *", 
                        value = if(!is.null(pt$dob)) {
                          # Convert to character first, then to Date. 
                          # This strips timezones/timestamps that cause the coercion warning.
                          as.Date(substring(as.character(pt$dob), 1, 10))
                        } else {
                          NULL
                        },
                        max = Sys.Date(), 
                        width = "100%"),
              radioButtons(ns("gender"), "Gender", choices = c("male", "female"), 
                           selected = pt$gender %||% "male", inline = TRUE)
            ),
            
            textInput(ns("phone"), "Phone Number *", value = pt$phone %||% ""),
            textAreaInput(ns("address1"), "Address", value = pt$address1 %||% "", rows = 2),
            textAreaInput(ns("allergies"), "Allergies", value = pt$allergies %||% "NIL", rows = 1),
            textAreaInput(ns("comments"), "Clinical Comments", value = pt$comments %||% "", rows = 1)
          )
        ),
        card_footer(
          div(class = "d-grid gap-2",
              actionButton(ns("save_pt"), "Save Record", class = "btn-success btn-lg py-3"), 
              actionButton(ns("close_profile"), "Cancel", class = "btn-light py-2"))
        )
      )
    })
    
    # 4. Database Search Logic
    search_results <- reactive({
      req(input$reg_q)
      q_text <- trimws(input$reg_q)
      if (nchar(q_text) < 2) return(NULL)
      
      query_val <- paste0("%", q_text, "%")
      clean_q <- if(exists("clean_phone")) clean_phone(q_text) else q_text
      
      tryCatch({
        actual_cols <- DBI::dbListFields(pool, "registrations")
        true_hosp_col <- actual_cols[grep("hospital_number", actual_cols, ignore.case = TRUE)][1] %||% "hospital_number"
        
        sql <- glue::glue_sql("
          SELECT * FROM registrations 
          WHERE (first_name ILIKE {query_val} OR last_name ILIKE {query_val} OR phone ILIKE {query_val} OR CAST({`true_hosp_col`} AS TEXT) ILIKE {query_val})
          OR (LENGTH({clean_q}) >= 10 AND RIGHT(REGEXP_REPLACE(phone, '[^0-9]', '', 'g'), 10) = {clean_q})
          ORDER BY updated_at DESC LIMIT 10
        ", .con = pool)
        DBI::dbGetQuery(pool, sql)
      }, error = function(e) { 
        message("DB Search Error: ", e$message)
        NULL 
      })
    })
    
    # 5. Render Data Table
    output$reg_table <- renderDT({
      df <- search_results()
      req(df, nrow(df) > 0)
      datatable(df, selection = "single", rownames = FALSE,
                options = list(dom = 'tp', scrollX = TRUE, pageLength = 5,
                               columnDefs = list(list(visible = FALSE, targets = c(0, 8, 13, 14, 15)))))
    })
    
    # 6. Interaction Handlers (Selection & Transitions)
    
    observeEvent(input$reg_table_rows_selected, {
      s <- input$reg_table_rows_selected
      req(s)
      
      # Use isolate to ensure we don't trigger a recursive loop
      res <- isolate(search_results())
      req(nrow(res) >= s)
      
      # Process row
      raw_row <- res[s, , drop = FALSE]
      clean_list <- setNames(lapply(as.list(raw_row), function(x) {
        if (is.list(x) || length(x) > 1) x[[1]] else x
      }), names(raw_row))
      
      # LOGIC: Force the state change
      # We update the global value first
      current_pt_out(clean_list)
      
      # Then we force the UI to switch mode
      pt_state$view_mode <- "editor"
      
      message("DEBUG: Row selected. Mode switched to Editor.")
    }, priority = 20) # High priority ensures this runs before the table re-renders
    
    # Back/Close logic
    observeEvent(list(input$back_to_search, input$close_profile, input$close_profile_top), {
      pt_state$view_mode <- "search"
    })
    
    # New Patient logic
    observeEvent(input$go_new_pt, { 
      current_pt_out(NULL) # Clear selection for new entry
      pt_state$view_mode <- "editor"
    })
    
    # 7. Database Save Logic
    observeEvent(input$save_pt, {
      req(input$first_name, input$phone, user_info()) 
      pt <- isolate(current_pt_out())
      is_new <- is.null(pt$id)
      curr_user <- user_info()$username
      
      .with_conn(pool, {
        tryCatch({
          if (is_new) {
            # INSERT NEW
            res <- DBI::dbGetQuery(con, glue::glue_sql("
              INSERT INTO registrations (hospital_number, first_name, last_name, dob, gender, phone, address1, allergies, comments, created_by)
              VALUES ({input$hospital_number}, {input$first_name}, {input$last_name}, {input$dob}::DATE, {input$gender}, {input$phone}, {input$address1}, {input$allergies}, {input$comments}, {curr_user})
              RETURNING id
            ", .con = con))
            target_id <- res$id
            log_audit(con, curr_user, "CREATE", "registrations", target_id)
          } else {
            # UPDATE EXISTING
            target_id <- pt$id
            DBI::dbExecute(con, glue::glue_sql("
              UPDATE registrations SET 
                hospital_number={input$hospital_number}, first_name={input$first_name}, last_name={input$last_name}, 
                dob={input$dob}::DATE, gender={input$gender}, phone={input$phone}, address1={input$address1}, 
                allergies={input$allergies}, comments={input$comments}, 
                updated_by={curr_user}, updated_at=NOW()
              WHERE id = {target_id}
            ", .con = con))
            log_audit(con, curr_user, "UPDATE", "registrations", target_id)
          }
          
          # Force refresh of the global patient object to reflect new data
          updated_df <- DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM registrations WHERE id = {target_id}", .con = con))
          updated_list <- setNames(lapply(as.list(updated_df[1,,drop=F]), function(x) x[[1]]), names(updated_df))
          
          current_pt_out(updated_list)
          pt_state$view_mode <- "search" # Exit editor
          showNotification("Record Saved Successfully", type = "message")
          
        }, error = function(e) { 
          showNotification(paste("Database Error:", e$message), type = "error") 
        })
      })
    })
  })
}