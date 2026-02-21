# mod_registration.R

registration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container-fluid p-1",
        actionButton(ns("go_new_pt"), "Create New Patient", 
                     class="btn-success w-100 mb-2", icon = icon("user-plus")),
        
        # Search Card
        card(
          card_header(class="bg-primary text-white d-flex justify-content-between", 
                      "Patient Search", 
                      uiOutput(ns("minimize_btn_ui"))),
          
          textInput(ns("reg_q"), NULL, placeholder="Search Name/Phone/UHID..."),
          
          div(style = "max-height: 300px; overflow-y: auto;",
              DTOutput(ns("reg_table")))
        ),
        
        # Profile Panel
        uiOutput(ns("profile_panel_ui"))
    )
  )
}

registration_server <- function(id, pool, current_pt_out, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Internal state to track if we are in "New Patient" mode
    pt_state <- reactiveValues(creating_new = FALSE)
    
    # --- Search Logic ---
    search_results <- reactive({
      req(input$reg_q)
      q_text <- trimws(input$reg_q)
      if (nchar(q_text) < 2) return(NULL) # Prevent searching on a single character
      
      query_val <- paste0("%", q_text, "%")
      clean_q <- if(exists("clean_phone")) clean_phone(q_text) else q_text
      
      tryCatch({
        actual_cols <- DBI::dbListFields(pool, "registrations")
        true_hosp_col <- actual_cols[grep("hospital_number", actual_cols, ignore.case = TRUE)][1] %||% "hospital_number"
        
        sql <- glue::glue_sql("
          SELECT * FROM registrations
          WHERE (
            first_name ILIKE {query_val}
            OR last_name ILIKE {query_val}
            OR phone ILIKE {query_val}
            OR CAST({`true_hosp_col`} AS TEXT) ILIKE {query_val}
          )
          -- Only attempt exact phone match if input is 10 digits to avoid random matches
          OR (LENGTH({clean_q}) >= 10 AND RIGHT(REGEXP_REPLACE(phone, '[^0-9]', '', 'g'), 10) = {clean_q})
          LIMIT 15
        ", .con = pool)
        
        DBI::dbGetQuery(pool, sql)
      }, error = function(e) {
        message("Search error: ", e$message)
        return(NULL)
      })
    })
    
    output$reg_table <- renderDT({
      df <- search_results()
      req(df, nrow(df) > 0)
      datatable(df, selection = "single", rownames = FALSE,
                options = list(dom = 'tp', scrollX = TRUE,
                               columnDefs = list(list(visible = FALSE, targets = c(0, 8, 13, 14, 15)))))
    })
    
    # --- Profile UI Rendering ---
    output$profile_panel_ui <- renderUI({
      is_creating <- isTRUE(pt_state$creating_new)
      has_selection <- !is.null(current_pt_out())
      
      if (!is_creating && !has_selection) return(NULL)
      pt <- if (has_selection) current_pt_out() else list()
      
      div(class = "mt-2",
          card(
            card_header(class = "bg-dark text-white d-flex justify-content-between align-items-center", 
                        if(is_creating) "New Patient Registration" else paste("Editing Profile:", pt$first_name),
                        actionButton(ns("close_profile"), icon("times"), class = "btn-sm btn-outline-light")),
            card_body(
              textInput(ns("hospital_number"), "UHID (Hospital Number)", value = pt$hospital_number %||% ""),
              textInput(ns("first_name"), "First Name *", value = pt$first_name %||% ""),
              textInput(ns("last_name"), "Last Name *", value = pt$last_name %||% ""),
              # Fix: Ensure date coercion is safe
              dateInput(ns("dob"), "DOB *", value = if(!is.null(pt$dob) && !is.na(pt$dob)) as.Date(pt$dob) else NA),
              radioButtons(ns("gender"), "Gender", choices = c("male", "female"), selected = pt$gender %||% "male", inline = TRUE),
              textInput(ns("phone"), "Phone *", value = pt$phone %||% ""),
              textAreaInput(ns("address1"), "Address", value = pt$address1 %||% "", rows = 2),
              textAreaInput(ns("allergies"), "Allergies", value = pt$allergies %||% "NIL", rows = 1),
              textAreaInput(ns("comments"), "Clinical Comments", value = pt$comments %||% "", rows = 1)
            ),
            card_footer(div(class = "d-grid gap-2",
                            actionButton(ns("save_pt"), "Save Patient Record", class = "btn-success btn-lg"), 
                            actionButton(ns("close_profile"), "Cancel & Close", class = "btn-light")))
          ))
    })
    
    # --- Interaction Handlers ---
    observeEvent(input$go_new_pt, {
      pt_state$creating_new <- TRUE
      current_pt_out(NULL) # Deselect current patient
      shinyjs::runjs("window.scrollTo(0, document.body.scrollHeight);")
    })
    
    observeEvent(input$reg_table_rows_selected, {
      s <- input$reg_table_rows_selected
      req(s)
      
      res <- search_results()
      selected_row <- res[s, , drop = FALSE]
      
      # Convert to list so UI inputs can read elements like pt$first_name
      pt_data_list <- as.list(selected_row)
      
      # Exit "New" mode
      pt_state$creating_new <- FALSE
      
      # Global update: Other modules will see this update automatically
      current_pt_out(pt_data_list)
      
      # Smooth scroll to editor
      shinyjs::runjs("setTimeout(function(){ window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' }); }, 100);")
    })
    
    observeEvent(input$save_pt, {
      # user_info() check ensures the session is still valid
      req(input$first_name, input$phone, user_info()) 
      
      is_new <- isTRUE(pt_state$creating_new)
      current_id <- if(!is_new) current_pt_out()$id else NULL
      curr_user <- user_info()$username
      
      .with_conn(pool, {
        tryCatch({
          if (is_new) {
            # INSERT: includes created_by and returns the new ID for the audit log
            res <- DBI::dbGetQuery(con, glue::glue_sql("
              INSERT INTO registrations (
                hospital_number, first_name, last_name, dob, gender, 
                phone, address1, allergies, comments, created_by
              ) VALUES (
                {input$hospital_number}, {input$first_name}, {input$last_name}, {input$dob}::DATE, 
                {input$gender}, {input$phone}, {input$address1}, {input$allergies}, 
                {input$comments}, {curr_user}
              ) RETURNING id
            ", .con = con))
            
            log_audit(con, curr_user, "CREATE", "registrations", res$id)
            showNotification("Patient Created Successfully", type = "message")
            
            # Reset UI to show the newly created patient
            pt_state$creating_new <- FALSE
            # Optionally reload search results here or just close
            
          } else {
            # UPDATE: includes updated_by and updated_at
            DBI::dbExecute(con, glue::glue_sql("
              UPDATE registrations 
              SET hospital_number = {input$hospital_number},
                  first_name = {input$first_name},
                  last_name = {input$last_name},
                  dob = {input$dob}::DATE,
                  gender = {input$gender},
                  phone = {input$phone},
                  address1 = {input$address1},
                  allergies = {input$allergies},
                  comments = {input$comments},
                  updated_by = {curr_user},
                  updated_at = NOW()
              WHERE id = {current_id}
            ", .con = con))
            
            log_audit(con, curr_user, "UPDATE", "registrations", current_id)
            showNotification("Changes Saved", type = "message")
          }
        }, error = function(e) {
          showNotification(paste("Database Error:", e$message), type = "error")
        })
      })
    })
    
    observeEvent(input$close_profile, {
      pt_state$creating_new <- FALSE
      current_pt_out(NULL)
    })
  })
}