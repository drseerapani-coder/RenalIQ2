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
    
    save_trigger <- reactiveVal(0)
    # Track UI mode explicitly: "search" or "editor"
    pt_state <- reactiveValues(view_mode = "search")
    # Local copy of the selected patient — drives the form directly so
    # changes to the global current_pt_out() from other modules don't
    # cause unwanted re-renders or stale-read issues on first selection.
    selected_pt <- reactiveVal(NULL)
    
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
      if (pt_state$view_mode == "editor") {
        return(
          actionButton(ns("back_to_search"), "← Back to Search Results", 
                       class="btn-outline-primary w-100 mb-3 py-2")
        )
      }
      
      card(
        card_header(class="bg-primary text-white", "Patient Search"),
        textInput(ns("reg_q"), NULL, placeholder="Search Name/Phone/UHID...", width = "100%"),
        div(style = "max-height: 400px; overflow-y: auto;",
            DTOutput(ns("reg_table")))
      )
    })
    
    # 3. Profile Editor Logic (The Form View)
    output$profile_panel_ui <- renderUI({
      if (pt_state$view_mode != "editor") return(NULL)

      # Use selected_pt() — a local reactive set at the moment of row selection
      # or "New Patient" click. This avoids the isolate() stale-read on first
      # selection and shields the form from global current_pt_out() updates
      # made by other modules while the user is mid-edit.
      pt_raw <- selected_pt()
      pt <- if (is.null(pt_raw)) list() else as.list(pt_raw)
      
      is_creating <- is.null(pt$id)
      
      card(
        class = "shadow-sm border-0 mt-2",
        card_header(class = "bg-dark text-white d-flex justify-content-between align-items-center", 
                    # Now pt$first_name will safely return NULL instead of crashing
                    if(is_creating || is.null(pt$first_name)) "New Registration" else paste("Edit:", pt$first_name),
                    actionButton(ns("close_profile_top"), icon("times"), class = "btn-sm btn-outline-light")),
        card_body(
          class = "p-3",
          layout_column_wrap(
            width = 1,
            textInput(ns("hospital_number"), "UHID (Hospital Number)", value = pt$hospital_number %||% ""),
            
            layout_column_wrap(
              width = 1/2,
              textInput(ns("first_name"), "First Name *", value = pt$first_name %||% ""),
              textInput(ns("last_name"), "Last Name *", value = pt$last_name %||% "")
            ),
            
            layout_column_wrap(
              width = 1/2,
              dateInput(ns("dob"), "Date of Birth *", 
                        value = if(!is.null(pt$dob)) as.Date(substring(as.character(pt$dob), 1, 10)) else NULL,
                        max = Sys.Date(), width = "100%"),
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
      
      tryCatch({
        DBI::dbGetQuery(pool, glue::glue_sql("
          SELECT * FROM registrations 
          WHERE (first_name ILIKE {query_val} OR last_name ILIKE {query_val} OR phone ILIKE {query_val} OR CAST(hospital_number AS TEXT) ILIKE {query_val})
          ORDER BY updated_at DESC LIMIT 10
        ", .con = pool))
      }, error = function(e) { 
        message("Search Error: ", e$message)
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
    
    # 6. Interaction Handlers
    observeEvent(input$reg_table_rows_selected, {
      s <- input$reg_table_rows_selected
      req(s)
      res <- isolate(search_results())
      req(nrow(res) >= s)

      raw_row <- res[s, , drop = FALSE]
      clean_list <- setNames(lapply(as.list(raw_row), function(x) {
        if (is.list(x) || length(x) > 1) x[[1]] else x
      }), names(raw_row))

      selected_pt(clean_list)      # drives the form immediately — no isolate() needed
      current_pt_out(clean_list)   # updates the global patient for other modules
      pt_state$view_mode <- "editor"
    }, priority = 20)
    
    observeEvent(list(input$back_to_search, input$close_profile, input$close_profile_top), {
      # Action buttons initialise at 0 when first rendered (NULL → 0 counts as
      # a change and would fire this observer immediately, collapsing the form).
      # Only proceed when at least one button has actually been clicked (value > 0).
      btn_clicked <- isTRUE(input$back_to_search > 0) ||
                     isTRUE(input$close_profile > 0) ||
                     isTRUE(input$close_profile_top > 0)
      req(btn_clicked)
      pt_state$view_mode <- "search"
    })
    
    observeEvent(input$go_new_pt, {
      selected_pt(NULL)        # blank form for new registration
      current_pt_out(NULL)
      pt_state$view_mode <- "editor"
    })
    
    # 7. Database Save Logic
    observeEvent(input$save_pt, {
      req(input$first_name, input$dob, user_info())

      clean_dob  <- as.character(input$dob)
      pid        <- selected_pt()$id  # NULL for new patients
      curr_user  <- user_info()$username

      tryCatch({
        if (!is.null(pid)) {
          # --- UPDATE EXISTING PATIENT ---
          DBI::dbExecute(pool,
            "UPDATE registrations
             SET first_name = $1, last_name = $2, dob = $3, gender = $4,
                 phone = $5, address1 = $6, allergies = $7, comments = $8,
                 hospital_number = $9, updated_at = NOW()
             WHERE id = $10",
            list(input$first_name, input$last_name, clean_dob, input$gender,
                 input$phone, input$address1, input$allergies, input$comments,
                 input$hospital_number, as.integer(pid))
          )
          log_audit(pool, curr_user, "UPDATE", "registrations", as.character(pid))
          showNotification("Patient record updated.", type = "message")
        } else {
          # --- INSERT NEW PATIENT ---
          pid <- DBI::dbGetQuery(pool,
            "INSERT INTO registrations
               (first_name, last_name, dob, gender, phone, address1,
                allergies, comments, hospital_number, created_at, updated_at)
             VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, NOW(), NOW())
             RETURNING id",
            list(input$first_name, input$last_name, clean_dob, input$gender,
                 input$phone, input$address1, input$allergies, input$comments,
                 input$hospital_number)
          )$id
          log_audit(pool, curr_user, "CREATE", "registrations", as.character(pid))
          showNotification("New patient registered.", type = "message")
        }

        # Re-fetch the saved row and update both local and global state
        updated <- as.list(DBI::dbGetQuery(pool,
          "SELECT * FROM registrations WHERE id = $1", list(as.integer(pid)))[1, ])
        selected_pt(updated)
        current_pt_out(updated)
        save_trigger(save_trigger() + 1)

      }, error = function(e) {
        showNotification(paste("Save Error:", e$message), type = "error")
        message("Registration Save Error: ", e$message)
      })
    })
    
    return(list(
      saved = reactive({ save_trigger() })
    ))
    
  })
}