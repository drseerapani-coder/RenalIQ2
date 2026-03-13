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
        DBI::dbGetQuery(pool,
          "SELECT * FROM registrations
           WHERE (first_name ILIKE $1 OR last_name ILIKE $1 OR phone ILIKE $1
                  OR CAST(hospital_number AS TEXT) ILIKE $1)
           ORDER BY updated_at DESC LIMIT 10",
          list(query_val))
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
      
      current_pt_out(clean_list)
      pt_state$view_mode <- "editor"
    }, priority = 20)
    
    observeEvent(list(input$back_to_search, input$close_profile, input$close_profile_top), {
      # Guard: only reset when a button was actually clicked (value > 0).
      # Without this, the observer fires when close_profile / close_profile_top
      # are first rendered (NULL → 0), causing the profile to disappear immediately.
      clicked <- isTRUE(input$back_to_search   > 0) ||
                 isTRUE(input$close_profile     > 0) ||
                 isTRUE(input$close_profile_top > 0)
      if (!clicked) return()
      pt_state$view_mode <- "search"
    })
    
    observeEvent(input$go_new_pt, { 
      current_pt_out(NULL)
      pt_state$view_mode <- "editor"
    })
    
    # 7. Database Save Logic (Adapted to bypass .with_conn scope issue)
    observeEvent(input$save_pt, {
      req(input$first_name, input$phone, user_info()) 
      pt <- isolate(current_pt_out())
      is_new <- is.null(pt$id)
      curr_user <- user_info()$username
      
      # Step A: Checkout connection manually
      con <- pool::poolCheckout(pool)
      
      # Step B: Ensure it's returned no matter what happens
      on.exit(pool::poolReturn(con), add = TRUE)
      
      tryCatch({
        if (is_new) {
          # INSERT NEW
          res <- DBI::dbGetQuery(con,
            "INSERT INTO registrations (hospital_number, first_name, last_name, dob, gender, phone, address1, allergies, comments, created_by)
             VALUES ($1, $2, $3, $4::DATE, $5, $6, $7, $8, $9, $10)
             RETURNING id",
            list(input$hospital_number, input$first_name, input$last_name,
                 as.character(input$dob), input$gender, input$phone,
                 input$address1, input$allergies, input$comments, curr_user))
          target_id <- res$id
          if(exists("log_audit")) log_audit(con, curr_user, "CREATE", "registrations", target_id)
        } else {
          # UPDATE EXISTING
          target_id <- pt$id
          DBI::dbExecute(con,
            "UPDATE registrations SET
               hospital_number=$1, first_name=$2, last_name=$3,
               dob=$4::DATE, gender=$5, phone=$6, address1=$7,
               allergies=$8, comments=$9,
               updated_by=$10, updated_at=NOW()
             WHERE id = $11",
            list(input$hospital_number, input$first_name, input$last_name,
                 as.character(input$dob), input$gender, input$phone,
                 input$address1, input$allergies, input$comments,
                 curr_user, target_id))
          if(exists("log_audit")) log_audit(con, curr_user, "UPDATE", "registrations", target_id)
        }
        
        # Refresh global patient object
        updated_df <- DBI::dbGetQuery(con, "SELECT * FROM registrations WHERE id = $1", list(target_id))
        updated_list <- setNames(lapply(as.list(updated_df[1,,drop=F]), function(x) x[[1]]), names(updated_df))
        
        current_pt_out(updated_list)
        pt_state$view_mode <- "search" 
        showNotification("Record Saved Successfully", type = "message")
        
      }, error = function(e) { 
        showNotification(paste("Database Error:", e$message), type = "error") 
      })
    })
  })
}