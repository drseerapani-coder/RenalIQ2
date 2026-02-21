user_management_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Add New Portal User"),
        textInput(ns("new_username"), "Username / Employee ID"),
        textInput(ns("new_fullname"), "Full Name"),
        passwordInput(ns("new_password"), "Initial Password"),
        
        # FIXED: Ensure choices are exactly what the server logic expects
        selectInput(ns("new_role"), "System Role", 
                    choices = c("Clinician" = "clinician", 
                                "Administrator" = "admin")),
        
        card_footer(
          actionButton(ns("save_user"), "Create & Approve User", 
                       class = "btn-success w-100", icon = icon("user-check"))
        )
      ),
      
      card(
        card_header("Existing System Users"),
        # We add the table here
        DTOutput(ns("user_list_table"))
      )
    )
  )
}

user_management_server <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh the table after a new user is added
    refresh_trigger <- reactiveVal(0)
    
    # FIXED: Querying both name columns to ensure visibility
    output$user_list_table <- renderDT({
      refresh_trigger() # Dependency
      
      df <- dbGetQuery(pool, "
        SELECT username, full_name, role, approved 
        FROM users 
        ORDER BY id DESC
      ")
      
      datatable(df, 
                options = list(pageLength = 5, dom = 'tp'),
                rownames = FALSE,
                colnames = c("ID/User", "Display Name", "Role", "Status"))
    })
    
    observeEvent(input$save_user, {
      req(input$new_username, input$new_password, input$new_fullname, input$new_role)
      
      hashed_pw <- sodium::password_store(input$new_password)
      
      tryCatch({
        # We populate BOTH full_name and fullname to be safe
        dbExecute(pool, 
                  "INSERT INTO users (username, full_name, fullname, role, password_hash, approved, requested_on) 
           VALUES ($1, $2, $3, $4, $5, 1, CURRENT_TIMESTAMP)",
                  list(
                    tolower(trimws(input$new_username)), 
                    input$new_fullname, # for full_name
                    input$new_fullname, # for fullname
                    input$new_role, 
                    hashed_pw
                  )
        )
        
        showNotification("User created and approved!", type = "message")
        
        # Clear form
        updateTextInput(session, "new_username", value = "")
        updateTextInput(session, "new_fullname", value = "")
        updateTextInput(session, "new_password", value = "")
        
        # Refresh the table
        refresh_trigger(refresh_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error")
      })
    })
  })
}