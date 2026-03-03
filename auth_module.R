library(shiny)
library(sodium)
library(stringr)
library(glue)

# UI Component for Sign Out
logout_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("logout_btn"), 
    "Sign Out", 
    class = "btn-outline-dark btn-sm", 
    icon = icon("sign-out-alt"),
    style = "margin-left: 15px;"
  )
}

# Main Login UI
auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(glue("
      #{ns('login_container')} {{
        height: 100vh; display: flex; align-items: center; justify-content: center; background: #f8f9fa;
      }}
      .login-card {{ width: 400px; padding: 30px; border-radius: 12px; box-shadow: 0 10px 25px rgba(0,0,0,0.1); }}
    "))),
    div(id = ns("login_container"),
        div(class = "login-card bg-white",
            h3("Renal IQ Login", class="text-center mb-4", style="color: #26A69A; font-weight: bold;"),
            textInput(ns("user_id"), "Username / Employee ID"),
            passwordInput(ns("password"), "Password"),
            br(),
            actionButton(ns("login_btn"), "Sign In", class="btn-primary w-100", style="background-color: #26A69A; border: none;"),
            uiOutput(ns("error_msg"))
        )
    )
  )
}

# Server Logic
auth_server <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    user_auth <- reactiveValues(is_logged = FALSE, user_info = NULL)
    
    observeEvent(input$login_btn, {
      req(input$user_id, input$password)
      
      # 1. Clean inputs
      clean_user <- str_trim(input$user_id)
      
      # 2. Database Query with Error Handling
      user_data <- tryCatch({
        dbGetQuery(pool, 
                   "SELECT id, username, full_name, role, password_hash FROM users WHERE LOWER(username) = LOWER($1)", 
                   list(clean_user))
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = NULL)
        return(NULL)
      })
      
      # 3. Validation Logic
      if(!is.null(user_data) && nrow(user_data) == 1) {
        hash_in_db <- user_data$password_hash[1]
        
        # Verify the hash exists
        if (is.na(hash_in_db) || hash_in_db == "") {
          output$error_msg <- renderUI({ p("Account configuration error", class="text-danger mt-2 text-center") })
          return()
        }
        
        # SODIUM Check
        is_valid <- tryCatch({
          sodium::password_verify(hash_in_db, input$password)
        }, error = function(e) { 
          showNotification("Password hashing error. Check DB format.", type = "warning")
          FALSE 
        })
        
        if (is_valid) {
          user_auth$is_logged <- TRUE
          # Store only the columns needed by modules — never expose password_hash
          user_auth$user_info <- user_data[, c("id", "username", "full_name", "role"), drop = FALSE]
          showNotification(paste("Welcome,", user_data$full_name), type = "message")
        } else {
          output$error_msg <- renderUI({ p("Invalid username or password", class="text-danger mt-2 text-center") })
        }
      } else {
        output$error_msg <- renderUI({ p("User not found", class="text-danger mt-2 text-center") })
      }
    })
    
    # Logout Logic
    observeEvent(input$logout_btn, {
      user_auth$is_logged <- FALSE
      user_auth$user_info <- NULL
      updateTextInput(session, "user_id", value = "")
      updateTextInput(session, "password", value = "") 
      showNotification("Signed out.", type = "warning")
    })
    
    return(list(
      is_logged = reactive({ user_auth$is_logged }),
      user_info = reactive({ user_auth$user_info })
    ))
  })
}