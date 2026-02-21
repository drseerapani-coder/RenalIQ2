# app.R
library(shiny)
library(bslib)
library(pool)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(shinyjs)
library(jsonlite)
library(glue)
library(rhandsontable)
library(shinycssloaders)

# 1. Load Configuration & Helpers
source("helpers.R")
source("auth_module.R") 
source("mod_registration.R")
source("mod_clinical.R")
#source("mod_clinical_templates.R")
source("mod_mobile_rx.R")
source("mod_lab_flowsheet.R")
source("mod_lab_ingestion.R")
source("mod_user_mgmt.R")
source("clinical_summary.R")

lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
lab_config <- split(lab_targets_raw$test_name, lab_targets_raw$category)

# 2. Database Connection
pool <- tryCatch({
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = Sys.getenv("DO_DB_NAME"), 
    host     = Sys.getenv("DO_DB_HOST"),
    user     = Sys.getenv("DO_DB_USER"),
    password = Sys.getenv("DO_DB_PASSWORD"),
    port     = as.integer(Sys.getenv("DO_DB_PORT", unset = "25060")),
    sslrootcert = Sys.getenv("DO_SSLROOTCERT", unset = "ca-certificate.crt"),
    sslmode  = "require"
  )
}, error = function(e) {
  message("DB connection failed: ", e$message)
  NULL 
})

onStop(function() { if (!is.null(pool)) poolClose(pool) })

# --- UI Layout Functions ---
# --- UI Layout Functions ---
secure_ui_contents <- function() {
  # Note: page_navbar is a top-level layout; it shouldn't be inside fluidPage
  page_navbar(
    title = div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
      span("Renal IQ Portal", style = "margin-right: 30px; font-weight: bold;"),
      uiOutput("global_pt_header") 
    ),
    nav_spacer(),
    nav_item(uiOutput("logout_btn_ui")), 
    
    theme = bs_theme(version = 5, primary = "#26A69A"),
    id = "main_nav",
    
    # Include shinyjs here so it's available after login
    header = list(shinyjs::useShinyjs()),
    
    nav_panel("1. Registration", registration_ui("reg_mod")),
    nav_panel("2. Clinical Notes", clinical_ui("clin_mod")),
    nav_panel("3. Mobile Rx", mobile_rx_ui("rx_mod")),
    nav_panel("4. Labs Flowsheet", lab_flowsheet_ui("lab_mod", lab_config)),
    nav_panel("5. AI Lab Ingestion", lab_ingestion_ui("lab_ingest_mod")),
    nav_panel("6. Timeline", timeline_ui("pt_timeline")),
    nav_menu(
      title = "Admin",
      align = "right",
      nav_panel("User Management", uiOutput("admin_panel_ui"))
    )
  )
}

ui <- uiOutput("root_layout")

# ... (Previous library imports and DB connection code remain the same) ...

# 3. Server Logic

  server <- function(input, output, session) {
    
    auth <- auth_server("auth_mod", pool)
    current_pt <- reactiveVal(NULL)
    
    is_authenticated <- reactive({
      req(auth)
      val <- if (is.function(auth$is_logged)) auth$is_logged() else auth$is_logged
      isTRUE(val)
    })
    
    # Switch between full layouts
    output$root_layout <- renderUI({
      if (is_authenticated()) {
        secure_ui_contents()
      } else {
        # Use fluidPage only for the login screen to center the box
        fluidPage(
          theme = bs_theme(version = 5, primary = "#26A69A"),
          auth_ui("auth_mod")
        )
      }
    })
  
  # --- 3. MODULE SERVERS (STATIC REGISTRATION) ---
  
  
  registration_server("reg_mod", pool, current_pt, auth$user_info)
  clinical_server("clin_mod", pool, current_pt, auth$user_info)
  lab_flowsheet_server("lab_mod", pool, current_pt, lab_targets_raw, reactive(input$main_nav), auth$user_info)
  lab_ingestion_server("lab_ingest_mod", pool, current_pt, auth$user_info)
  mobile_rx_server("rx_mod", pool, current_pt, auth$user_info)
  timeline_server("pt_timeline", pool, current_pt)
  
  # --- 4. GLOBAL UI RENDERERS ---
  
  # Logout Button (appears in the navbar)
  output$logout_btn_ui <- renderUI({
    req(is_authenticated())
    logout_ui("auth_mod")
  })
  
  # Patient Header (displays name and ID at the top)
  output$global_pt_header <- renderUI({
    req(is_authenticated())
    pt <- current_pt()
    
    if (is.null(pt)) {
      span("No Patient Selected", class = "badge bg-secondary text-white", style="opacity: 0.8;")
    } else {
      # Robust name detection logic
      p_name <- "Unknown Patient"
      if (!is.null(pt$full_name)) {
        p_name <- pt$full_name
      } else if (!is.null(pt$first_name)) {
        p_name <- paste(pt$first_name, pt$last_name)
      } else if (!is.null(pt$name)) {
        p_name <- pt$name
      }
      
      span(
        style = "display: flex; align-items: center; gap: 10px; margin-left: 15px;",
        # Changed color from white to a dark gray/black for visibility
        span(toupper(p_name), style = "color: #333; font-weight: bold; font-size: 1.1rem;"),
        span(paste("ID:", pt$id), class = "badge bg-dark text-white", style="font-size: 0.7rem;")
      )
    }
  })
  
  # --- 5. STATE CLEANUP LOGIC ---
  # If the user logs out, reset the selected patient so the next person 
  # starts with a fresh session and no leaked data.
  observeEvent(is_authenticated(), {
    if (!is_authenticated()) {
      current_pt(NULL)
    }
  })
  
  # Logic to clear clinical notes/inputs when a new patient is selected
  # Most modules should handle this internally with req(current_pt())
  observeEvent(current_pt(), {
    if (is.null(current_pt())) {
      # This is where you'd trigger any extra cleanup logic if needed
      message("Patient context cleared. Modules resetting...")
    }
  }, ignoreNULL = FALSE)
  
  # Render the admin UI only if the user is an admin
  output$admin_panel_ui <- renderUI({
    req(is_authenticated())
    user_info <- auth$user_info()
    
    # Only allow users with 'admin' role to see the management tools
    if (!is.null(user_info) && user_info$role[1] == "admin") {
      user_management_ui("user_mgmt")
    } else {
      div(class = "alert alert-danger", "Access Denied: Admin privileges required.")
    }
  })
  
  # Call the admin module server
  # We assume you'll create user_management_server
  user_management_server("user_mgmt", pool)
  
}

shinyApp(ui, server)