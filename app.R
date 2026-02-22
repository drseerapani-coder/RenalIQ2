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
source("mod_mobile_rx.R")
source("mod_lab_flowsheet.R")
source("mod_lab_ingestion.R")
source("mod_user_mgmt.R")
source("clinical_summary.R")

# Load static data
lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
lab_config <- split(lab_targets_raw$test_name, lab_targets_raw$category)

# 2. Database Connection (RE-CLEANED)
pool <- tryCatch({
  # Explicitly using the password we just verified
  db_pass <- Sys.getenv("DO_DB_PASSWORD")
  
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = "defaultdb", 
    host     = "db-postgresql-blr1-50634-do-user-27163608-0.f.db.ondigitalocean.com",
    user     = "doadmin",
    port     = 25060,
    password = db_pass,
    sslmode  = "require",
    connect_timeout = 15
  )
}, error = function(e) {
  message("CRITICAL DB CONNECTION FAILURE: ", e$message)
  return(NULL)
})

onStop(function() { 
  if (!is.null(pool) && inherits(pool, "Pool")) {
    poolClose(pool) 
  }
})

# --- UI Layout Functions ---
secure_ui_contents <- function() {
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

# 3. Server Logic
server <- function(input, output, session) {
  
  # A. Setup stable reactives
  current_pt <- reactiveVal(NULL)
  
  # B. Check Database Availability
  db_available <- reactive({ 
    !is.null(pool) && inherits(pool, "Pool") 
  })
  
  # C. Initialize Auth Server ONCE at the top level
  # This prevents the re-initialization loop causing the "Welcome" flicker
  auth <- auth_server("auth_mod", pool)
  
  # D. Main UI Switcher
  output$root_layout <- renderUI({
    # Condition 1: DB Fail
    if (!db_available()) {
      return(fluidPage(
        theme = bs_theme(version = 5, primary = "#26A69A"),
        div(style="margin-top: 100px; max-width: 600px; margin-left: auto; margin-right: auto;",
            class = "alert alert-danger",
            h4("Database Offline"),
            p("The application is running but cannot reach the database cluster."),
            tags$small("Check DigitalOcean Trusted Sources/Firewall.")
        )
      ))
    }
    
    # Condition 2: Authenticated
    if (auth$is_logged()) {
      secure_ui_contents()
    } else {
      # Condition 3: Login Screen
      fluidPage(
        theme = bs_theme(version = 5, primary = "#26A69A"),
        auth_ui("auth_mod")
      )
    }
  })
  
  # E. Trigger Module Servers ONLY on Login Success
  # Using observeEvent prevents the modules from re-firing constantly
  observeEvent(auth$is_logged(), {
    req(auth$is_logged())
    
    registration_server("reg_mod", pool, current_pt, auth$user_info)
    clinical_server("clin_mod", pool, current_pt, auth$user_info)
    lab_flowsheet_server("lab_mod", pool, current_pt, lab_targets_raw, reactive(input$main_nav), auth$user_info)
    lab_ingestion_server("lab_ingest_mod", pool, current_pt, auth$user_info)
    mobile_rx_server("rx_mod", pool, current_pt, auth$user_info)
    timeline_server("pt_timeline", pool, current_pt)
    user_management_server("user_mgmt", pool)
    
    message("Session started for user: ", auth$user_info()$username)
  })
  
  # F. Global UI Renderers
  output$logout_btn_ui <- renderUI({
    req(auth$is_logged())
    logout_ui("auth_mod")
  })
  
  output$admin_panel_ui <- renderUI({
    req(auth$is_logged())
    user_data <- auth$user_info() 
    if (!is.null(user_data) && user_data$role[1] == "admin") {
      user_management_ui("user_mgmt")
    } else {
      div(class = "alert alert-warning", "Admin privileges required.")
    }
  })
  
  output$global_pt_header <- renderUI({
    req(auth$is_logged())
    pt <- current_pt()
    if (is.null(pt)) {
      span("No Patient Selected", class = "badge bg-secondary text-white", style="opacity: 0.8;")
    } else {
      p_name <- if (!is.null(pt$full_name)) pt$full_name else "Selected Patient"
      span(
        style = "display: flex; align-items: center; gap: 10px; margin-left: 15px;",
        span(toupper(p_name), style = "color: #333; font-weight: bold; font-size: 1.1rem;"),
        span(paste("ID:", pt$id), class = "badge bg-dark text-white", style="font-size: 0.7rem;")
      )
    }
  })
  
  # G. Cleanup on Logout
  observe({
    if (!auth$is_logged()) {
      current_pt(NULL)
    }
  })
  
  # Network Settings
  options(shiny.host = '0.0.0.0')
  options(shiny.port = 8080)
}

shinyApp(ui, server)