library(shiny)
library(bslib)
library(pool)
library(DBI)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(shinyjs)
library(jsonlite)
library(glue)
library(rhandsontable)
library(shinycssloaders)
library(pdftools)
try(library(sodium), silent = TRUE)
try(library(RPostgres), silent = TRUE)
try(library(base64enc), silent = TRUE)

# 1. Load Configuration & Helpers
source("helpers.R")
source("auth_module.R") 
source("mod_registration.R")
source("mod_clinical.R")
source("mod_mobile_rx.R")
source("mod_lab_ingestion.R")
source("mod_lab_flowsheet.R")
source("mod_user_mgmt.R")
source("clinical_summary.R")

# Load static data - Wrapped in try to prevent crash if file read fails
lab_targets_raw <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
lab_config <- split(lab_targets_raw$test_name, lab_targets_raw$category)

# 2. Database Connection Logic
pool <- tryCatch({
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = Sys.getenv("DO_DB_NAME"), 
    host     = Sys.getenv("DO_DB_HOST"),
    user     = Sys.getenv("DO_DB_USER"),
    password = Sys.getenv("DO_DB_PASSWORD"),
    port     = as.integer(Sys.getenv("DO_DB_PORT", unset = "25060")),
    sslmode  = "require",
    sslrootcert = "ca-certificate.crt",
    connect_timeout = 3 
  )
}, error = function(e) {
  message("DATABASE CONNECTION HANG PREVENTED: ", e$message)
  return(NULL) 
})

onStop(function() { 
  if (!is.null(pool) && inherits(pool, "Pool")) {
    poolClose(pool) 
  }
})

# 2. FAIL-SAFE OPENAI KEY
openai_key <- Sys.getenv("OPENAI_API_KEY")
if (nchar(openai_key) == 0) {
  message("Warning: OPENAI_API_KEY is missing. AI features will be disabled.")
}

# --- Mobile Optimized UI ---
secure_ui_contents <- function() {
  page_navbar(
    title = "Renal IQ",
    theme = bs_theme(
      version = 5, 
      primary = "#26A69A",
      "navbar-bg" = "#ffffff"
    ),
    id = "main_nav",
    header = list(
      shinyjs::useShinyjs(),
      tags$style(HTML("
        .navbar-brand { font-weight: bold; color: #26A69A !important; }
        
        /* 1. Lower the z-index of the sticky patient header */
        .pt-display-header { 
          background: #f8f9fa; 
          padding: 10px 15px; 
          border-bottom: 2px solid #26A69A;
          display: flex;
          align-items: center;
          justify-content: space-between;
          position: sticky;
          top: 0;
          z-index: 900; /* Reduced from 1000 */
        }

        /* 2. Ensure the Navbar and its dropdowns stay on top */
        .navbar {
          z-index: 1050 !important; 
        }
        
        .dropdown-menu {
          z-index: 1100 !important;
        }

        /* 3. Fix for mobile: ensure dropdowns aren't clipped by hidden overflow */
        .navbar-collapse {
          overflow: visible !important;
        }

        @media (max-width: 576px) {
          .selected-pt-name { font-size: 0.9rem !important; }
        }
      ")),
      uiOutput("global_pt_header")
    ),
    
    nav_spacer(),
    
    nav_panel("Demographics", registration_ui("reg_mod")),
    nav_panel("Notes", clinical_ui("clin_mod")),
    nav_panel("Prescriptions", mobile_rx_ui("rx_mod")),
    nav_panel("Labs", lab_flowsheet_ui("lab_mod", lab_config)),
    nav_panel("Upload Labs", lab_ingestion_ui("lab_ingest_mod")),
    nav_panel("Summary", timeline_ui("pt_timeline")),
    
    nav_menu(
      title = "More",
      nav_panel("Admin", uiOutput("admin_panel_ui")),
      nav_item(uiOutput("logout_btn_ui"))
    )
  )
}

ui <- uiOutput("root_layout")

# 3. Server Logic
server <- function(input, output, session) {
  current_pt  <- reactiveVal(NULL)
  refresh_val <- reactiveVal(0)

  db_available <- reactive({ !is.null(pool) && inherits(pool, "Pool") })
  auth <- auth_server("auth_mod", pool)

  # Reactive UI Switcher
  output$root_layout <- renderUI({
    if (!db_available()) {
      return(fluidPage(
        theme = bs_theme(version = 5, primary = "#26A69A"),
        div(style="margin-top: 50px; padding: 20px;",
            class = "alert alert-danger",
            h4("Connection Error"),
            p("Database cluster unreachable."),
            tags$small("Check DO_DB credentials and Trusted Sources.")
        )
      ))
    }

    if (auth$is_logged()) {
      secure_ui_contents()
    } else {
      fluidPage(theme = bs_theme(version = 5, primary = "#26A69A"), auth_ui("auth_mod"))
    }
  })

  # CRITICAL: Initialize module servers ONLY ONCE after first login.
  # ignoreInit = TRUE prevents the body running at startup (is_logged = FALSE).
  # once = TRUE ensures modules are NEVER re-created on a subsequent login —
  # accumulating duplicate observers was the previous bug.
  observeEvent(auth$is_logged(), {
    req(auth$is_logged())

    reg_logic        <- registration_server("reg_mod", pool, current_pt, auth$user_info)
    clin_logic       <- clinical_server("clin_mod", pool, current_pt, auth$user_info)
    rx_logic         <- mobile_rx_server("rx_mod", pool, current_pt, auth$user_info)
    lab_flow_logic   <- lab_flowsheet_server("lab_mod", pool, current_pt, lab_targets_raw,
                                             reactive(input$main_nav), auth$user_info)
    lab_ingest_logic <- lab_ingestion_server("lab_ingest_mod", pool, current_pt,
                                             auth$user_info, lab_targets_raw)

    # Refresh timeline whenever any module successfully saves data.
    # Pass refresh_val directly (not wrapped in reactive()) — creating reactive()
    # inside observeEvent is an anti-pattern that silently breaks invalidation.
    observeEvent(clin_logic$saved(),       { refresh_val(refresh_val() + 1) }, ignoreInit = TRUE)
    observeEvent(rx_logic$saved(),         { refresh_val(refresh_val() + 1) }, ignoreInit = TRUE)
    observeEvent(lab_flow_logic$saved(),   { refresh_val(refresh_val() + 1) }, ignoreInit = TRUE)
    observeEvent(lab_ingest_logic$saved(), { refresh_val(refresh_val() + 1) }, ignoreInit = TRUE)

    timeline_server("pt_timeline", pool = pool, current_pt = current_pt,
                    refresh_trigger = refresh_val)

    # Return values (daily_selected, followup_selected) reserved for future
    # cross-module navigation; discarded intentionally for now.
    user_management_server("user_mgmt", pool)

  }, ignoreInit = TRUE, once = TRUE)
  
  
  # Sticky Header Logic
  # Sticky Header Logic with Age Calculation
  output$global_pt_header <- renderUI({
    req(auth$is_logged())
    pt <- current_pt()
    
    if (is.null(pt)) {
      return(div(class = "pt-display-header", 
                 span("No Patient Selected", class = "text-muted small italic")))
    }
    
    # 1. Formatting Name
    f_name <- as.character(pt$first_name %||% "")
    l_name <- as.character(pt$last_name %||% "")
    full_name <- toupper(trimws(paste(f_name, l_name)))
    if(nchar(full_name) == 0) full_name <- "SELECTED PATIENT"
    
    # 2. Calculate Age
    # Assumes your database column is named 'dob' or 'date_of_birth'
    pt_dob <- pt$dob %||% pt$date_of_birth 
    age_display <- ""
    
    if (!is.null(pt_dob) && !is.na(pt_dob)) {
      # Ensure it's a Date object
      birth_date <- as.Date(pt_dob)
      age_val <- floor(time_length(difftime(Sys.Date(), birth_date), "years"))
      age_display <- paste0(" | Age: ", age_val)
    }
    
    # 3. Render UI
    div(class = "pt-display-header",
        span(class = "selected-pt-name",
             icon("user-circle"), 
             tags$strong(full_name, style="margin-left: 5px; color: #26A69A;"),
             span(age_display, style="font-weight: normal; color: #666; margin-left: 5px;")
        ),
        span(paste("ID:", pt$id), class = "badge bg-dark", style="margin-left:5px;")
    )
  })
  
  output$logout_btn_ui <- renderUI({ req(auth$is_logged()); logout_ui("auth_mod") })
  
  # 2. Centralized Global Refresh
  
  
  output$admin_panel_ui <- renderUI({
    req(auth$is_logged())
    user_data <- auth$user_info() 
    if (!is.null(user_data) && user_data$role[1] == "admin") {
      user_management_ui("user_mgmt")
    } else {
      div(class = "alert alert-warning", "Admin Access Required")
    }
  })
}

shinyApp(ui, server)