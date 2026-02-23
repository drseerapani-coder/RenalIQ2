# --- UI Function ---
user_management_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_pill(
      # Tab 1: User Access Control
      nav_panel(
        title = "User Access",
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Add New Portal User"),
            card_body(
              textInput(ns("new_username"), "Username / Employee ID"),
              textInput(ns("new_fullname"), "Full Name"),
              passwordInput(ns("new_password"), "Initial Password"),
              selectInput(ns("new_role"), "System Role", 
                          choices = c("Clinician" = "clinician", "Administrator" = "admin"))
            ),
            card_footer(
              actionButton(ns("save_user"), "Create & Approve User", 
                           class = "btn-success w-100", icon = icon("user-check"))
            )
          ),
          card(
            card_header("Existing System Users"),
            card_body(
              DTOutput(ns("user_list_table"))
            )
          )
        )
      ),
      
      # Tab 2: Clinical Note Review (The Split Panel)
      nav_panel(
        title = "Clinical Note Review",
        layout_column_wrap(
          width = 1/2,
          # Left Column: Daily Visits
          card(
            card_header(
              div(class = "d-flex justify-content-between align-items-center",
                  span(icon("hospital-user"), " Patients Seen"),
                  div(style = "width: 150px;",
                      dateInput(ns("review_date"), NULL, value = Sys.Date()))
              )
            ),
            card_body(
              DTOutput(ns("daily_review_table"))
            )
          ),
          # Right Column: Follow-ups
          card(
            card_header(
              div(class = "d-flex justify-content-between align-items-center",
                  span(icon("calendar-day"), " Scheduled Follow-ups"),
                  div(style = "width: 150px;",
                      dateInput(ns("followup_date_picker"), NULL, value = Sys.Date() + 1))
              )
            ),
            card_body(
              DTOutput(ns("followup_table"))
            )
          )
        )
      )
    )
  )
}

# --- Server Function ---
user_management_server <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. User Management Logic
    output$user_list_table <- renderDT({
      df <- dbGetQuery(pool, "SELECT username, full_name, role FROM users")
      datatable(df, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
    })
    
    # 2. Daily Visits Reactive (Seen on Review Date)
    daily_visits <- reactive({
      req(input$review_date)
      query <- glue::glue_sql("
        SELECT 
          v.id as visit_id,
          CONCAT(p.first_name, ' ', p.last_name) as patient_name
        FROM visitsmodule v
        LEFT JOIN registrations p ON TRIM(v.patient_id::text) = TRIM(p.id::text)
        WHERE v.visit_date::date = {input$review_date}
        ORDER BY v.updated_at DESC
      ", .con = pool)
      
      tryCatch({
        dbGetQuery(pool, query)
      }, error = function(e) {
        message("Daily Visit Error: ", e$message)
        data.frame(visit_id = character(), patient_name = character())
      })
    })
    
    # 3. Follow-up Reactive (Scheduled for Follow-up Date)
    followup_list <- reactive({
      req(input$followup_date_picker)
      
      # We extract the date, handle empty arrays/strings, 
      # and cast to date for comparison.
      query <- glue::glue_sql("
        SELECT 
          v.id as visit_id,
          CONCAT(p.first_name, ' ', p.last_name) as patient_name
        FROM visitsmodule v
        LEFT JOIN registrations p ON TRIM(v.patient_id::text) = TRIM(p.id::text)
        WHERE 
          NULLIF(v.visit_json ->> 'followup_date', '[]') IS NOT NULL 
          AND (v.visit_json ->> 'followup_date')::date = {input$followup_date_picker}
        ORDER BY p.first_name ASC
      ", .con = pool)
      
      tryCatch({
        dbGetQuery(pool, query)
      }, error = function(e) {
        # If the JSON value isn't a valid date format, it might error.
        # This keeps the app running.
        message("Followup JSON Error: ", e$message)
        data.frame(visit_id = integer(), patient_name = character())
      })
    })
    
    # 4. Render Daily Review Table (Name Only)
    output$daily_review_table <- renderDT({
      df <- daily_visits()
      datatable(df, 
                selection = 'single', 
                rownames = FALSE,
                colnames = c("Visit ID", "Patient Name"),
                options = list(
                  columnDefs = list(list(visible = FALSE, targets = 0)), # Hide Visit ID
                  dom = 'ftp', 
                  pageLength = 10,
                  language = list(search = "Filter Names:")
                ))
    })
    
    # 5. Render Follow-up Table (Name Only)
    output$followup_table <- renderDT({
      df <- followup_list()
      datatable(df, 
                selection = 'single', 
                rownames = FALSE,
                colnames = c("Visit ID", "Patient Name"),
                options = list(
                  columnDefs = list(list(visible = FALSE, targets = 0)), # Hide Visit ID
                  dom = 'ftp', 
                  pageLength = 10,
                  language = list(search = "Filter Names:")
                ))
    })
    
    # Return the reactives so the main app can observe row selections
    return(list(
      daily_selected = reactive({ daily_visits()[input$daily_review_table_rows_selected, ] }),
      followup_selected = reactive({ followup_list()[input$followup_table_rows_selected, ] })
    ))
  })
}