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
      
      # Tab 2: Duplicate Patient Cleanup (Admin tool)
      nav_panel(
        title = "Duplicate Patients",
        card(
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                span(icon("users"), " Duplicate Patient Records"),
                actionButton(ns("scan_dups"), "Scan for Duplicates",
                             class = "btn-warning btn-sm", icon = icon("search")))
          ),
          card_body(uiOutput(ns("dup_results_ui")))
        )
      ),

      # Tab 3: Clinical Note Review (The Split Panel)
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

    # Create new user — was missing entirely (button did nothing)
    observeEvent(input$save_user, {
      req(input$new_username, input$new_fullname, input$new_password)
      clean_user <- trimws(input$new_username)
      clean_name <- trimws(input$new_fullname)

      tryCatch({
        hash <- sodium::password_store(input$new_password)
        dbExecute(pool,
          "INSERT INTO users (username, full_name, password_hash, role)
           VALUES ($1, $2, $3, $4)",
          list(clean_user, clean_name, hash, input$new_role)
        )
        updateTextInput(session, "new_username", value = "")
        updateTextInput(session, "new_fullname", value = "")
        updateTextInput(session, "new_password", value = "")
        showNotification(paste("User", clean_user, "created successfully."), type = "message")
        # Refresh the user list table
        output$user_list_table <- renderDT({
          df <- dbGetQuery(pool, "SELECT username, full_name, role FROM users")
          datatable(df, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
        })
      }, error = function(e) {
        showNotification(paste("Error creating user:", e$message), type = "error")
        message("User creation error: ", e$message)
      })
    })
    
    # 2. Daily Visits Reactive (Seen on Review Date)
    daily_visits <- reactive({
      req(input$review_date)
      tryCatch({
        dbGetQuery(pool,
          "SELECT
             v.id as visit_id,
             CONCAT(p.first_name, ' ', p.last_name) as patient_name
           FROM visitsmodule v
           LEFT JOIN registrations p ON TRIM(v.patient_id::text) = TRIM(p.id::text)
           WHERE v.visit_date::date = $1::date
           ORDER BY v.updated_at DESC",
          list(as.character(input$review_date))
        )
      }, error = function(e) {
        message("Daily Visit Error: ", e$message)
        data.frame(visit_id = character(), patient_name = character())
      })
    })

    # 3. Follow-up Reactive (Scheduled for Follow-up Date)
    followup_list <- reactive({
      req(input$followup_date_picker)
      tryCatch({
        dbGetQuery(pool,
          "SELECT
             v.id as visit_id,
             CONCAT(p.first_name, ' ', p.last_name) as patient_name
           FROM visitsmodule v
           LEFT JOIN registrations p ON TRIM(v.patient_id::text) = TRIM(p.id::text)
           WHERE
             NULLIF(v.visit_json ->> 'followup_date', '[]') IS NOT NULL
             AND (v.visit_json ->> 'followup_date')::date = $1::date
           ORDER BY p.first_name ASC",
          list(as.character(input$followup_date_picker))
        )
      }, error = function(e) {
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
    
    # ============================================================
    # 6. Duplicate Patient Management
    # ============================================================
    dup_scan_results <- reactiveVal(NULL)
    del_dup_pending  <- reactiveVal(NULL)
    merge_group_data <- reactiveVal(NULL)

    # Reusable query — run on demand and after every delete/merge
    run_dup_scan <- function() {
      tryCatch(
        dbGetQuery(pool,
          "WITH dup_groups AS (
             SELECT LOWER(TRIM(first_name)) AS fn,
                    LOWER(TRIM(last_name))  AS ln
               FROM registrations
              GROUP BY LOWER(TRIM(first_name)), LOWER(TRIM(last_name))
             HAVING COUNT(*) > 1
           )
           SELECT r.id, r.first_name, r.last_name,
                  r.dob, r.phone, r.hospital_number, r.created_at,
                  (SELECT COUNT(*) FROM visitsmodule
                    WHERE patient_id::text = r.id::text) AS visit_count,
                  (SELECT COUNT(*) FROM labs
                    WHERE patient_id::text = r.id::text) AS lab_count,
                  (SELECT COUNT(*) FROM prescriptions
                    WHERE patient_id::text = r.id::text) AS rx_count,
                  (SELECT COUNT(*) FROM past_medical_history
                    WHERE patient_id::text = r.id::text) AS pmh_count
             FROM registrations r
             JOIN dup_groups d
               ON LOWER(TRIM(r.first_name)) = d.fn
              AND LOWER(TRIM(r.last_name))  = d.ln
            ORDER BY LOWER(r.last_name), LOWER(r.first_name), r.created_at"
        ),
        error = function(e) {
          showNotification(paste("Scan error:", e$message), type = "error")
          NULL
        }
      )
    }

    observeEvent(input$scan_dups, {
      withProgress(message = "Scanning for duplicates...", value = 0.5, {
        dup_scan_results(run_dup_scan())
      })
    })

    output$dup_results_ui <- renderUI({
      res <- dup_scan_results()
      if (is.null(res))
        return(p(class = "text-muted mt-2", icon("info-circle"),
                 " Click 'Scan for Duplicates' to search."))
      if (nrow(res) == 0)
        return(div(class = "alert alert-success mt-2",
                   icon("check-circle"), " No duplicate patients found."))

      res$group_key <- paste(tolower(trimws(res$first_name)),
                             tolower(trimws(res$last_name)))
      groups <- split(res, res$group_key)

      div(
        p(class = "text-muted small mb-3",
          sprintf("Found %d duplicate group(s) across %d records.",
                  length(groups), nrow(res))),
        lapply(groups, function(grp) {
          grp <- grp[order(grp$created_at), ]   # oldest first = likely the original record

          div(class = "border rounded mb-3",
              # Group header
              div(class = "bg-light px-3 py-2 d-flex justify-content-between align-items-center",
                  tags$strong(paste(grp$first_name[1], grp$last_name[1])),
                  div(class = "d-flex align-items-center gap-2",
                      span(class = "badge bg-danger", paste(nrow(grp), "duplicates")),
                      tags$button(
                        class   = "btn btn-sm btn-outline-warning",
                        onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                          ns("open_merge_modal"), grp$group_key[1]),
                        icon("code-branch"), " Merge"
                      )
                  )
              ),
              # Each patient row within the group
              do.call(tagList, lapply(1:nrow(grp), function(i) {
                r        <- grp[i, ]
                has_data <- (r$visit_count + r$lab_count + r$rx_count + r$pmh_count) > 0

                div(class = "px-3 py-2 border-top d-flex justify-content-between align-items-start",
                    div(
                      span(class = "fw-bold me-2 small", paste0("ID: ", r$id)),
                      span(class = "text-muted small",
                           paste0("DOB: ",
                                  if (!is.na(r$dob)) as.character(r$dob) else "—",
                                  " | Phone: ",
                                  if (!is.na(r$phone) && nchar(trimws(r$phone)) > 0) r$phone else "—",
                                  if (!is.na(r$hospital_number) && nchar(trimws(r$hospital_number)) > 0)
                                    paste0(" | UHID: ", r$hospital_number) else "",
                                  " | Registered: ",
                                  format(as.Date(r$created_at), "%d %b %Y"))),
                      br(),
                      if (has_data) {
                        span(class = "badge bg-info text-dark",
                             paste0(r$visit_count, " visits  ",
                                    r$lab_count,   " labs  ",
                                    r$rx_count,    " Rx  ",
                                    r$pmh_count,   " PMH"))
                      } else {
                        span(class = "badge bg-secondary", "No clinical data")
                      }
                    ),
                    if (!has_data) {
                      tags$button(
                        class   = "btn btn-sm btn-outline-danger",
                        onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                          ns("del_dup_id"), r$id),
                        icon("trash"), " Delete"
                      )
                    } else {
                      span(class = "text-muted small fst-italic mt-1", "has data — use Merge")
                    }
                )
              }))
          )
        })
      )
    })

    # Delete empty duplicate — show confirmation first
    observeEvent(input$del_dup_id, {
      rid <- as.integer(input$del_dup_id)

      # Re-verify the record is truly empty before allowing deletion
      counts <- tryCatch(
        dbGetQuery(pool,
          "SELECT
             (SELECT COUNT(*) FROM visitsmodule        WHERE patient_id::text = $1::text) +
             (SELECT COUNT(*) FROM labs                WHERE patient_id::text = $1::text) +
             (SELECT COUNT(*) FROM prescriptions       WHERE patient_id::text = $1::text) +
             (SELECT COUNT(*) FROM past_medical_history WHERE patient_id::text = $1::text)
           AS total",
          list(as.character(rid)))$total,
        error = function(e) 1L   # fail safe: assume has data
      )

      if (counts > 0) {
        showNotification("This record has clinical data — use Merge instead.", type = "error")
        return()
      }

      pt <- tryCatch(
        dbGetQuery(pool,
          "SELECT first_name, last_name FROM registrations WHERE id = $1", list(rid)),
        error = function(e) data.frame(first_name = "?", last_name = "")
      )

      del_dup_pending(rid)
      showModal(modalDialog(
        title = tags$h4(class = "text-danger", icon("trash"), " Delete Empty Duplicate"),
        p("Permanently delete the registration for ",
          tags$strong(paste(pt$first_name, pt$last_name)),
          sprintf(" (ID: %d)?", rid)),
        p(class = "text-muted small",
          "This record has no clinical data. This action cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_del_dup"), "Delete", class = "btn-danger fw-bold")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_del_dup, {
      req(del_dup_pending())
      rid <- del_dup_pending()
      tryCatch({
        dbExecute(pool, "DELETE FROM registrations WHERE id = $1", list(rid))
        del_dup_pending(NULL)
        removeModal()
        showNotification("Duplicate record deleted.", type = "warning")
        dup_scan_results(run_dup_scan())   # refresh the list
      }, error = function(e) {
        showNotification(paste("Delete error:", e$message), type = "error")
      })
    })

    # Merge group — show modal to choose the primary record to keep
    observeEvent(input$open_merge_modal, {
      res <- dup_scan_results()
      req(res)
      grp_key <- input$open_merge_modal
      grp <- res[paste(tolower(trimws(res$first_name)),
                       tolower(trimws(res$last_name))) == grp_key, ]
      req(nrow(grp) >= 2)
      grp <- grp[order(grp$created_at), ]
      merge_group_data(grp)

      showModal(modalDialog(
        title = tags$h4(icon("code-branch"), " Merge Duplicate Records"),
        p("Select which record to ", tags$strong("KEEP"), " as the primary patient. ",
          "All clinical data from the other record(s) will be moved to it, ",
          "then the duplicates will be deleted."),
        radioButtons(ns("merge_primary_choice"), "Primary record to keep:",
          choiceValues = as.character(grp$id),
          choiceNames  = lapply(1:nrow(grp), function(i) {
            r          <- grp[i, ]
            total_data <- r$visit_count + r$lab_count + r$rx_count + r$pmh_count
            span(
              tags$strong(paste0("ID: ", r$id)),
              span(class = "text-muted small",
                   paste0(" | Registered: ", format(as.Date(r$created_at), "%d %b %Y"),
                          " | ", r$visit_count, " visits, ",
                          r$lab_count, " labs, ",
                          r$rx_count,  " Rx, ",
                          r$pmh_count, " PMH")),
              if (total_data == 0) span(class = "badge bg-secondary ms-1", "no data")
            )
          })
        ),
        p(class = "text-danger small mt-2",
          icon("exclamation-triangle"),
          " The other record(s) will be permanently deleted after their data is moved."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_merge"), "Merge & Delete Others",
                       class = "btn-danger fw-bold")
        ),
        size = "m", easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_merge, {
      grp <- merge_group_data()
      req(grp, input$merge_primary_choice)

      primary_id    <- as.integer(input$merge_primary_choice)
      secondary_ids <- grp$id[as.integer(grp$id) != primary_id]

      tryCatch({
        con <- pool::poolCheckout(pool)
        on.exit(pool::poolReturn(con), add = TRUE)
        DBI::dbExecute(con, "BEGIN")

        for (sec_id in secondary_ids) {
          sid <- as.character(sec_id)
          pid <- as.character(primary_id)
          DBI::dbExecute(con,
            "UPDATE visitsmodule SET patient_id = $1 WHERE patient_id::text = $2",
            list(pid, sid))
          DBI::dbExecute(con,
            "UPDATE labs SET patient_id = $1 WHERE patient_id::text = $2",
            list(pid, sid))
          DBI::dbExecute(con,
            "UPDATE prescriptions SET patient_id = $1 WHERE patient_id::text = $2",
            list(pid, sid))
          DBI::dbExecute(con,
            "UPDATE past_medical_history SET patient_id = $1 WHERE patient_id::text = $2",
            list(pid, sid))
          DBI::dbExecute(con,
            "DELETE FROM registrations WHERE id = $1", list(as.integer(sec_id)))
        }

        DBI::dbExecute(con, "COMMIT")
        merge_group_data(NULL)
        removeModal()
        showNotification(
          paste("Merged", length(secondary_ids), "duplicate(s) into primary record."),
          type = "message")
        dup_scan_results(run_dup_scan())   # refresh the list

      }, error = function(e) {
        tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
        showNotification(paste("Merge error:", e$message), type = "error")
        message("Merge error: ", e$message)
      })
    })

    # Return the reactives so the main app can observe row selections
    return(list(
      daily_selected = reactive({ daily_visits()[input$daily_review_table_rows_selected, ] }),
      followup_selected = reactive({ followup_list()[input$followup_table_rows_selected, ] })
    ))
  })
}