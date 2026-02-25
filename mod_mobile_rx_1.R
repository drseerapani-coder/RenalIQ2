# mod_mobile_rx.R

mobile_rx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("rx_main_container"))
  )
}

mobile_rx_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Local Reactive State ---
    rx_master <- reactiveVal(data.frame())
    
    clean_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(empty_rx_df()[0,])
      df[] <- lapply(df, function(x) {
        x[is.na(x)] <- ""
        as.character(x)
      })
      df
    }
    
    empty_rx_df <- function() {
      data.frame(
        brand_name = "", generic = "", dose = "", 
        freq = "OD8", route = "Oral", duration = "Until next visit", 
        stringsAsFactors = FALSE
      )
    }
    
    rx_meds <- reactiveValues(
      df = data.frame(brand_name = character(), generic = character(), dose = character(), 
                      freq = character(), route = character(), duration = character(),
                      stringsAsFactors = FALSE),
      selected_idx = NULL,
      s_cat  = "OD",  
      s_num  = "8",   
      s_ampm = "AM"   
    )
    rx_meta <- reactiveValues(is_history = FALSE, history_date = NULL)
    
    # --- Internal Stitcher Logic ---
    observe({
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      cat <- rx_meds$s_cat
      
      if(cat %in% c("OD", "BD")) {
        num <- as.numeric(rx_meds$s_num)
        ampm <- rx_meds$s_ampm
        hour <- num
        if(ampm == "PM" && hour < 12) hour <- hour + 12
        if(ampm == "AM" && hour == 12) hour <- 24 
        rx_meds$df$freq[idx] <- paste0(cat, hour)
      } 
    })
    
    # --- Quick Edit Panel Function ---
    # Moved to a function so it can be called inside the list renderer
    render_quick_edit <- function() {
      idx <- as.numeric(rx_meds$selected_idx)
      curr <- rx_meds$df[idx, ]
      
      div(class="card border-primary mb-3 shadow-lg",
          div(class="card-header bg-primary text-white d-flex justify-content-between align-items-center",
              span(icon("edit"), strong(" Adjusting: "), curr$brand_name),
              actionButton(ns("close_edit"), "Done", class="btn-sm btn-light")
          ),
          div(class="card-body p-2", style="background-color: #fcfcfc;",
              
              tags$label(class="text-muted small", "Frequency Group"),
              div(class="d-flex flex-wrap gap-1 mb-3",
                  lapply(c("OD", "BD", "TDS/QID", "Days", "Period", "Misc"), function(cat) {
                    actionButton(ns(paste0("scat_", cat)), cat, 
                                 class = if(rx_meds$s_cat == cat) "btn-sm btn-dark" else "btn-sm btn-outline-dark",
                                 onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_cat"), cat))
                  })
              ),
              
              div(class="p-2 border rounded bg-white mb-3",
                  if(rx_meds$s_cat %in% c("OD", "BD")) {
                    div(class="d-flex align-items-center gap-2",
                        div(class="d-flex flex-wrap gap-1", style="flex: 3;",
                            lapply(1:12, function(n) {
                              actionButton(ns(paste0("snum_", n)), as.character(n), 
                                           style="width: 42px; height: 42px; padding: 0;",
                                           class = if(rx_meds$s_num == as.character(n)) "btn btn-primary" else "btn btn-outline-primary",
                                           onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_num"), n))
                            })
                        ),
                        div(style="width: 2px; height: 80px; background-color: #dee2e6; margin: 0 5px;"),
                        div(class="d-flex flex-column gap-1", style="flex: 1;",
                            actionButton(ns("sampm_am"), "AM", style="height: 42px;",
                                         class = if(rx_meds$s_ampm == "AM") "btn btn-info text-white" else "btn btn-outline-info",
                                         onclick = sprintf("Shiny.setInputValue('%s', 'AM')", ns("change_ampm"))),
                            actionButton(ns("sampm_pm"), "PM", style="height: 42px;",
                                         class = if(rx_meds$s_ampm == "PM") "btn btn-info text-white" else "btn btn-outline-info",
                                         onclick = sprintf("Shiny.setInputValue('%s', 'PM')", ns("change_ampm")))
                        )
                    )
                  } else {
                    codes <- switch(rx_meds$s_cat,
                                    "TDS/QID" = c("tid", "TDS", "QID", "5xd", "6xd", "stat", "PRN"),
                                    "Days"    = c("mon", "tue", "wed", "thu", "fri", "sat", "sun", "mwf", "tts", "FriW"),
                                    "Period"  = c("1in14", "1in21", "1in42", "1in60", "1in90", "1in120", "1in180"),
                                    "Misc"    = c("alt", "weekly", "monthly", "flu", "hepb", "rtx", "diuretic", "bd1410")
                    )
                    div(class="d-flex flex-wrap gap-1",
                        lapply(codes, function(c) {
                          actionButton(ns(paste0("fcode_", c)), c, 
                                       class = if(curr$freq == c) "btn-sm btn-primary" else "btn-sm btn-outline-primary",
                                       onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("direct_freq"), c))
                        })
                    )
                  }
              ),
              
              div(class="row g-2",
                  div(class="col-6",
                      tags$label(class="text-muted small", "Route"),
                      div(class="d-flex flex-wrap gap-1",
                          lapply(c("Oral", "SC", "IV", "IM", "TOP", "INH"), function(r) {
                            actionButton(ns(paste0("r_", r)), r, 
                                         class = paste("btn-sm", if(curr$route == r) "btn-warning" else "btn-outline-warning"),
                                         onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("set_route"), r))
                          }))
                  ),
                  div(class="col-6",
                      tags$label(class="text-muted small", "Duration"),
                      div(class="d-flex flex-wrap gap-1",
                          lapply(c("3D", "5D", "7D", "1M", "Until next visit"), function(d) {
                            actionButton(ns(paste0("d_", d)), d, 
                                         class = paste("btn-sm", if(curr$duration == d) "btn-info text-white" else "btn-outline-info"),
                                         onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("set_dur"), d))
                          }))
                  )
              )
          )
      )
    }
    
    # --- Interaction Observers ---
    observeEvent(input$change_cat, { rx_meds$s_cat <- input$change_cat })
    observeEvent(input$change_num, { rx_meds$s_num <- input$change_num })
    observeEvent(input$change_ampm, { rx_meds$s_ampm <- input$change_ampm })
    observeEvent(input$direct_freq, { rx_meds$df$freq[rx_meds$selected_idx] <- input$direct_freq })
    observeEvent(input$set_route,   { rx_meds$df$route[rx_meds$selected_idx] <- input$set_route })
    observeEvent(input$set_dur,     { rx_meds$df$duration[rx_meds$selected_idx] <- input$set_dur })
    observeEvent(input$close_edit,  { rx_meds$selected_idx <- NULL })
    
    # --- Search and History Loading ---
    load_rx_master <- function() { 
      req(pool); tryCatch({
        data <- dbGetQuery(pool, "SELECT * FROM drug_master ORDER BY brand_name ASC")
        if("id" %in% names(data)) data$id <- as.character(data$id)
        rx_master(clean_df(data))
      }, error = function(e) message(e))
    }
    observe({ load_rx_master() })
    
    load_rx_history <- function(pt_id) {
      req(pt_id, pool); tryCatch({
        res <- dbGetQuery(pool, "SELECT meds_json, visit_date FROM prescriptions WHERE patient_id::text = $1::text ORDER BY visit_date DESC LIMIT 1", list(as.character(pt_id)))
        if (nrow(res) > 0) {
          rx_meta$history_date <- format(as.Date(res$visit_date[1]), "%Y-%m-%d")
          rx_meta$is_history <- TRUE
          rx_meds$df <- clean_df(as.data.frame(jsonlite::fromJSON(res$meds_json[1])))
        } else {
          rx_meta$is_history <- FALSE
          rx_meds$df <- rx_meds$df[0,]
        }
      }, error = function(e) message(e))
    }
    
    observeEvent(current_pt(), { req(current_pt()$id); load_rx_history(current_pt()$id) })
    
    output$rx_main_container <- renderUI({
      req(current_pt())
      card(
        card_header(div(class="d-flex justify-content-between", span("Prescription Builder"), if(rx_meta$is_history) span(class="badge bg-info", rx_meta$history_date))),
        card_body(
          textInput(ns("rx_q"), "Search & Add Drug"),
          uiOutput(ns("rx_search_results")),
          hr(),
          uiOutput(ns("rx_list")), # This now contains the edit panel logic
          rHandsontableOutput(ns("rx_hot"))
        ),
        card_footer(actionButton(ns("save_rx"), "Save Prescription", class="btn-success w-100 btn-lg"))
      )
    })
    
    output$rx_search_results <- renderUI({
      req(input$rx_q); if(nchar(input$rx_q) < 2) return(NULL)
      matches <- rx_master() %>% filter(grepl(input$rx_q, brand_name, ignore.case=T)) %>% head(5)
      lapply(1:nrow(matches), function(i) {
        d <- matches[i,]
        actionButton(ns(paste0("addbtn_", i)), paste(d$brand_name, " (", d$generic, ")"), 
                     class="btn btn-sm btn-outline-secondary mb-1 w-100",
                     onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("add_rx_id"), d$id))
      })
    })
    
    observeEvent(input$add_rx_id, {
      lib_match <- rx_master() %>% filter(id == input$add_rx_id)
      new_row <- empty_rx_df()
      new_row$brand_name <- lib_match$brand_name[1]
      new_row$generic    <- lib_match$generic[1]
      new_row$dose       <- lib_match$dose[1]
      if(nzchar(lib_match$freq[1]))     new_row$freq     <- lib_match$freq[1]
      if(nzchar(lib_match$route[1]))    new_row$route    <- lib_match$route[1]
      if(nzchar(lib_match$duration[1])) new_row$duration <- lib_match$duration[1]
      rx_meds$df <- clean_df(rbind(rx_meds$df, new_row))
      rx_meds$selected_idx <- nrow(rx_meds$df)
      updateTextInput(session, "rx_q", value = "")
    })
    
    # --- INLINE RENDERING LOGIC ---
    output$rx_list <- renderUI({
      df <- rx_meds$df; if(nrow(df) == 0) return(NULL)
      
      lapply(1:nrow(df), function(i) {
        is_selected <- !is.null(rx_meds$selected_idx) && rx_meds$selected_idx == i
        
        tagList(
          # 1. The Drug Card
          div(class=paste("p-3 mb-2 border rounded shadow-sm", if(is_selected) "bg-primary-subtle border-primary" else "bg-white"),
              style="cursor:pointer", onclick=sprintf("Shiny.setInputValue('%s', %d)", ns("select_rx_card"), i),
              div(class="d-flex justify-content-between align-items-center",
                  div(strong(df$brand_name[i]), br(), 
                      span(class="badge bg-white text-dark border", df$freq[i]),
                      span(class="badge bg-white text-dark border ms-1", df$route[i]),
                      span(class="badge bg-white text-dark border ms-1", df$duration[i])),
                  actionButton(ns(paste0("del_", i)), NULL, icon("trash"), class="btn-sm btn-outline-danger border-0", 
                               onclick=sprintf("event.stopPropagation(); Shiny.setInputValue('%s', %d)", ns("del_rx_idx"), i)))
          ),
          # 2. The Inline Edit Panel (Appears only if this specific row is selected)
          if(is_selected) render_quick_edit()
        )
      })
    })
    
    observeEvent(input$select_rx_card, { rx_meds$selected_idx <- input$select_rx_card })
    observeEvent(input$del_rx_idx, { rx_meds$df <- rx_meds$df[-input$del_rx_idx, ]; rx_meds$selected_idx <- NULL })
    
    output$rx_hot <- renderRHandsontable({ req(nrow(rx_meds$df)>0); rhandsontable(rx_meds$df, rowHeaders=F) })
    observeEvent(input$rx_hot, { rx_meds$df <- clean_df(hot_to_r(input$rx_hot)) })
    
    observeEvent(input$save_rx, {
      req(current_pt()); pt_id <- as.character(current_pt()$id)
      json_data <- as.character(jsonlite::toJSON(rx_meds$df, auto_unbox = T))
      dbExecute(pool, "INSERT INTO prescriptions (patient_id, meds_json, visit_date) VALUES ($1, $2, $3) ON CONFLICT (patient_id, visit_date) DO UPDATE SET meds_json = EXCLUDED.meds_json", list(pt_id, json_data, Sys.Date()))
      showNotification("Prescription Saved Successfully")
    })
  })
}