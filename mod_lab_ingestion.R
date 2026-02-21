# 1. Load configuration from CSV (Run at app startup)
# Ensure lab_targets.csv is in your project directory
Sys.getenv("OPENAI_API_KEY")
lab_targets <- read.csv("lab_targets.csv", stringsAsFactors = FALSE)
all_test_names <- lab_targets$test_name
# --- mod_lab_ingestion.R ---

lab_ingestion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        div(class="d-flex justify-content-between align-items-center",
            "AI Lab Ingestion - Master Version",
            span(icon("microscope")))
      ),
      card_body(
        # File uploader for PDFs/Images
        fileInput(ns("file_input"), "Upload PDF or Image", 
                  multiple = TRUE, 
                  accept = c('image/png', 'image/jpeg', 'application/pdf')),
        
        # Results table with a loading spinner
        DTOutput(ns("results_table")) %>% 
          shinycssloaders::withSpinner(color="#26A69A"),
        
        # This will contain the "Confirm" and "Discard" buttons after extraction
        uiOutput(ns("action_buttons"))
      )
    ),
    
    # Debug section to see what the AI is thinking
    accordion(
      open = FALSE,
      accordion_panel(
        "Loud Debug & Privacy Log",
        icon = icon("bug"),
        verbatimTextOutput(ns("debug_console"))
      )
    )
  )
}
# 3. Server Function
lab_ingestion_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    extracted_data <- reactiveVal(data.frame())
    debug_logs <- reactiveVal("System Ready. Waiting for upload...")
    
    output$debug_console <- renderPrint({ cat(debug_logs()) })
    
    observeEvent(input$file_input, {
      req(input$file_input, current_pt())
      
      debug_logs(paste0("\n--- New Upload: ", Sys.time(), " ---"))
      files <- input$file_input
      all_results <- list()
      
      withProgress(message = 'AI Analysis in Progress...', value = 0, {
        for (i in 1:nrow(files)) {
          path <- files$datapath[i]
          
          tryCatch({
            # STEP 1: TEXT EXTRACTION & SANITIZATION
            raw_text <- if(grepl("pdf", files$type[i], ignore.case = TRUE)) {
              debug_logs(paste0(debug_logs(), "\nReading PDF: ", files$name[i]))
              txt <- paste(pdftools::pdf_text(path), collapse = "\n\n")
              iconv(txt, "UTF-8", "ASCII", sub="") # Clean non-ASCII
            } else {
              debug_logs(paste0(debug_logs(), "\nOCR Image: ", files$name[i]))
              tesseract::ocr(path)
            }
            
            # STEP 2: DIRECT HTTR CALL
            # STEP 2: DIRECT HTTR CALL
            incProgress(0.5, detail = "Consulting AI...")
            
            # STEP 2: REFINED AI REQUEST
            res <- httr::POST(
              url = "https://api.openai.com/v1/chat/completions",
              httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
              httr::content_type_json(),
              body = list(
                model = "gpt-4o-mini",
                messages = list(
                  # Update the system prompt in mod_lab_ingestion.R
                  list(role = "system", content = "You are a medical data parser. 
      Extract data into a JSON array using these EXACT keys:
      - 'test_name': Use provided list names.
      - 'num_val': The number ONLY as a decimal (e.g., 23.0).
      - 'unit': The unit ONLY (e.g., 'mg/dl').
      - 'test_date': YYYY-MM-DD.
      If a test is non-numeric (e.g. 'Normal'), put it in 'value_text' and leave 'num_val' null.
      Return RAW JSON only."),
                  list(role = "user", content = paste("Target List:", paste(all_test_names, collapse=", "), "\n\nText:", raw_text))
                ),
                temperature = 0
              ),
              encode = "json"
            )
            
            # STEP 3: HANDLE API FAILURES
            if (httr::status_code(res) != 200) {
              err <- httr::content(res, "text")
              debug_logs(paste0(debug_logs(), "\nAPI ERROR: ", err))
              next
            }
            
            # STEP 4: PARSE & CLEAN (THE BACKTICK FIX)
            # STEP 4: ZERO-LOGIC PARSING
            ai_out <- httr::content(res)$choices[[1]]$message$content
            clean_json <- gsub("```json|```", "", ai_out) %>% trimws()
            batch <- jsonlite::fromJSON(clean_json)
            
            if (is.data.frame(batch) && nrow(batch) > 0) {
              # Standardize column names to lowercase just in case
              names(batch) <- tolower(names(batch))
              
              # Join with your CSV limits
              batch <- batch %>%
                inner_join(lab_targets, by = "test_name") %>%
                mutate(
                  # AI already gave us num_val, so we just use it!
                  status = case_when(
                    is.na(num_val) ~ "TEXT",
                    !is.na(high_limit) & num_val > high_limit ~ "HIGH",
                    !is.na(low_limit) & num_val < low_limit ~ "LOW",
                    TRUE ~ "NORMAL"
                  )
                )
              
              all_results[[i]] <- batch
            }
          }, error = function(e) {
            debug_logs(paste0(debug_logs(), "\nProcessing Error: ", e$message))
          })
        }
        
        final_df <- bind_rows(all_results)
        extracted_data(final_df)
      })
    })
    
    # Render results with conditional formatting
    output$results_table <- renderDT({
      req(nrow(extracted_data()) > 0)
      datatable(extracted_data(), options = list(dom = 't', pageLength = 100), rownames = FALSE) %>%
        formatStyle('status', target = 'row',
                    backgroundColor = styleEqual(c('HIGH', 'LOW'), c('#ffdad9', '#fff4d1')))
    })
    
    output$action_buttons <- renderUI({
      req(nrow(extracted_data()) > 0)
      div(class = "mt-3 d-flex gap-2",
          actionButton(ns("save"), "Confirm & Save to DB", class = "btn-success w-100"),
          actionButton(ns("clear"), "Discard", class = "btn-outline-danger"))
    })
    
    # DATABASE SAVE WITH DUPLICATE CHECK
    # --- 3. THE IRONCLAD SAVE LOGIC ---
    observeEvent(input$save, {
      req(extracted_data(), current_pt(), user_info())
      
      # Identity & Metadata
      curr_user <- user_info()$username
      pt_id_str <- as.character(current_pt()$id)
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      transaction_active <- FALSE
      
      tryCatch({
        # A. PREPARE QUEUE (Clean data types strictly)
        save_df <- extracted_data()
        
        # Ensure all columns exist before processing
        cols <- c("num_val", "unit", "value_text", "test_date")
        for(c in cols) if(!(c %in% names(save_df))) save_df[[c]] <- NA
        
        upload_queue <- save_df %>%
          mutate(
            patient_id = pt_id_str,
            created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            created_by = curr_user,
            num_val = suppressWarnings(as.numeric(num_val)),
            test_date = as.character(as.Date(test_date)) # SQL ready string
          ) %>%
          select(patient_id, test_name, num_val, unit, value_text, test_date, created_at, created_by)
        
        # B. DEDUPLICATION (Fetch current patient labs)
        existing <- DBI::dbGetQuery(con, "SELECT test_name, test_date, num_val FROM labs WHERE patient_id = $1", list(pt_id_str))
        
        if (nrow(existing) > 0) {
          existing$test_date <- as.character(as.Date(existing$test_date))
          existing$num_val <- as.numeric(existing$num_val)
          upload_queue <- anti_join(upload_queue, existing, by = c("test_name", "test_date", "num_val"))
        }
        
        # C. EXECUTE TRANSACTION
        if (nrow(upload_queue) > 0) {
          DBI::dbExecute(con, "BEGIN")
          transaction_active <- TRUE
          
          for(i in 1:nrow(upload_queue)) {
            DBI::dbExecute(con, "
              INSERT INTO labs (patient_id, test_name, num_val, unit, value_text, test_date, created_at, created_by) 
              VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
                           list(
                             upload_queue$patient_id[i],
                             upload_queue$test_name[i],
                             upload_queue$num_val[i],
                             upload_queue$unit[i],
                             upload_queue$value_text[i],
                             upload_queue$test_date[i],
                             upload_queue$created_at[i],
                             upload_queue$created_by[i]
                           )
            )
          }
          
          # D. AUDIT & COMMIT
          # Safely wrap audit in its own try so it doesn't break the main save
          try(log_audit(con, curr_user, "LAB_AI_SAVE", "labs", pt_id_str), silent = TRUE)
          
          DBI::dbExecute(con, "COMMIT")
          transaction_active <- FALSE
          
          showNotification(paste("Success: Saved", nrow(upload_queue), "records."), type = "message")
          extracted_data(data.frame()) # Clear UI
        } else {
          showNotification("No new records found (Duplicates skipped).", type = "warning")
        }
        
      }, error = function(e) {
        if (transaction_active) DBI::dbExecute(con, "ROLLBACK")
        message("--- LAB SAVE ERROR ---")
        message(e$message)
        showNotification(paste("Database Error:", e$message), type = "error")
      })
    })

    
    observeEvent(input$clear, { extracted_data(data.frame()) })
  })
}