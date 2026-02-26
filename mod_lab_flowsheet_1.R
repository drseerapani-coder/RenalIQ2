# --- UI Function ---
lab_ingestion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        div(class="d-flex justify-content-between align-items-center",
            "AI Lab Ingestion - Source-Aware Master",
            span(icon("microscope")))
      ),
      card_body(
        fileInput(ns("file_input"), "Upload PDF or Image", 
                  multiple = TRUE, 
                  accept = c('image/png', 'image/jpeg', 'application/pdf')),
        
        DTOutput(ns("results_table")) %>% 
          shinycssloaders::withSpinner(color="#26A69A"),
        
        uiOutput(ns("action_buttons"))
      )
    ),
    
    accordion(
      open = TRUE, 
      accordion_panel(
        "Live Debug Console",
        icon = icon("bug"),
        verbatimTextOutput(ns("debug_console"))
      )
    )
  )
}

# --- Server Function ---
lab_ingestion_server <- function(id, pool, current_pt, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive State ---
    extracted_data <- reactiveVal(data.frame())
    debug_logs <- reactiveVal("System Ready. Waiting for upload...")
    
    output$debug_console <- renderPrint({ cat(debug_logs()) })
    
    # 1. AI EXTRACTION LOGIC
    observeEvent(input$file_input, {
      req(input$file_input, current_pt())
      
      system_prompt_raw <- "You are a medical data extractor. 
      OBJECTIVE: Extract EVERY lab parameter. 
      RULES:
      1. SPECIMEN SOURCE: Identify if the test is 'Serum' or 'Urine' based on headers. Key: 'source'.
      2. RATIOS: Extract 'Albumin: Creatinine' or 'ACR' as one entry.
      3. FORMAT: RAW JSON array with keys: 'raw_name', 'num_val', 'unit', 'test_date', 'source', 'value_text'.
      Return RAW JSON only."
      
      files <- input$file_input
      all_results <- list()
      
      debug_logs(paste0("\n--- Start Processing: ", Sys.time(), " ---"))
      
      withProgress(message = 'AI Analysis & Source Bucketing...', value = 0, {
        for (i in 1:nrow(files)) {
          path <- files$datapath[i]
          
          tryCatch({
            debug_logs(paste0(debug_logs(), "\nProcessing file: ", files$name[i]))
            
            raw_text <- if(grepl("pdf", files$type[i], ignore.case = TRUE)) {
              paste(pdftools::pdf_text(path), collapse = "\n\n")
            } else {
              tesseract::ocr(path)
            }
            raw_text <- iconv(raw_text, "UTF-8", "ASCII", sub="")
            
            incProgress(0.3, detail = "Consulting AI...")
            res <- httr::POST(
              url = "https://api.openai.com/v1/chat/completions",
              httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
              httr::content_type_json(),
              body = list(
                model = "gpt-4o-mini",
                messages = list(
                  list(role = "system", content = system_prompt_raw),
                  list(role = "user", content = paste("Text:", raw_text))
                ),
                temperature = 0
              ),
              encode = "json"
            )
            
            if (httr::status_code(res) != 200) stop(paste("API Error:", httr::content(res, "text")))
            
            ai_out <- httr::content(res)$choices[[1]]$message$content
            clean_json <- gsub("```json|```", "", ai_out) %>% trimws()
            raw_batch <- jsonlite::fromJSON(clean_json)
            
            if (is.data.frame(raw_batch) && nrow(raw_batch) > 0) {
              processed_batch <- raw_batch %>%
                mutate(
                  source_clean = tolower(ifelse(is.na(source) | source == "", "serum", source)),
                  clean_name = tolower(raw_name),
                  test_name = case_when(
                    grepl("albumin.*creatinine|acr", clean_name) ~ "Urine Microalbumin Creatinine Ratio (UMACR)",
                    grepl("protein.*creatinine|upcr|pcr", clean_name) ~ "Urine Protein Creatinine Ratio (UPCR)",
                    grepl("creatinine", clean_name) & grepl("urine", source_clean) ~ "Urine Creatinine",
                    grepl("albumin", clean_name) & grepl("urine", source_clean) ~ "Urine Albumin",
                    grepl("creatinine", clean_name) & !grepl("urine", source_clean) ~ "Creatinine",
                    TRUE ~ raw_name
                  ),
                  test_date = as.character(as.Date(test_date))
                )
              
              # Join with lab_targets (assumed loaded in global.R)
              validated_batch <- processed_batch %>%
                inner_join(lab_targets, by = "test_name") %>%
                distinct(test_name, test_date, num_val, .keep_all = TRUE) %>%
                mutate(
                  status = case_when(
                    is.na(num_val) ~ "TEXT",
                    !is.na(high_limit) & num_val > high_limit ~ "HIGH",
                    !is.na(low_limit) & num_val < low_limit ~ "LOW",
                    TRUE ~ "NORMAL"
                  )
                )
              
              all_results[[i]] <- validated_batch
              debug_logs(paste0(debug_logs(), "\nSuccess: Extracted ", nrow(validated_batch), " rows."))
            }
          }, error = function(e) {
            debug_logs(paste0(debug_logs(), "\nFATAL ERROR: ", e$message))
          })
        }
        
        final_df <- bind_rows(all_results)
        extracted_data(final_df)
      })
    })
    
    # 2. CHECK DATABASE FOR EXISTING DATA
    observe({
      req(nrow(extracted_data()) > 0)
      dates <- unique(extracted_data()$test_date)
      pt_id <- as.character(current_pt()$id)
      
      existing <- dbGetQuery(pool, 
                             "SELECT test_name, test_date, num_val FROM patient_labs 
         WHERE patient_id::text = $1 AND test_date = ANY($2::date[])",
                             list(pt_id, dates))
      
      if(nrow(existing) > 0) {
        debug_logs(paste0(debug_logs(), "\nALERT: Found ", nrow(existing), 
                          " existing records in DB for these dates. Review for duplicates."))
      }
    })
    
    # Handle manual edits in the browser and update the R dataframe
    observeEvent(input$results_table_cell_edit, {
      info <- input$results_table_cell_edit
      df <- extracted_data()
      
      # DT (JS) is 0-indexed; R is 1-indexed. Adjust column index.
      # info$row is the row number, info$col is the column index (0, 1, 2...)
      df[info$row, info$col + 1] <- info$value 
      
      # Recalculate status if the numerical value was edited
      if(names(df)[info$col + 1] == "num_val") {
        df <- df %>%
          mutate(status = case_when(
            is.na(num_val) ~ "TEXT",
            !is.na(high_limit) & num_val > high_limit ~ "HIGH",
            !is.na(low_limit) & num_val < low_limit ~ "LOW",
            TRUE ~ "NORMAL"
          ))
      }
      
      # Update the reactive value so the Save button sees the changes
      extracted_data(df) 
    })
    # 4. RENDER TABLE (GLITCH FIX)
    output$results_table <- renderDT({
      req(nrow(extracted_data()) > 0)
      datatable(
        extracted_data(), 
        editable = "cell", # Enable editing
        rownames = FALSE,
        options = list(dom = 't', pageLength = 100, scrollX = TRUE)
      ) %>%
        formatStyle('status', target = 'row',
                    backgroundColor = styleEqual(c('HIGH', 'LOW'), c('#ffdad9', '#fff4d1')))
    })
    
    
    
    # 5. ACTIONS
    output$action_buttons <- renderUI({
      req(nrow(extracted_data()) > 0)
      div(class = "mt-3 d-flex gap-2",
          actionButton(ns("save"), "Confirm & Save", class = "btn-success w-100"),
          actionButton(ns("clear"), "Discard", class = "btn-outline-danger"))
    })
    
    observeEvent(input$clear, { extracted_data(data.frame()) })
    
    observeEvent(input$save, {
      # 1. Capture and Clean Data
      raw_df <- extracted_data()
      req(nrow(raw_df) > 0, current_pt())
      
      # Crucial: Convert to a standard data frame and force types
      # This prevents tibble-related issues with dbExecute
      df_to_save <- as.data.frame(raw_df) 
      
      pt_id <- as.character(current_pt()$id)
      
      withProgress(message = 'Saving to Database...', value = 0, {
        tryCatch({
          pool::dbWithTransaction(pool, {
            for (i in 1:nrow(df_to_save)) {
              
              # Prepare parameters: Force types and handle NAs
              # Using 'as.numeric' ensures R's NA becomes a SQL NULL properly
              params <- list(
                pt_id,
                as.character(df_to_save$test_name[i]),
                as.character(df_to_save$test_date[i]),
                as.numeric(df_to_save$num_val[i]), 
                as.character(df_to_save$unit[i]),
                as.character(df_to_save$source_clean[i]),
                as.character(df_to_save$value_text[i]),
                as.character(df_to_save$status[i])
              )
              
              # Replace any NULL/NaN in the list with NA for the DB driver
              params <- lapply(params, function(x) if(length(x) == 0 || is.null(x)) NA else x)
              
              dbExecute(pool, 
                        "INSERT INTO patient_labs (
              patient_id, test_name, test_date, num_val, unit, 
              source, value_text, status
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            ON CONFLICT (patient_id, test_name, test_date) 
            DO UPDATE SET 
              num_val = EXCLUDED.num_val,
              status = EXCLUDED.status,
              value_text = EXCLUDED.value_text",
                        params = params
              )
              incProgress(1/nrow(df_to_save))
            }
          })
          
          showNotification("Success: All lab results saved.", type = "message")
          debug_logs(paste0(debug_logs(), "\nDATABASE UPDATE: Success at ", Sys.time()))
          extracted_data(data.frame()) # Clear UI
          
        }, error = function(e) {
          # This will now catch specific SQL syntax or type errors
          showNotification(paste("Database Error:", e$message), type = "error")
          debug_logs(paste0(debug_logs(), "\nDATABASE ERROR: ", e$message))
          print(e$message) # Check your R console for the full traceback
        })
      })
    })
    
  })
}