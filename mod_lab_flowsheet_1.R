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
      open = TRUE, # Set to TRUE so you can see errors immediately
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
    
    extracted_data <- reactiveVal(data.frame())
    debug_logs <- reactiveVal("System Ready. Waiting for upload...")
    
    output$debug_console <- renderPrint({ cat(debug_logs()) })
    
    observeEvent(input$file_input, {
      req(input$file_input, current_pt())
      
      # 1. HARDENED SYSTEM PROMPT
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
          
          # Text Extraction
          tryCatch({
            debug_logs(paste0(debug_logs(), "\nProcessing file: ", files$name[i]))
            
            raw_text <- if(grepl("pdf", files$type[i], ignore.case = TRUE)) {
              paste(pdftools::pdf_text(path), collapse = "\n\n")
            } else {
              tesseract::ocr(path)
            }
            raw_text <- iconv(raw_text, "UTF-8", "ASCII", sub="")
            
            # AI API Call
            incProgress(0.3, detail = "Consulting OpenAI...")
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
            
            if (httr::status_code(res) != 200) {
              stop(paste("API Error:", httr::content(res, "text")))
            }
            
            # Parsing & Regex Bucketing
            ai_out <- httr::content(res)$choices[[1]]$message$content
            clean_json <- gsub("```json|```", "", ai_out) %>% trimws()
            raw_batch <- jsonlite::fromJSON(clean_json)
            
            if (is.data.frame(raw_batch) && nrow(raw_batch) > 0) {
              processed_batch <- raw_batch %>%
                mutate(
                  source_clean = tolower(ifelse(is.na(source), "serum", source)),
                  clean_name = tolower(raw_name),
                  
                  # THE BUCKETING LOGIC
                  test_name = case_when(
                    grepl("albumin.*creatinine|acr", clean_name) ~ "Urine Microalbumin Creatinine Ratio (UMACR)",
                    grepl("protein.*creatinine|upcr|pcr", clean_name) ~ "Urine Protein Creatinine Ratio (UPCR)",
                    grepl("creatinine", clean_name) & grepl("urine", source_clean) ~ "Urine Creatinine",
                    grepl("albumin", clean_name) & grepl("urine", source_clean) ~ "Urine Albumin",
                    grepl("creatinine", clean_name) & !grepl("urine", source_clean) ~ "Creatinine",
                    TRUE ~ raw_name
                  ),
                  # Ensure date format is clean for R
                  test_date = as.character(as.Date(test_date))
                )
              
              # Join with lab_targets.csv (Ensure this is loaded globally)
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
            debug_logs(paste0(debug_logs(), "\nFATAL ERROR in file ", i, ": ", e$message))
            warning("Ingestion Error: ", e$message)
          })
        }
        
        final_df <- bind_rows(all_results)
        extracted_data(final_df)
        if(nrow(final_df) == 0) debug_logs(paste0(debug_logs(), "\nWarning: Extraction finished but 0 rows matched your lab_targets.csv"))
      })
    })
    
    # [Results Table and Action Button render logic remains same as previous version]
    output$results_table <- renderDT({
      req(nrow(extracted_data()) > 0)
      datatable(extracted_data(), options = list(dom = 't', pageLength = 100), rownames = FALSE) %>%
        formatStyle('status', target = 'row',
                    backgroundColor = styleEqual(c('HIGH', 'LOW'), c('#ffdad9', '#fff4d1')))
    })
    
    output$action_buttons <- renderUI({
      req(nrow(extracted_data()) > 0)
      div(class = "mt-3 d-flex gap-2",
          actionButton(ns("save"), "Confirm & Save", class = "btn-success w-100"),
          actionButton(ns("clear"), "Discard", class = "btn-outline-danger"))
    })
  })
}