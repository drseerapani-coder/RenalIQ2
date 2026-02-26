# --- mod_lab_ingestion.R ---
# Requires: pdftools, tesseract, httr, jsonlite, dplyr, DT, shiny, bslib, pool, DBI

# ============================================================
# UI
# ============================================================
lab_ingestion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            "AI Lab Ingestion - Master Version",
            span(icon("microscope")))
      ),
      card_body(
        fileInput(ns("file_input"), "Upload PDF or Image",
                  multiple = TRUE,
                  accept = c("image/png", "image/jpeg", "application/pdf")),
        DTOutput(ns("results_table")) %>%
          shinycssloaders::withSpinner(color = "#26A69A"),
        uiOutput(ns("action_buttons"))
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "Debug & Privacy Log",
        icon = icon("bug"),
        verbatimTextOutput(ns("debug_console"))
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================
lab_ingestion_server <- function(id, pool, current_pt, user_info, lab_targets) {
  # lab_targets: data.frame loaded from lab_targets.csv with columns:
  #   test_name, low_limit, high_limit (and any other metadata columns)
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    extracted_data <- reactiveVal(data.frame())
    debug_logs     <- reactiveVal("System Ready. Waiting for upload...")
    
    output$debug_console <- renderPrint({ cat(debug_logs()) })
    
    # ----------------------------------------------------------
    # Build the AI system prompt once, incorporating all known
    # canonical test names so the model maps to them directly.
    # ----------------------------------------------------------
    all_test_names <- lab_targets$test_name
    
    system_prompt <- paste0(
      "You are a medical laboratory report parser. ",
      "Extract ALL lab test results from the text provided. ",
      "Return ONLY a valid JSON array — no markdown, no explanation. ",
      "Each element must be a JSON object with these exact keys:\n",
      "  'raw_name'   : the test name exactly as it appears in the report (string)\n",
      "  'test_name'  : the best matching canonical name from this list — ",
      paste(all_test_names, collapse = ", "),
      " — or null if no good match exists\n",
      "  'num_val'    : the numeric result value as a JSON number, or null if non-numeric\n",
      "  'unit'       : the unit string (e.g. 'mg/dL'), or null if absent\n",
      "  'value_text' : the raw result string exactly as printed (always populated)\n",
      "  'test_date'  : the date the specimen was collected or resulted, in YYYY-MM-DD format, or null\n",
      "Do not invent values. If a field is absent from the report, use null."
    )
    
    # ----------------------------------------------------------
    # Helper: extract text from a single file path
    # Returns a named list: list(text = "...", is_image = TRUE/FALSE)
    # ----------------------------------------------------------
    extract_text_from_file <- function(path, mime) {
      is_image <- grepl("image/", mime, fixed = TRUE)
      
      if (!is_image) {
        # PDF: use pdftools for clean text extraction
        pages <- tryCatch(
          pdftools::pdf_text(path),
          error = function(e) {
            message("pdftools failed, falling back to OCR: ", e$message)
            NULL
          }
        )
        
        text <- if (!is.null(pages) && nchar(trimws(paste(pages, collapse = ""))) > 20) {
          paste(pages, collapse = "\n")
        } else {
          # Scanned PDF — render to image and OCR
          imgs <- tryCatch(pdftools::pdf_render_page(path, dpi = 300), error = function(e) NULL)
          if (!is.null(imgs)) {
            # tesseract expects a PNG path
            tmp <- tempfile(fileext = ".png")
            png::writePNG(imgs, tmp)
            tesseract::ocr(tmp)
          } else {
            ""
          }
        }
        list(text = text, is_image = FALSE)
      } else {
        list(text = NULL, is_image = TRUE, path = path)
      }
    }
    
    # ----------------------------------------------------------
    # Helper: call OpenAI API
    #   - text mode : standard chat completions with raw_text
    #   - image mode: vision API with base64-encoded image
    # ----------------------------------------------------------
    call_openai <- function(system_prompt, file_info) {
      api_key <- Sys.getenv("OPENAI_API_KEY")
      if (nchar(api_key) == 0) stop("OPENAI_API_KEY environment variable is not set.")
      
      if (!file_info$is_image) {
        # ---- Text / PDF mode ----
        body <- list(
          model       = "gpt-4o-mini",
          temperature = 0,
          messages    = list(
            list(role = "system", content = system_prompt),
            list(role = "user",   content = paste("Lab report text:\n\n", file_info$text))
          )
        )
      } else {
        # ---- Image mode: use gpt-4o vision ----
        img_bytes  <- readBin(file_info$path, "raw", n = file.info(file_info$path)$size)
        img_b64    <- base64enc::base64encode(img_bytes)
        ext        <- tolower(tools::file_ext(file_info$path))
        mime_type  <- if (ext == "png") "image/png" else "image/jpeg"
        
        body <- list(
          model       = "gpt-4o",   # vision requires gpt-4o (not mini)
          temperature = 0,
          messages    = list(
            list(role = "system", content = system_prompt),
            list(
              role    = "user",
              content = list(
                list(type = "text",      text = "Extract all lab results from this report image:"),
                list(type = "image_url", image_url = list(
                  url    = paste0("data:", mime_type, ";base64,", img_b64),
                  detail = "high"
                ))
              )
            )
          )
        )
      }
      
      res <- httr::POST(
        url     = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(Authorization = paste("Bearer", api_key)),
        body    = body,
        encode  = "json"
      )
      
      if (httr::http_error(res)) {
        stop("OpenAI API error: ", httr::content(res, as = "text", encoding = "UTF-8"))
      }
      
      httr::content(res)$choices[[1]]$message$content
    }
    
    # ----------------------------------------------------------
    # Helper: parse AI JSON output into a clean data frame
    # ----------------------------------------------------------
    parse_ai_output <- function(ai_text, lab_targets) {
      clean_json <- gsub("```json|```", "", ai_text) %>% trimws()
      
      raw_batch <- tryCatch(
        {
          parsed <- jsonlite::fromJSON(clean_json, flatten = TRUE)
          if (is.list(parsed) && !is.data.frame(parsed)) as.data.frame(parsed)
          else parsed
        },
        error = function(e) {
          message("JSON parse error: ", e$message, "\nRaw AI output:\n", ai_text)
          NULL
        }
      )
      
      if (is.null(raw_batch) || !is.data.frame(raw_batch) || nrow(raw_batch) == 0) {
        return(data.frame())
      }
      
      # Ensure required columns exist
      for (col in c("raw_name", "test_name", "num_val", "unit", "value_text", "test_date")) {
        if (!(col %in% names(raw_batch))) raw_batch[[col]] <- NA_character_
      }
      
      # ---- Regex fallback: only fires when AI test_name is NA / unrecognised ----
      processed_batch <- raw_batch %>%
        mutate(
          clean_name = tolower(coalesce(as.character(raw_name), "")),
          
          test_name = case_when(
            # Use AI-provided canonical name if it matches our list
            !is.na(test_name) & test_name %in% lab_targets$test_name ~ test_name,
            
            # Regex safety-net for common problem cases
            grepl("albumin.*creatinine|\\bacr\\b", clean_name)         ~ "Urine Microalbumin Creatinine Ratio (UMACR)",
            grepl("protein.*creatinine|\\bupcr\\b|\\bpcr\\b", clean_name) ~ "Urine Protein Creatinine Ratio (UPCR)",
            grepl("albumin", clean_name) & grepl("urine", clean_name)  ~ "Urine Albumin",
            grepl("creatinine", clean_name) & grepl("urine", clean_name) ~ "Urine Creatinine",
            
            TRUE ~ NA_character_   # Unknown → will be dropped by inner_join
          )
        ) %>%
        select(-clean_name)
      
      # ---- Validate: keep only tests in lab_targets ----
      validated_batch <- processed_batch %>%
        filter(!is.na(test_name)) %>%
        inner_join(lab_targets, by = "test_name") %>%
        distinct(test_name, test_date, num_val, .keep_all = TRUE) %>%
        mutate(
          num_val   = suppressWarnings(as.numeric(num_val)),
          test_date = suppressWarnings(as.character(as.Date(test_date))),
          status    = case_when(
            is.na(num_val)                                     ~ "TEXT",
            !is.na(high_limit) & num_val > high_limit          ~ "HIGH",
            !is.na(low_limit)  & num_val < low_limit           ~ "LOW",
            TRUE                                               ~ "NORMAL"
          )
        )
      
      validated_batch
    }
    
    # ----------------------------------------------------------
    # Main event: file uploaded
    # ----------------------------------------------------------
    observeEvent(input$file_input, {
      req(input$file_input, current_pt())
      
      files       <- input$file_input
      all_results <- list()
      logs        <- character(0)
      
      withProgress(message = "AI Lab Extraction in Progress...", value = 0, {
        for (i in seq_len(nrow(files))) {
          incProgress(1 / nrow(files), detail = paste("File", i, "of", nrow(files)))
          
          path      <- files$datapath[i]
          file_name <- files$name[i]
          mime      <- files$type[i]
          
          logs <- c(logs, paste0("\n--- Processing: ", file_name, " ---"))
          
          tryCatch({
            # Step 1: Extract text (or prepare image reference)
            file_info <- extract_text_from_file(path, mime)
            
            if (!file_info$is_image) {
              char_count <- nchar(file_info$text)
              logs <- c(logs, paste("  Text extracted:", char_count, "characters"))
              if (char_count < 20) {
                logs <- c(logs, "  WARNING: Very little text found. Check if PDF is scanned.")
              }
            } else {
              logs <- c(logs, "  Image file detected. Using vision API.")
            }
            
            # Step 2: Call AI
            ai_raw <- call_openai(system_prompt, file_info)
            logs   <- c(logs, paste("  AI response length:", nchar(ai_raw), "chars"))
            
            # Step 3: Parse & validate
            result_df <- parse_ai_output(ai_raw, lab_targets)
            logs      <- c(logs, paste("  Validated rows:", nrow(result_df)))
            
            if (nrow(result_df) > 0) {
              all_results[[i]] <- result_df
            }
            
          }, error = function(e) {
            logs <<- c(logs, paste("  ERROR:", e$message))
            message("Lab ingestion error on file ", file_name, ": ", e$message)
          })
        }
      })
      
      debug_logs(paste(logs, collapse = "\n"))
      
      final_df <- if (length(all_results) > 0) bind_rows(all_results) else data.frame()
      extracted_data(final_df)
      
      if (nrow(final_df) == 0) {
        showNotification(
          "No recognised lab values were extracted. Check the debug log for details.",
          type = "warning", duration = 8
        )
      }
    })
    
    # ----------------------------------------------------------
    # Results table with conditional row colouring
    # ----------------------------------------------------------
    output$results_table <- renderDT({
      req(nrow(extracted_data()) > 0)
      
      display_cols <- c("test_name", "num_val", "unit", "value_text",
                        "test_date", "low_limit", "high_limit", "status")
      display_cols <- intersect(display_cols, names(extracted_data()))
      
      datatable(
        extracted_data()[, display_cols, drop = FALSE],
        options  = list(dom = "t", pageLength = 100),
        rownames = FALSE
      ) %>%
        formatStyle(
          "num_val",
          valueColumns = "status",
          color        = styleEqual(
            c("HIGH", "LOW",    "NORMAL", "TEXT"),
            c("red",  "#1565C0", "inherit", "inherit")
          ),
          fontWeight = styleEqual(
            c("HIGH", "LOW"),
            c("bold",  "bold")
          )
        )
    })
    
    # ----------------------------------------------------------
    # Action buttons (shown only after extraction)
    # ----------------------------------------------------------
    output$action_buttons <- renderUI({
      req(nrow(extracted_data()) > 0)
      div(class = "mt-3 d-flex gap-2",
          actionButton(ns("save"),  "Confirm & Save to DB", class = "btn-success w-100"),
          actionButton(ns("clear"), "Discard",              class = "btn-outline-danger"))
    })
    
    # ----------------------------------------------------------
    # Save to database (transaction + deduplication)
    # ----------------------------------------------------------
    observeEvent(input$save, {
      req(nrow(extracted_data()) > 0, current_pt(), user_info())
      
      curr_user  <- user_info()$username
      pt_id_str  <- as.character(current_pt()$id)
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con), add = TRUE)
      
      transaction_active <- FALSE
      
      tryCatch({
        # A. Prepare upload queue
        save_df <- extracted_data()
        
        for (col in c("num_val", "unit", "value_text", "test_date")) {
          if (!(col %in% names(save_df))) save_df[[col]] <- NA
        }
        
        upload_queue <- save_df %>%
          mutate(
            patient_id = pt_id_str,
            created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            created_by = curr_user,
            num_val    = suppressWarnings(as.numeric(num_val)),
            test_date  = as.character(as.Date(test_date))
          ) %>%
          select(patient_id, test_name, num_val, unit, value_text,
                 test_date, created_at, created_by)
        
        # B. Server-side deduplication
        existing <- DBI::dbGetQuery(
          con,
          "SELECT test_name, test_date::text, num_val::numeric FROM labs WHERE patient_id = $1",
          list(pt_id_str)
        )
        
        if (nrow(existing) > 0) {
          existing$test_date <- as.character(as.Date(existing$test_date))
          existing$num_val   <- as.numeric(existing$num_val)
          upload_queue <- anti_join(upload_queue, existing,
                                    by = c("test_name", "test_date", "num_val"))
        }
        
        # C. Insert within transaction
        if (nrow(upload_queue) > 0) {
          DBI::dbExecute(con, "BEGIN")
          transaction_active <- TRUE
          
          for (i in seq_len(nrow(upload_queue))) {
            DBI::dbExecute(con,
                           "INSERT INTO labs
                 (patient_id, test_name, num_val, unit, value_text,
                  test_date, created_at, created_by)
               VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
                           list(
                             upload_queue$patient_id[i], upload_queue$test_name[i],
                             upload_queue$num_val[i],    upload_queue$unit[i],
                             upload_queue$value_text[i], upload_queue$test_date[i],
                             upload_queue$created_at[i], upload_queue$created_by[i]
                           )
            )
          }
          
          # D. Audit — log failure but don't roll back the save
          audit_ok <- tryCatch({
            log_audit(con, curr_user, "LAB_AI_SAVE", "labs", pt_id_str)
            TRUE
          }, error = function(e) {
            message("AUDIT LOG FAILURE for user=", curr_user,
                    " patient=", pt_id_str, ": ", e$message)
            FALSE
          })
          
          DBI::dbExecute(con, "COMMIT")
          transaction_active <- FALSE
          
          msg <- paste0("Saved ", nrow(upload_queue), " record(s).",
                        if (!audit_ok) " [Audit log failed — check server logs]" else "")
          showNotification(msg, type = if (audit_ok) "message" else "warning")
          extracted_data(data.frame())
          
        } else {
          showNotification("No new records to save (all duplicates).", type = "warning")
        }
        
      }, error = function(e) {
        if (transaction_active) {
          tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(re) NULL)
        }
        message("LAB SAVE ERROR for patient=", pt_id_str, ": ", e$message)
        showNotification(paste("Database Error:", e$message), type = "error", duration = 15)
      })
    })
    
    observeEvent(input$clear, {
      extracted_data(data.frame())
      debug_logs("Cleared. Ready for next upload.")
    })
  })
}
