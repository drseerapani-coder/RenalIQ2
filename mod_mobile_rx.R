# mod_mobile_rx.R

mobile_rx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* High-Contrast Card Styling */
      .rx-card {
        transition: all 0.2s; border-left: 5px solid #f39c12; background: #fff;
        border-radius: 8px !important; margin-bottom: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .rx-card.selected { border-left: 5px solid #2c3e50; background-color: #fff9f0 !important; }
      .rx-brand-name { font-size: 1.15rem; font-weight: 800; color: #1a252f; text-transform: uppercase; }
      .rx-generic-name { font-size: 0.85rem; color: #e67e22; font-weight: 600; font-style: italic; margin-bottom: 4px; }
      .rx-badge {
        font-weight: 700; font-size: 0.7rem; padding: 4px 8px; border-radius: 4px;
        background: #fff; border: 1px solid #f39c12; color: #2c3e50; margin-right: 4px; display: inline-block;
      }
      .stitch-grid {
        display: grid;
        grid-template-columns: repeat(6, 1fr);
        gap: 10px;
        margin-bottom: 12px;
        flex-grow: 1;
      }
      .btn-stitch {
        height: 48px;
        min-width: 42px;
        font-weight: 800;
        border-radius: 6px;
        padding: 5px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .override-row {
        background: #fef5e7; padding: 10px; border-radius: 0 0 8px 8px;
        border-top: 1px dashed #f39c12; margin-top: 5px;
      }
      .ampm-col { display: flex; flex-direction: column; gap: 4px; min-width: 55px; }
      .label-mini { font-size: 0.75rem; font-weight: bold; color: #7f8c8d; display: block; margin-bottom: 2px; }
    ")),
    uiOutput(ns("rx_main_container"))
  )
}

mobile_rx_server <- function(id, pool, current_pt, user_info,
                             rx_output_dir  = "",          # local folder (dev — PAD watches directly)
                             gcs_enabled    = FALSE,       # Google Cloud Storage (production on DO)
                             gcs_bucket     = "",          # GCS bucket name
                             freq_cats      = list(),      # category → UPPERCASE codes
                             freq_label_map = character()) {  # UPPERCASE code → label
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- State ---
    rx_master    <- reactiveVal(data.frame())
    save_trigger <- reactiveVal(0)
    edit_drug_rv <- reactiveVal(NULL)   # holds the drug row currently being edited
    rx_meds <- reactiveValues(
      df = data.frame(brand_name=character(), generic=character(), dose=character(),
                      freq=character(), route=character(), duration=character(),
                      stringsAsFactors=FALSE),
      selected_idx = NULL, s_cat = "OD", s_num = "8", s_ampm = "AM"
    )
    rx_meta <- reactiveValues(is_history = FALSE, history_date = NULL)

    EXPECTED_COLS <- c("brand_name", "generic", "dose", "freq", "route", "duration")

    clean_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(brand_name=character(), generic=character(), dose=character(),
                          freq=character(), route=character(), duration=character(),
                          stringsAsFactors=FALSE))
      }
      df[] <- lapply(df, function(x) { x[is.na(x)] <- ""; as.character(x) })
      return(as.data.frame(df))
    }

    # Normalise column names when loading JSON from DB.
    # Guards against legacy column-name drift and ensures only the 6 expected
    # columns are kept, in the canonical order.
    normalize_rx_cols <- function(df) {
      # Map known legacy/alternate names → canonical name
      legacy <- c(
        drug_name  = "brand_name", name       = "brand_name",
        medication = "brand_name", drug       = "brand_name",
        frequency  = "freq",       interval   = "freq",
        duration_days = "duration"
      )
      for (old in names(legacy)) {
        new <- legacy[[old]]
        if (old %in% names(df) && !(new %in% names(df)))
          names(df)[names(df) == old] <- new
      }
      # Add any still-missing columns as empty strings
      for (col in EXPECTED_COLS) {
        if (!(col %in% names(df))) df[[col]] <- ""
      }
      # Return only the 6 canonical columns in order (drop id, timestamps, etc.)
      df[EXPECTED_COLS]
    }

    # --- Helpers ---

    # Fix 7: Infer route from drug name keywords
    infer_route <- function(drug_name) {
      n <- tolower(trimws(drug_name))
      if (grepl("\\binj\\.?|\\binjection\\b|\\binfusion\\b|\\bvial\\b|\\bampoule\\b|\\bamp\\b|\\biv\\b", n)) return("IV")
      if (grepl("\\bsc\\b|\\bsubcut\\b|\\bsubcutaneous\\b", n)) return("SC")
      return("Oral")
    }

    # ── Prescription export helpers ──────────────────────────────

    # Map RenalIQ freq codes → human-readable labels.
    # Checks the CSV-derived lookup first so any new entry in freq_list.csv is
    # automatically resolved.  Pattern-based fallback handles formula-generated
    # OD*/BD* codes that are not enumerated in the CSV.
    map_freq_display <- function(freq) {
      fu <- toupper(trimws(freq))
      # 1. CSV lookup (covers all named codes)
      lbl <- freq_label_map[fu]
      if (!is.null(lbl) && !is.na(lbl) && nchar(lbl) > 0) return(lbl)
      # 2. Pattern fallback for formula-generated codes (OD8, BD10, etc.)
      if (startsWith(fu, "OD")) return("Once Daily")
      if (startsWith(fu, "BD")) return("Twice Daily")
      return(freq)
    }

    # Map route abbreviations → full labels
    map_route_display <- function(route) {
      r <- toupper(trimws(route))
      if (r == "ORAL")  return("Oral")
      if (r == "IV")    return("Intravenous")
      if (r == "SC")    return("Subcutaneous")
      if (r == "IM")    return("Intramuscular")
      return(route)
    }

    # Split "1 TAB" → list(qty="1", unit="TAB");  "500mg" → list(qty="500", unit="mg")
    parse_dose_parts <- function(dose) {
      parts <- strsplit(trimws(dose), "\\s+")[[1]]
      if (length(parts) >= 2) return(list(qty = parts[1], unit = parts[2]))
      m <- regmatches(dose, regexec("^([0-9.]+)(.*)", trimws(dose)))[[1]]
      if (length(m) >= 3 && nchar(trimws(m[3])) > 0)
        return(list(qty = m[2], unit = trimws(m[3])))
      list(qty = dose, unit = "TAB")
    }

    # Split "30 days" → list(qty="30", type="Day(s)"); "Until next visit" → list(qty="30", type="Day(s)")
    parse_duration_parts <- function(dur) {
      d <- trimws(dur)
      if (is.na(d) || nchar(d) == 0 || tolower(d) == "until next visit")
        return(list(qty = "30", type = "Day(s)"))
      parts <- strsplit(d, "\\s+")[[1]]
      qty   <- parts[1]
      type  <- if (length(parts) >= 2) {
        u <- toupper(parts[2])
        if (grepl("WEEK",  u)) "Week(s)" else if (grepl("MONTH", u)) "Month(s)" else "Day(s)"
      } else "Day(s)"
      list(qty = qty, type = type)
    }

    # Build the prescription Excel workbook and export it.
    # Tries local folder first (RX_OUTPUT_DIR, dev), then Google Cloud Storage (GCS, production).
    # Returns a human-readable description of where it was saved, or NULL if both disabled.
    export_rx <- function(rx_df, pt) {
      if (!requireNamespace("writexl", quietly = TRUE)) return(invisible(NULL))
      if (nrow(rx_df) == 0) return(invisible(NULL))

      local_enabled <- nchar(rx_output_dir) > 0 && dir.exists(rx_output_dir)
      cloud_enabled <- isTRUE(gcs_enabled) && nchar(gcs_bucket) > 0 &&
                       requireNamespace("googleCloudStorageR", quietly = TRUE)
      if (!local_enabled && !cloud_enabled) return(invisible(NULL))

      # ── Sheet 1: Prescription ──────────────────────────────────
      export_rows <- lapply(seq_len(nrow(rx_df)), function(i) {
        r    <- rx_df[i, ]
        dp   <- parse_dose_parts(r$dose)
        durp <- parse_duration_parts(r$duration)
        data.frame(
          Brand_Name    = r$brand_name,
          Generic       = r$generic,
          Dose          = r$dose,
          Dose_Qty      = dp$qty,
          Dose_Unit     = dp$unit,
          Freq_Code     = r$freq,
          Freq_Display  = map_freq_display(r$freq),
          Route         = r$route,
          Route_Display = map_route_display(r$route),
          Duration      = r$duration,
          Duration_Qty  = durp$qty,
          Duration_Type = durp$type,
          stringsAsFactors = FALSE
        )
      })
      export_df <- do.call(rbind, export_rows)

      # ── Sheet 2: Patient_Info ─────────────────────────────────
      pt_name <- trimws(paste(pt$first_name %||% "", pt$last_name %||% ""))
      info_df <- data.frame(
        Field = c("Patient Name", "Patient ID", "Prescription Date", "Exported At"),
        Value = c(pt_name, as.character(pt$id),
                  as.character(Sys.Date()), format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        stringsAsFactors = FALSE
      )

      fname <- paste0("Rx_", gsub("[^A-Za-z0-9]", "_", pt_name),
                      "_", as.character(Sys.Date()), ".xlsx")

      # Write to temp file — used by both local copy and GCS upload
      tmp <- tempfile(fileext = ".xlsx")
      on.exit(unlink(tmp), add = TRUE)
      writexl::write_xlsx(list(Prescription = export_df, Patient_Info = info_df), tmp)

      # ── Option A: copy to local folder (dev — PAD watches this directly) ──
      if (local_enabled) {
        dest <- file.path(rx_output_dir, fname)
        file.copy(tmp, dest, overwrite = TRUE)
        return(paste0("Saved locally: ", fname))
      }

      # ── Option B: upload to Google Cloud Storage (production on DO) ────────
      # Power Automate Cloud triggers on new file in bucket → saves to local
      # watched folder → Power Automate Desktop opens Apollo EARS.
      googleCloudStorageR::gcs_upload(
        file        = tmp,
        bucket      = gcs_bucket,
        name        = fname,
        type        = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
      paste0("Uploaded to GCS: ", fname)
    }

    # Parse freq code into UI state for the frequency picker.
    # Category detection is driven by freq_cats (loaded from CSV) so any new
    # code added to freq_list.csv is automatically routed to the right tab.
    parse_freq_to_state <- function(freq) {
      if (is.null(freq) || is.na(freq) || nchar(trimws(freq)) == 0)
        return(list(cat = "OD", num = "8", ampm = "AM"))
      fu <- toupper(trimws(freq))

      # OD with numeric hour — formula-generated, handled before CSV lookup
      if (grepl("^OD(\\d+)$", fu)) {
        hr <- as.integer(sub("^OD(\\d+)$", "\\1", fu))
        if (hr == 24) return(list(cat = "OD", num = "12", ampm = "AM"))  # midnight
        if (hr == 12) return(list(cat = "OD", num = "12", ampm = "PM"))  # noon
        if (hr > 12)  return(list(cat = "OD", num = as.character(hr - 12), ampm = "PM"))
        return(list(cat = "OD", num = as.character(hr), ampm = "AM"))
      }

      # BD named presets (check before BD numeric regex — BD1422 would misparse as hr=1422)
      if (fu %in% (freq_cats[["BD"]] %||% c("BD8", "BD10", "BD1422", "DIURETIC")))
        return(list(cat = "BD", num = "8", ampm = "AM"))

      # BD with numeric hour: BD8, BD10
      if (grepl("^BD(\\d+)$", fu)) {
        hr <- as.integer(sub("^BD(\\d+)$", "\\1", fu))
        return(list(cat = "BD", num = as.character(hr), ampm = "AM"))
      }

      # All other categories: look up in freq_cats (built from CSV)
      for (cat_name in c("TDS/QID", "Days", "Misc")) {
        codes <- freq_cats[[cat_name]]
        if (!is.null(codes) && fu %in% codes)
          return(list(cat = cat_name, num = "8", ampm = "AM"))
      }

      # Hardcoded fallbacks — cover the app when freq_list.csv is missing/empty
      if (fu %in% c("TDS", "TID", "QID", "PRN", "STAT", "5XD", "6XD"))
        return(list(cat = "TDS/QID", num = "8", ampm = "AM"))
      if (fu %in% c("MWF", "TTS", "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"))
        return(list(cat = "Days", num = "8", ampm = "AM"))
      if (fu %in% c("ALT", "WEEKLY", "MONTHLY", "FLU", "FRIW", "RTX", "HEPB") ||
          grepl("^1IN\\d+$", fu))
        return(list(cat = "Misc", num = "8", ampm = "AM"))

      return(list(cat = "OD", num = "8", ampm = "AM"))
    }

    # --- Database Operations ---
    load_rx_master <- function() {
      req(pool)
      # Fix 6: Fill empty durations in DB with default
      dbExecute(pool, "UPDATE drug_master SET duration = 'Until next visit' WHERE duration IS NULL OR TRIM(duration) = ''")
      data <- dbGetQuery(pool, "SELECT * FROM drug_master ORDER BY brand_name ASC")
      if ("id" %in% names(data)) data$id <- as.character(data$id)
      # Fix 7: Infer route for any rows that have empty route
      if (nrow(data) > 0 && "route" %in% names(data)) {
        empty_rt <- is.na(data$route) | trimws(data$route) == ""
        if (any(empty_rt)) {
          data$route[empty_rt] <- sapply(data$brand_name[empty_rt], infer_route)
        }
      }
      rx_master(clean_df(data))
    }
    observe({ load_rx_master() })

    observeEvent(input$add_custom_drug, {
      showModal(modalDialog(
        title = tags$h4(style="color:#e67e22;", icon("database"), " Add Drug to Master List"),
        div(class="row g-2",
            div(class="col-12", textInput(ns("new_brand"),   "Brand Name",  value = input$rx_q)),
            div(class="col-12", textInput(ns("new_generic"), "Generic Name")),
            div(class="col-6",  textInput(ns("new_dose"),    "Dose",        value = "1 TAB")),
            div(class="col-6",  textInput(ns("new_freq"),    "Freq",        value = "OD8")),
            div(class="col-12", textInput(ns("new_dur"),     "Duration",    value = "Until next visit"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_new_to_db"), "Save to Master", class="btn-warning fw-bold")
        ), size = "m", easyClose = TRUE
      ))
    })

    observeEvent(input$save_new_to_db, {
      req(input$new_brand)
      route_val <- infer_route(input$new_brand)
      dbExecute(pool,
        "INSERT INTO drug_master (brand_name, generic, dose, freq, route, duration) VALUES ($1, $2, $3, $4, $5, $6)",
        list(input$new_brand, input$new_generic, input$new_dose, input$new_freq, route_val, input$new_dur))
      load_rx_master()
      new_row <- data.frame(brand_name = input$new_brand, generic = input$new_generic,
                            dose = input$new_dose, freq = input$new_freq,
                            route = route_val, duration = input$new_dur, stringsAsFactors = FALSE)
      rx_meds$df <- clean_df(rbind(rx_meds$df, new_row))
      rx_meds$selected_idx <- nrow(rx_meds$df)
      removeModal()
      updateTextInput(session, "rx_q", value = "")
    })

    # --- Live Input Sync ---
    # One plain observeEvent per field — fires only when THAT input changes on the
    # client, NOT when selected_idx changes server-side.  Avoids the race condition
    # where switching cards would fire with stale ov_* values from the previous card.

    # Guard: only write to df when value ACTUALLY differs from current.
    # Without this, every renderUI re-render (triggered by s_cat/s_num/s_ampm
    # changes) recreates the ov_* textInputs → Shiny sends their values back →
    # observers fire → df "changes" (same value) → re-render → infinite loop.
    observeEvent(input$ov_brand, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      new_val <- trimws(input$ov_brand)
      if (nchar(new_val) > 0 &&
          !identical(new_val, trimws(as.character(rx_meds$df$brand_name[idx]))))
        rx_meds$df$brand_name[idx] <- new_val
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$ov_generic, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      new_val <- trimws(input$ov_generic)
      if (!identical(new_val, trimws(as.character(rx_meds$df$generic[idx]))))
        rx_meds$df$generic[idx] <- new_val
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$ov_dose, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      new_val <- trimws(input$ov_dose)
      if (!identical(new_val, trimws(as.character(rx_meds$df$dose[idx]))))
        rx_meds$df$dose[idx] <- new_val
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$ov_route, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      new_val <- trimws(input$ov_route)
      if (!identical(new_val, trimws(as.character(rx_meds$df$route[idx]))))
        rx_meds$df$route[idx] <- new_val
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$ov_dur, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      new_val <- trimws(input$ov_dur)
      if (!identical(new_val, trimws(as.character(rx_meds$df$duration[idx]))))
        rx_meds$df$duration[idx] <- new_val
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ov_freq text override: takes priority over the picker buttons.
    # Same re-render guard applied — only write when value actually changed.
    observeEvent(input$ov_freq, {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      new_val <- trimws(input$ov_freq)
      if (nchar(new_val) > 0 &&
          !identical(new_val, trimws(as.character(rx_meds$df$freq[idx]))))
        rx_meds$df$freq[idx] <- new_val
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Freq picker (category/number/ampm buttons) → only fires when user clicks a picker button
    observeEvent(list(rx_meds$s_cat, rx_meds$s_num, rx_meds$s_ampm), {
      req(rx_meds$selected_idx)
      idx <- as.numeric(rx_meds$selected_idx)
      # Picker only drives freq when the manual override field is empty
      if (!is.null(input$ov_freq) && input$ov_freq != "") return()
      new_freq <- NULL
      if (rx_meds$s_cat == "OD") {
        num <- as.numeric(rx_meds$s_num)
        hr  <- if (rx_meds$s_ampm == "PM" && num < 12) num + 12 else if (rx_meds$s_ampm == "AM" && num == 12) 24 else num
        new_freq <- paste0("OD", hr)
      } else if (rx_meds$s_cat == "BD") {
        new_freq <- paste0("BD", rx_meds$s_num)
      }
      if (!is.null(new_freq)) {
        rx_meds$df$freq[idx] <- new_freq
        # Keep the placeholder current without re-rendering the whole panel
        updateTextInput(session, "ov_freq", placeholder = new_freq)
      }
    }, ignoreInit = TRUE)

    # --- UI Logic Builders ---
    # Replaced render_quick_edit() with two separate renderUI outputs to eliminate
    # full card-list re-renders whenever a freq button is clicked.
    #
    #  output$freq_picker      — only the category tabs + buttons
    #                            re-renders when s_cat / s_num / s_ampm change (fast)
    #  output$quick_edit_panel — the card shell + override row
    #                            only re-renders when selected_idx changes (new card)
    #  output$rx_list          — no longer reads freq state, so stays quiet while
    #                            the user taps through the frequency picker

    # Fast output: only the interactive picker portion
    output$freq_picker <- renderUI({
      req(rx_meds$selected_idx)
      s_cat  <- rx_meds$s_cat
      s_num  <- rx_meds$s_num
      s_ampm <- rx_meds$s_ampm

      freq_panel <- if (s_cat == "OD") {
        div(class = "d-flex gap-2",
            div(class = "stitch-grid", lapply(1:12, function(n) {
              actionButton(ns(paste0("sn_", n)), as.character(n),
                           class   = paste("btn btn-stitch", if (s_num == as.character(n)) "btn-warning" else "btn-outline-warning"),
                           onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_num"), n))
            })),
            div(class = "ampm-col",
                actionButton(ns("s_am"), "AM",
                             class   = if (s_ampm == "AM") "btn btn-xs btn-dark" else "btn btn-xs btn-outline-dark",
                             onclick = sprintf("Shiny.setInputValue('%s','AM')", ns("change_ampm"))),
                actionButton(ns("s_pm"), "PM",
                             class   = if (s_ampm == "PM") "btn btn-xs btn-dark" else "btn btn-xs btn-outline-dark",
                             onclick = sprintf("Shiny.setInputValue('%s','PM')", ns("change_ampm"))))
        )
      } else if (s_cat == "BD") {
        # BD presets come from freq_cats[["BD"]]; labels from freq_label_map.
        # Hardcoded fallback when CSV is missing.
        bd_codes <- freq_cats[["BD"]] %||% c("BD1422", "DIURETIC", "BD8", "BD10")
        div(
          div(class = "stitch-grid", lapply(1:12, function(n) {
            actionButton(ns(paste0("sn_", n)), as.character(n),
                         class   = paste("btn btn-stitch", if (s_num == as.character(n)) "btn-warning" else "btn-outline-warning"),
                         onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_num"), n))
          })),
          div(class = "d-flex flex-wrap gap-1 mt-1 align-items-center",
              tags$small(class = "text-muted me-1", "Presets:"),
              lapply(bd_codes, function(code) {
                lbl <- freq_label_map[code]
                if (is.null(lbl) || is.na(lbl) || nchar(lbl) == 0) lbl <- code
                actionButton(ns(paste0("fc_", tolower(code))), lbl,
                             class   = "btn btn-sm btn-outline-secondary",
                             onclick = sprintf("Shiny.setInputValue('%s','%s')",
                                               ns("direct_freq"), tolower(code)))
              })
          )
        )
      } else {
        # TDS/QID, Days, Misc: codes from CSV; hardcoded fallback if CSV absent.
        codes <- freq_cats[[s_cat]]
        if (is.null(codes) || length(codes) == 0) {
          codes <- switch(s_cat,
            "TDS/QID" = c("TDS", "TID", "QID", "PRN", "STAT", "5XD", "6XD"),
            "Days"    = c("MWF", "TTS", "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"),
            "Misc"    = c("ALT", "WEEKLY", "MONTHLY", "FLU", "FRIW", "RTX", "HEPB",
                          "1IN14", "1IN21", "1IN42", "1IN60", "1IN90", "1IN120", "1IN180"),
            character(0)
          )
        }
        div(class = "d-flex flex-wrap gap-1",
            lapply(codes, function(code) {
              actionButton(ns(paste0("fc_", tolower(code))), code,
                           class   = "btn btn-sm btn-outline-warning",
                           onclick = sprintf("Shiny.setInputValue('%s','%s')",
                                             ns("direct_freq"), tolower(code)))
            }))
      }

      tagList(
        # Category tabs
        div(class = "d-flex flex-wrap gap-1 mb-2",
            lapply(c("OD", "BD", "TDS/QID", "Days", "Misc"), function(cat) {
              actionButton(ns(paste0("scat_", cat)), cat,
                           class   = if (s_cat == cat) "btn btn-xs btn-dark" else "btn btn-xs btn-outline-warning",
                           onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("change_cat"), cat))
            })),
        # Frequency picker panel
        div(class = "p-2 border rounded bg-white", freq_panel)
      )
    })

    # Slow output: card shell + override row — only re-renders when card changes
    output$quick_edit_panel <- renderUI({
      req(rx_meds$selected_idx)
      idx  <- as.numeric(rx_meds$selected_idx)
      curr <- isolate(rx_meds$df[idx, ])   # isolate: don't re-render on every df update

      div(class = "card border-warning mb-2",
          div(class = "card-header bg-warning py-1 d-flex justify-content-between",
              tags$strong("Set Frequency"),
              actionButton(ns("close_edit"), "Done", class = "btn btn-xs btn-dark")),
          div(class = "card-body p-2",
              # Live-updating picker lives in its own fast output
              uiOutput(ns("freq_picker")),

              # Override row: static fields initialised once per card selection.
              # ov_freq placeholder is kept current via updateTextInput() in observers.
              div(class = "override-row",
                  div(class = "row g-1 mb-1",
                      div(class = "col-6", span(class = "label-mini", "Brand"),   textInput(ns("ov_brand"),   NULL, value = curr$brand_name)),
                      div(class = "col-6", span(class = "label-mini", "Generic"), textInput(ns("ov_generic"), NULL, value = curr$generic))
                  ),
                  div(class = "row g-1",
                      div(class = "col-3", span(class = "label-mini", "Dose"),  textInput(ns("ov_dose"),  NULL, value = curr$dose)),
                      div(class = "col-3", span(class = "label-mini", "Freq"),  textInput(ns("ov_freq"),  NULL, value = "", placeholder = curr$freq)),
                      div(class = "col-3", span(class = "label-mini", "Route"), textInput(ns("ov_route"), NULL, value = curr$route)),
                      div(class = "col-3", span(class = "label-mini", "Dur"),   textInput(ns("ov_dur"),   NULL, value = curr$duration))
                  )
              )
          )
      )
    })

    # --- Observers ---
    # Clicking a category tab = "use the picker" → clear any manual ov_freq override
    # so the s_cat/s_num/s_ampm observer is not blocked by a stale non-empty ov_freq.
    observeEvent(input$change_cat, {
      rx_meds$s_cat <- input$change_cat
      updateTextInput(session, "ov_freq", value = "")
    })
    observeEvent(input$change_num, {
      rx_meds$s_num <- input$change_num
      updateTextInput(session, "ov_freq", value = "")
    })
    observeEvent(input$change_ampm, {
      rx_meds$s_ampm <- input$change_ampm
      updateTextInput(session, "ov_freq", value = "")
    })
    observeEvent(input$direct_freq, {
      updateTextInput(session, "ov_freq", value = "")
      rx_meds$df$freq[rx_meds$selected_idx] <- input$direct_freq
      # Keep the placeholder showing the current freq code without re-rendering the panel
      updateTextInput(session, "ov_freq", placeholder = input$direct_freq)
    })
    observeEvent(input$close_edit, { rx_meds$selected_idx <- NULL })

    # Fix 1: Pre-populate freq state when selecting an existing card
    observeEvent(input$select_rx_card, {
      idx <- input$select_rx_card
      rx_meds$selected_idx <- idx
      if (!is.null(idx) && idx >= 1 && idx <= nrow(rx_meds$df)) {
        fs <- parse_freq_to_state(rx_meds$df$freq[idx])
        rx_meds$s_cat  <- fs$cat
        rx_meds$s_num  <- fs$num
        rx_meds$s_ampm <- fs$ampm
      }
    })

    observeEvent(input$del_rx_idx, {
      rx_meds$df <- rx_meds$df[-input$del_rx_idx, ]
      rx_meds$selected_idx <- NULL
    })

    # --- Rendering ---
    output$rx_search_results <- renderUI({
      req(input$rx_q)
      if (nchar(input$rx_q) < 2) return(NULL)
      matches <- rx_master() %>% filter(grepl(input$rx_q, brand_name, ignore.case = TRUE)) %>% head(5)
      if (nrow(matches) == 0)
        return(actionButton(ns("add_custom_drug"), paste("Add New:", input$rx_q), class = "btn btn-sm btn-warning w-100 mt-2"))
      lapply(1:nrow(matches), function(i) {
        d <- matches[i, ]
        div(class = "d-flex align-items-center mb-1",
            # Left: drug info — click to add to prescription
            tags$button(
              class   = "btn btn-sm btn-outline-secondary flex-grow-1 text-start",
              onclick = sprintf("Shiny.setInputValue('%s', '%s')", ns("add_rx_id"), d$id),
              HTML(paste0(
                "<b>", d$brand_name, "</b>",
                " <span style='color:#e67e22;font-style:italic;font-size:0.8rem;'>", d$generic, "</span>",
                "<br><small class='text-muted'>",
                d$dose, " &nbsp;·&nbsp; ", d$freq,
                " &nbsp;·&nbsp; ", d$route,
                " &nbsp;·&nbsp; ", d$duration,
                "</small>"
              ))
            ),
            # Right: edit button — opens edit modal for this master drug
            tags$button(
              class   = "btn btn-sm btn-outline-warning ms-1",
              title   = "Edit this drug in master list",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                ns("edit_drug_id"), d$id),
              icon("pencil")
            ),
            # Far right: delete button — removes drug from master list
            tags$button(
              class   = "btn btn-sm btn-outline-danger ms-1",
              title   = "Delete this drug from master list",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                ns("del_master_id"), d$id),
              icon("trash")
            )
        )
      })
    })

    # Fix 1 + 7: Pre-populate freq state and infer route when adding from master
    observeEvent(input$add_rx_id, {
      lib       <- rx_master() %>% filter(id == input$add_rx_id)
      route_val <- if (!is.na(lib$route[1]) && trimws(lib$route[1]) != "") lib$route[1] else infer_route(lib$brand_name[1])
      dur_val   <- if (!is.na(lib$duration[1]) && trimws(lib$duration[1]) != "") lib$duration[1] else "Until next visit"
      row <- data.frame(brand_name = lib$brand_name[1], generic = lib$generic[1],
                        dose = lib$dose[1], freq = lib$freq[1],
                        route = route_val, duration = dur_val, stringsAsFactors = FALSE)
      rx_meds$df <- clean_df(rbind(rx_meds$df, row))
      new_idx    <- nrow(rx_meds$df)
      rx_meds$selected_idx <- new_idx
      # Pre-populate frequency picker state from this drug's saved freq
      fs <- parse_freq_to_state(lib$freq[1])
      rx_meds$s_cat  <- fs$cat
      rx_meds$s_num  <- fs$num
      rx_meds$s_ampm <- fs$ampm
      updateTextInput(session, "rx_q", value = "")
    })

    output$rx_list <- renderUI({
      df <- rx_meds$df
      if (nrow(df) == 0) return(NULL)
      lapply(1:nrow(df), function(i) {
        sel <- !is.null(rx_meds$selected_idx) && rx_meds$selected_idx == i
        tagList(
          div(class   = paste("rx-card p-3", if (sel) "selected"),
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("select_rx_card"), i),
              div(class = "d-flex justify-content-between align-items-start",
                  div(
                    span(class = "rx-brand-name", df$brand_name[i]),
                    div(class  = "rx-generic-name", df$generic[i]),
                    div(class  = "mt-1",
                        span(class = "rx-badge", icon("pills"),          df$dose[i]),
                        span(class = "rx-badge", icon("clock"),          df$freq[i]),
                        span(class = "rx-badge", icon("map-marker-alt"), df$route[i]))
                  ),
                  actionButton(ns(paste0("del_", i)), NULL, icon("trash"),
                               class   = "btn btn-sm btn-outline-danger border-0",
                               onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', %d)", ns("del_rx_idx"), i))
              )
          ),
          if (sel) uiOutput(ns("quick_edit_panel"))
        )
      })
    })

    output$rx_main_container <- renderUI({
      req(current_pt())
      div(
        div(class = "bg-dark text-white p-2 d-flex justify-content-between",
            span(icon("file-medical"), " PRESCRIPTION"),
            if (rx_meta$is_history) span(class = "badge bg-warning text-dark", rx_meta$history_date)),
        div(class = "p-2",
            textInput(ns("rx_q"), NULL, placeholder = "Search..."),
            uiOutput(ns("rx_search_results")),
            hr(class = "my-2"),
            uiOutput(ns("rx_list")),
            actionButton(ns("save_rx"), "SAVE & CLOSE", class = "btn-warning w-100 fw-bold mt-2"))
      )
    })

    # --- Edit Master Drug Modal ---
    observeEvent(input$edit_drug_id, {
      drug_id <- trimws(as.character(input$edit_drug_id))
      d <- rx_master() %>% filter(id == drug_id)
      req(nrow(d) > 0)
      edit_drug_rv(d[1, ])

      showModal(modalDialog(
        title = tags$h4(style = "color:#e67e22;", icon("pencil"), " Edit Drug in Master List"),
        div(class = "row g-2",
            div(class = "col-12", textInput(ns("edit_brand"),   "Brand Name",  value = d$brand_name[1])),
            div(class = "col-12", textInput(ns("edit_generic"), "Generic Name", value = d$generic[1])),
            div(class = "col-6",  textInput(ns("edit_dose"),    "Dose",         value = d$dose[1])),
            div(class = "col-6",  textInput(ns("edit_freq"),    "Freq",         value = d$freq[1])),
            div(class = "col-6",  textInput(ns("edit_route"),   "Route",        value = d$route[1])),
            div(class = "col-6",  textInput(ns("edit_dur"),     "Duration",     value = d$duration[1]))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_edit_as_new"), "Add as New Drug",
                       class = "btn-warning fw-bold", icon = icon("plus")),
          actionButton(ns("save_edit_update"),  "Update Drug",
                       class = "btn-success fw-bold", icon = icon("check"))
        ),
        size = "m", easyClose = TRUE
      ))
    })

    # Update existing master drug record
    observeEvent(input$save_edit_update, {
      req(edit_drug_rv(), input$edit_brand)
      d         <- edit_drug_rv()
      route_val <- if (nchar(trimws(input$edit_route)) > 0)
                     trimws(input$edit_route)
                   else
                     infer_route(input$edit_brand)
      tryCatch({
        dbExecute(pool,
          "UPDATE drug_master
              SET brand_name = $1,
                  generic    = $2,
                  dose       = $3,
                  freq       = $4,
                  route      = $5,
                  duration   = $6
            WHERE id::text   = $7",
          list(trimws(input$edit_brand), trimws(input$edit_generic),
               trimws(input$edit_dose),  trimws(input$edit_freq),
               route_val,                trimws(input$edit_dur),
               as.character(d$id)))
        load_rx_master()
        edit_drug_rv(NULL)
        removeModal()
        showNotification("Drug updated in master list.", type = "message")
      }, error = function(e) {
        showNotification(paste("Update error:", e$message), type = "error")
      })
    })

    # Insert the edited values as a brand-new master drug record
    observeEvent(input$save_edit_as_new, {
      req(input$edit_brand)
      route_val <- if (nchar(trimws(input$edit_route)) > 0)
                     trimws(input$edit_route)
                   else
                     infer_route(input$edit_brand)
      tryCatch({
        dbExecute(pool,
          "INSERT INTO drug_master (brand_name, generic, dose, freq, route, duration)
           VALUES ($1, $2, $3, $4, $5, $6)",
          list(trimws(input$edit_brand), trimws(input$edit_generic),
               trimws(input$edit_dose),  trimws(input$edit_freq),
               route_val,                trimws(input$edit_dur)))
        load_rx_master()
        edit_drug_rv(NULL)
        removeModal()
        showNotification("New drug variant added to master list.", type = "message")
      }, error = function(e) {
        showNotification(paste("Insert error:", e$message), type = "error")
      })
    })

    # Delete master drug — step 1: confirmation modal
    observeEvent(input$del_master_id, {
      drug_id <- trimws(as.character(input$del_master_id))
      req(nchar(drug_id) > 0)
      row <- rx_master() %>% filter(id == drug_id)
      if (nrow(row) == 0) return()

      showModal(modalDialog(
        title = tags$h5(icon("trash"), " Delete Drug from Master List"),
        p("Permanently remove ", tags$strong(row$brand_name[1]),
          " (", row$generic[1], ") from the master drug list?"),
        p(class = "text-muted small", "This does not affect prescriptions already saved."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_del_master"), "Delete",
                       class = "btn-danger fw-bold", icon = icon("trash"))
        ),
        easyClose = TRUE
      ))
      # Stash id so the confirm observer knows which row to remove.
      # Use a rx-module-specific key to avoid collision with mod_user_mgmt.
      session$userData$pending_master_del_rx <- drug_id
    })

    # Delete master drug — step 2: execute after confirmation
    observeEvent(input$confirm_del_master, {
      drug_id <- session$userData$pending_master_del_rx
      req(!is.null(drug_id))
      tryCatch({
        dbExecute(pool, "DELETE FROM drug_master WHERE id::text = $1", list(drug_id))
        removeModal()
        load_rx_master()
        showNotification("Drug removed from master list.", type = "warning")
      }, error = function(e) {
        showNotification(paste("Delete error:", e$message), type = "error")
      })
      session$userData$pending_master_del_rx <- NULL
    })

    # --- Persistence ---
    observeEvent(current_pt(), {
      req(current_pt()$id, pool)
      res <- dbGetQuery(pool,
        "SELECT meds_json, visit_date FROM prescriptions WHERE patient_id::text = $1::text ORDER BY visit_date DESC LIMIT 1",
        list(as.character(current_pt()$id)))
      if (nrow(res) > 0) {
        rx_meta$history_date <- format(as.Date(res$visit_date[1]), "%Y-%m-%d")
        rx_meta$is_history   <- TRUE
        loaded <- tryCatch(
          as.data.frame(jsonlite::fromJSON(res$meds_json[1])),
          error = function(e) data.frame()
        )
        # normalize_rx_cols: maps legacy column names, adds missing cols, drops extras
        rx_meds$df <- clean_df(normalize_rx_cols(loaded))
      } else {
        rx_meta$is_history <- FALSE
        rx_meds$df <- rx_meds$df[0, ]
      }
    })

    observeEvent(input$save_rx, {
      req(current_pt())
      final_df <- rx_meds$df

      # Fix 4: Auto-add new combos to master — check against in-memory rx_master()
      # to avoid N SELECT round-trips to the remote DB.
      if (nrow(final_df) > 0) {
        master     <- rx_master()
        added_any  <- FALSE
        for (i in seq_len(nrow(final_df))) {
          r <- final_df[i, ]
          # In-memory existence check: no DB query needed
          already_exists <- nrow(master[
            tolower(master$brand_name) == tolower(r$brand_name) &
            tolower(master$dose)       == tolower(r$dose)       &
            tolower(master$freq)       == tolower(r$freq)       &
            tolower(master$route)      == tolower(r$route)      &
            tolower(master$duration)   == tolower(r$duration), ]) > 0
          if (!already_exists) {
            dbExecute(pool,
              "INSERT INTO drug_master (brand_name, generic, dose, freq, route, duration)
               VALUES ($1, $2, $3, $4, $5, $6)",
              list(r$brand_name, r$generic, r$dose, r$freq, r$route, r$duration))
            added_any <- TRUE
          }
        }
        # Reload master only if something was actually added
        if (added_any) load_rx_master()
      }

      json_data <- as.character(jsonlite::toJSON(final_df, auto_unbox = TRUE))
      dbExecute(pool,
        "INSERT INTO prescriptions (patient_id, meds_json, visit_date) VALUES ($1, $2, $3)
         ON CONFLICT (patient_id, visit_date) DO UPDATE SET meds_json = EXCLUDED.meds_json",
        list(as.character(current_pt()$id), json_data, Sys.Date()))
      showNotification("Saved")
      save_trigger(save_trigger() + 1)

      # ── Export (local folder or Google Drive, non-blocking) ──────────────
      tryCatch({
        result <- export_rx(final_df, current_pt())
        if (!is.null(result))
          showNotification(paste0("📤 ", result), type = "message", duration = 6)
      }, error = function(e) {
        message("Rx export error: ", e$message)
        showNotification("Prescription saved. Export failed — see server log.",
                         type = "warning", duration = 6)
      })
    })

    return(list(saved = reactive(save_trigger())))
  })
}
