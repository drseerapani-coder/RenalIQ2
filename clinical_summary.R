library(shiny)
library(bslib)
library(DBI)
library(jsonlite)

# --- UI FUNCTION ---
timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .visit-card { border-left: 6px solid #0d6efd; margin-bottom: 20px; border-radius: 8px; }
      .vitals-strip { background: #f8f9fa; border-radius: 6px; padding: 10px; font-size: 0.9rem; border: 1px solid #e9ecef; margin-bottom: 10px; }
      .pmhx-strip { background: #fff8f0; border-radius: 6px; padding: 8px 12px; font-size: 0.9rem; border: 1px solid #f39c12; margin-bottom: 10px; }
      .clinical-text { white-space: pre-wrap; font-size: 1rem; line-height: 1.5; color: #2c3e50; }
      .rx-table { width: 100%; font-size: 0.85rem; border-collapse: collapse; background: white; margin-top: 5px; }
      .rx-table th { background: #f1f3f5; color: #198754; font-weight: 600; padding: 6px; border-bottom: 2px solid #dee2e6; text-align: left; }
      .rx-table td { padding: 6px; border-bottom: 1px solid #eee; vertical-align: top; }
      .lab-text { font-size: 0.95rem; color: #495057; background: #f0f9ff; padding: 8px; border-radius: 4px; border-left: 4px solid #0dcaf0; }
      .lab-results-text { margin-bottom: 0px !important; }
      .rx-table-container { margin-top: 5px !important; }
    ")),

    tags$script(HTML("
      window.copyClinicalVisit = function(btn) {
        try {
            var card = btn.closest('.card');
            if (!card) return;

            var getT = function(selector) {
                var el = card.querySelector(selector);
                if (!el) return 'N/A';
                var txt = el.innerText.trim();
                var placeholders = ['No lab records found.', 'None recorded.', 'No notes.',
                                    'No valid lab records found.', 'No vitals recorded.', 'N/A'];
                return placeholders.includes(txt) ? 'N/A' : txt;
            };

            var date    = getT('.visit-date');
            var pmhx    = getT('.pmhx-text');
            var notes   = getT('.exam-notes');
            var labs    = getT('.lab-results-text');
            var followup = getT('.follow-up-date');

            // Vitals: read individual spans
            var bp = card.querySelector('.vitals-text span:nth-child(1)');
            var hr = card.querySelector('.vitals-text span:nth-child(2)');
            var wt = card.querySelector('.vitals-text span:nth-child(3)');
            var vitalsText = [bp, hr, wt].map(function(el) {
                return el ? el.innerText.trim() : '';
            }).filter(Boolean).join('  |  ');

            // Multi-line prescription layout
            var rows = card.querySelectorAll('.rx-table tbody tr');
            var medsText = '';
            if (rows.length > 0) {
                medsText = Array.from(rows).map(function(tr) {
                    var c = Array.from(tr.querySelectorAll('td')).map(function(td) {
                        return td.innerText.trim().replace(/\\n/g, ' ');
                    });
                    return c[0] + ' (' + c[1] + ')\\n   Dose: ' + c[2] + ' | Freq: ' + c[3] + ' | Route: ' + c[4] + ' | Dur: ' + c[5];
                }).join('\\n\\n');
            } else {
                medsText = 'No prescriptions recorded.';
            }

            var fullText =
                '--- CLINICAL ENCOUNTER: ' + date + ' ---\\n' +
                'PMHx: ' + pmhx + '\\n\\n' +
                'VITALS: ' + vitalsText + '\\n' +
                'FOLLOW-UP: ' + followup + '\\n\\n' +
                'EXAM & PLAN:\\n' + notes + '\\n\\n' +
                'INVESTIGATIONS:\\n' + labs + '\\n\\n' +
                'PRESCRIPTIONS:\\n' + medsText;

            var textArea = document.createElement('textarea');
            textArea.value = fullText;
            document.body.appendChild(textArea);
            textArea.select();
            document.execCommand('copy');
            document.body.removeChild(textArea);

            var oldHtml = btn.innerHTML;
            btn.innerHTML = '✓ Copied';
            btn.classList.replace('btn-outline-secondary', 'btn-success');
            setTimeout(function() {
                btn.innerHTML = oldHtml;
                btn.classList.replace('btn-success', 'btn-outline-secondary');
            }, 2000);
        } catch (err) { console.error('Copy failed:', err); }
      };
    ")),
    uiOutput(ns("timeline_render"))
  )
}

# --- SERVER FUNCTION ---
# freq_label_map — named vector UPPERCASE_CODE → label, built once in app.R.
# Falls back to reading the CSV locally when called outside the main app
# (e.g., unit tests or standalone sourcing).
timeline_server <- function(id, pool, current_pt, refresh_trigger = reactive(0),
                            freq_label_map = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Frequency Lookup (Static) — UPPERCASE_CODE → label
    #    Use the pre-loaded map from app.R; fall back to local CSV read if absent.
    freq_lookup <- if (!is.null(freq_label_map) && length(freq_label_map) > 0) {
      freq_label_map   # already keyed by UPPERCASE code
    } else {
      tryCatch({
        df <- read.csv("freq_list.csv", header = FALSE, stringsAsFactors = FALSE)
        # names = UPPERCASE codes (col1), values = labels (col2)
        setNames(trimws(as.character(df[[2]])),
                 toupper(trimws(as.character(df[[1]]))))
      }, error = function(e) { NULL })
    }

    # 2. Reactive Data — visits, labs, rx, AND pmhx
    clinical_data <- reactive({
      req(current_pt(), pool)
      refresh_trigger()
      pid <- as.character(current_pt()$id)

      tryCatch({
        list(
          visits = dbGetQuery(pool,
            "SELECT visit_date::date AS v_date, visit_json
             FROM visitsmodule WHERE patient_id::text = $1
             ORDER BY visit_date DESC",
            list(pid)
          ),
          labs = dbGetQuery(pool,
            "SELECT test_date::date AS l_date, test_name, num_val
             FROM labs WHERE patient_id::text = $1
             ORDER BY test_date DESC",
            list(pid)
          ),
          rx = dbGetQuery(pool,
            "SELECT visit_date::date AS v_date, meds_json
             FROM prescriptions WHERE patient_id::text = $1",
            list(pid)
          ),
          pmhx = dbGetQuery(pool,
            "SELECT condition_text, onset_date
             FROM past_medical_history WHERE registration_id::text = $1
             ORDER BY onset_date ASC NULLS LAST",
            list(pid)
          )
        )
      }, error = function(e) {
        message("Data fetch error: ", e$message)
        NULL
      })
    })

    # 3. UI Rendering
    output$timeline_render <- renderUI({
      data   <- clinical_data()
      f_map  <- freq_lookup

      if (is.null(data$visits) || nrow(data$visits) == 0) {
        return(div(class = "p-4 text-center text-muted", "No encounters found."))
      }

      # --- PMHx: patient-level, same across all visit cards ---
      pmhx_text <- if (!is.null(data$pmhx) && nrow(data$pmhx) > 0) {
        items <- apply(data$pmhx, 1, function(r) {
          cond <- trimws(as.character(r["condition_text"]))
          yr   <- trimws(as.character(r["onset_date"]))
          if (!is.na(yr) && nchar(yr) > 0 && yr != "NA") paste0(cond, " (", yr, ")") else cond
        })
        paste(items, collapse = "\n")
      } else {
        "None recorded."
      }

      # --- Latest lab display (computed once for all cards) ---
      latest_lab_display <- "No valid lab records found."
      if (!is.null(data$labs) && nrow(data$labs) > 0) {
        l_date_raw <- data$labs$l_date[1]
        l_date_fmt <- tryCatch(format(as.Date(l_date_raw), "%d %b %Y"), error = function(e) as.character(l_date_raw))
        valid_records <- data$labs[
          data$labs$l_date == l_date_raw &
          !is.na(data$labs$num_val) &
          data$labs$num_val != "" &
          data$labs$num_val != "NULL", ]
        if (nrow(valid_records) > 0) {
          lab_items <- paste0(valid_records$test_name, ": ", valid_records$num_val)
          latest_lab_display <- paste0(l_date_fmt, " — ", paste(lab_items, collapse = " | "))
        } else {
          latest_lab_display <- paste0(l_date_fmt, " — No parameters recorded for this date.")
        }
      }

      # --- Latest prescription (used for the most recent visit card) ---
      latest_rx_df <- NULL
      if (!is.null(data$rx) && nrow(data$rx) > 0) {
        rx_sorted <- data$rx[order(data$rx$v_date, decreasing = TRUE), ]
        latest_rx_df <- tryCatch(jsonlite::fromJSON(rx_sorted$meds_json[1]), error = function(e) NULL)
        if (!is.data.frame(latest_rx_df) || nrow(latest_rx_df) == 0) latest_rx_df <- NULL
      }

      # --- Helper: build Rx table ---
      build_rx_table <- function(m_df) {
        tags$table(class = "rx-table border",
          tags$thead(tags$tr(
            tags$th("Brand"), tags$th("Generic"), tags$th("Dose"),
            tags$th("Freq"), tags$th("Route"), tags$th("Dur")
          )),
          tags$tbody(lapply(1:nrow(m_df), function(j) {
            row_vals   <- as.character(m_df[j, ])
            translated <- sapply(row_vals, function(cell) {
              clean      <- trimws(cell)
              lookup_key <- toupper(clean)
              if (!is.null(f_map) && lookup_key %in% names(f_map)) return(unname(f_map[lookup_key]))
              return(cell)
            })
            tags$tr(
              tags$td(strong(translated[1])),
              tags$td(span(translated[2], style = "font-size:0.75rem; color:#666")),
              tags$td(translated[3]),
              tags$td(span(class = "freq-badge", translated[4])),
              tags$td(translated[5]),
              tags$td(if (length(translated) >= 6) translated[6] else "-")
            )
          }))
        )
      }

      # --- Generate cards ---
      tagList(lapply(1:nrow(data$visits), function(i) {

        this_visit  <- data$visits[i, ]
        raw_v_date  <- this_visit$v_date
        clean_v_date <- tryCatch(format(as.Date(raw_v_date), "%d %b %Y"), error = function(e) as.character(raw_v_date))

        # Parse visit JSON
        v_json <- list(vitals = list(bp = "-", hr = "-", weight = "-"), clinic_notes = "No notes.")
        if (!is.na(this_visit$visit_json) && this_visit$visit_json != "") {
          parsed <- tryCatch(jsonlite::fromJSON(this_visit$visit_json, simplifyVector = FALSE), error = function(e) NULL)
          if (!is.null(parsed)) v_json <- parsed
        }

        # Prescription for this card
        rx_ui <- div(class = "p-2 text-muted", "None recorded.")
        if (!is.null(data$rx) && nrow(data$rx) > 0) {
          if (i == 1 && !is.null(latest_rx_df)) {
            rx_ui <- build_rx_table(latest_rx_df)
          } else {
            day_rx <- data$rx[which(data$rx$v_date == raw_v_date), ]
            if (nrow(day_rx) > 0) {
              m_df <- tryCatch(jsonlite::fromJSON(day_rx$meds_json[1]), error = function(e) NULL)
              if (is.data.frame(m_df) && nrow(m_df) > 0) rx_ui <- build_rx_table(m_df)
            }
          }
        }

        # Follow-up date
        followup_display <- "—"
        fd <- v_json$followup_date
        if (!is.null(fd) && nchar(trimws(as.character(fd[1]))) > 0)
          followup_display <- tryCatch(format(as.Date(as.character(fd[1])), "%d %b %Y"),
                                       error = function(e) as.character(fd[1]))

        card(class = "visit-card shadow-sm border-0",
          card_header(class = "bg-white d-flex justify-content-between align-items-center",
            span(strong(clean_v_date), class = "text-primary visit-date"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        onclick = "copyClinicalVisit(this)", "Copy Visit")
          ),
          card_body(

            # 1. PMHx
            div(class = "pmhx-strip",
                tags$span(strong("PMHx: "), class = "text-warning"),
                span(class = "pmhx-text", style = "white-space: pre-line; display: block;", pmhx_text)
            ),

            # 2. Vitals + Follow-up
            div(class = "vitals-strip d-flex flex-wrap gap-4 vitals-text",
                span(strong("BP: "),  v_json$vitals$bp     %||% "-"),
                span(strong("P: "),   v_json$vitals$hr     %||% "-"),
                span(strong("Wt: "),  v_json$vitals$weight %||% "-", " kg"),
                span(strong("Follow-up: "),
                     span(class = "follow-up-date", followup_display))
            ),

            # 3. Clinical Notes
            div(class = "mb-3",
                tags$h6("Examination & Plan", class = "border-bottom text-muted pb-1"),
                div(class = "clinical-text exam-notes", v_json$clinic_notes %||% "No notes.")
            ),

            # 4. Latest Investigations
            div(class = "mb-3",
                tags$h6("Latest Investigations", class = "text-info border-bottom pb-1"),
                div(class = "lab-text lab-results-text", latest_lab_display)
            ),

            # 5. Prescriptions
            div(
                tags$h6("Prescriptions", class = "text-success border-bottom pb-1"),
                rx_ui
            )
          )
        )
      }))
    })
  })
}
