library(shiny)
library(bslib)
library(DBI)
library(jsonlite)

# --- UI FUNCTION ---
timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* Remove bottom margins to close gaps */
      .lab-results-text { margin-bottom: 0px !important; }
      .rx-table-container { margin-top: 5px !important; }
      
      .visit-card { border-left: 6px solid #0d6efd; margin-bottom: 20px; border-radius: 8px; }
      .vitals-strip { background: #f8f9fa; border-radius: 6px; padding: 10px; font-size: 0.9rem; border: 1px solid #e9ecef; margin-bottom: 10px; }
      .clinical-text { white-space: pre-wrap; font-size: 1rem; line-height: 1.5; color: #2c3e50; }
      .rx-table { width: 100%; font-size: 0.85rem; border-collapse: collapse; background: white; margin-top: 5px; }
      .rx-table th { background: #f1f3f5; color: #198754; font-weight: 600; padding: 6px; border-bottom: 2px solid #dee2e6; text-align: left; }
      .rx-table td { padding: 6px; border-bottom: 1px solid #eee; vertical-align: top; }
      .lab-text { font-size: 0.95rem; color: #495057; background: #f0f9ff; padding: 8px; border-radius: 4px; border-left: 4px solid #0dcaf0; }
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
                var placeholders = ['No lab records found.', 'None recorded.', 'No notes.', 'No valid lab records found.', 'No vitals recorded.'];
                return placeholders.includes(txt) ? 'N/A' : txt;
            };
            
            var date = getT('.visit-date');
            var vitals = getT('.vitals-text');
            var notes = getT('.exam-notes');
            var labs = getT('.lab-results-text');
            
            // Original multi-line prescription print layout
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

            // Compact flow: Date -> Vitals -> Investigations -> Notes -> Prescriptions
            var fullText = \"--- CLINICAL ENCOUNTER: \" + date + \" ---\\n\" +
                           \"VITALS: \" + vitals + \"\\n\\n\" +
                           \"INVESTIGATIONS: \" + labs + \"\\n\\n\" + 
                           \"EXAM & PLAN:\\n\" + notes + \"\\n\\n\" +
                           \"PRESCRIPTIONS:\\n\" + medsText;
            
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
timeline_server <- function(id, pool, current_pt, refresh_trigger = reactive(0)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Frequency Lookup (Static)
    freq_lookup <- reactive({
      tryCatch({
        df <- read.csv("freq_list.csv", header = FALSE, stringsAsFactors = FALSE)
        setNames(trimws(as.character(df[[2]])), trimws(as.character(df[[1]])))
      }, error = function(e) { NULL })
    })
    
    # 2. Reactive Data (Triggers on patient change OR refresh_trigger)
    clinical_data <- reactive({
      req(current_pt(), pool)
      refresh_trigger()
      pid <- as.character(current_pt()$id)
      
      tryCatch({
        list(
          visits = dbGetQuery(pool, paste0("SELECT visit_date::date as v_date, visit_json, clinical_notes FROM visitsmodule WHERE patient_id::text = '", pid, "' ORDER BY visit_date DESC")),
          # Fetch labs, ordering by date DESC so the most recent is first
          labs = dbGetQuery(pool, paste0("SELECT test_date::date as l_date, test_name, num_val FROM labs WHERE patient_id::text = '", pid, "' ORDER BY test_date DESC")),
          rx = dbGetQuery(pool, paste0("SELECT visit_date::date as v_date, meds_json FROM prescriptions WHERE patient_id::text = '", pid, "'"))
        )
      }, error = function(e) { 
        message("Data fetch error: ", e$message)
        NULL 
      })
    })
    
    # 3. UI Rendering
    output$timeline_render <- renderUI({
      data <- clinical_data()
      f_map <- freq_lookup()
      
      # 1. Early exit if no visits
      if (is.null(data$visits) || nrow(data$visits) == 0) {
        return(div(class="p-4 text-center text-muted", "No encounters found."))
      }
      
      # 2. Calculate Global Latest Labs (Done once for the whole timeline)
      
      latest_lab_display <- "No valid lab records found."
      
      if (!is.null(data$labs) && nrow(data$labs) > 0) {
        # 1. Take the most recent date from the first row (sorted DESC in SQL)
        l_date_raw <- data$labs$l_date[1]
        l_date_fmt <- tryCatch(format(as.Date(l_date_raw), "%d %b %Y"), error = function(e) as.character(l_date_raw))
        
        # 2. Filter for records on that date AND where num_val is NOT NA/Empty
        latest_records <- data$labs[data$labs$l_date == l_date_raw, ]
        
        # Remove rows where num_val is NA, NULL, or empty string
        valid_records <- latest_records[!is.na(latest_records$num_val) & 
                                          latest_records$num_val != "" & 
                                          latest_records$num_val != "NULL", ]
        
        if (nrow(valid_records) > 0) {
          lab_items <- sapply(1:nrow(valid_records), function(idx) {
            paste0(valid_records$test_name[idx], ": ", valid_records$num_val[idx])
          })
          latest_lab_display <- paste0(l_date_fmt, " — ", paste(lab_items, collapse = " | "))
        } else {
          latest_lab_display <- paste0(l_date_fmt, " — No parameters recorded for this date.")
        }
      }
      
      # 3. Generate Timeline Cards
      tagList(
        lapply(1:nrow(data$visits), function(i) {
          # --- INITIALIZE VARIABLES (Prevents 'Object Not Found') ---
          v_json <- list(vitals = list(bp="-", hr="-", weight="-"), clinic_notes = "No notes.")
          rx_ui <- div(class="p-2 text-muted", "None recorded.")
          
          # Extract this specific row
          this_visit <- data$visits[i, ]
          raw_v_date <- this_visit$v_date
          
          # Safe Date Formatting for Header
          clean_v_date <- tryCatch(format(as.Date(raw_v_date), "%d %b %Y"), error = function(e) as.character(raw_v_date))
          
          # Parse Vitals/Notes JSON
          if (!is.na(this_visit$visit_json) && this_visit$visit_json != "") {
            v_json_parsed <- tryCatch(jsonlite::fromJSON(this_visit$visit_json, simplifyVector = FALSE), error = function(e) NULL)
            if(!is.null(v_json_parsed)) v_json <- v_json_parsed
          }
          
          # Prescription Logic
          if (!is.null(data$rx) && nrow(data$rx) > 0) {
            day_rx <- data$rx[which(data$rx$v_date == raw_v_date), ]
            if (nrow(day_rx) > 0) {
              m_df <- tryCatch(jsonlite::fromJSON(day_rx$meds_json[1]), error = function(e) NULL)
              if (is.data.frame(m_df) && nrow(m_df) > 0) {
                rx_ui <- tags$table(class="rx-table border",
                                    tags$thead(tags$tr(tags$th("Brand"), tags$th("Generic"), tags$th("Dose"), tags$th("Freq"), tags$th("Route"), tags$th("Dur"))),
                                    tags$tbody(lapply(1:nrow(m_df), function(j) {
                                      row_vals <- as.character(m_df[j, ])
                                      translated <- sapply(row_vals, function(cell) {
                                        clean <- trimws(cell); if (!is.null(f_map) && clean %in% names(f_map)) return(unname(f_map[clean])); return(cell)
                                      })
                                      tags$tr(
                                        tags$td(strong(translated[1])), tags$td(span(translated[2], style="font-size:0.75rem; color:#666")),
                                        tags$td(translated[3]), tags$td(span(class="freq-badge", translated[4])),
                                        tags$td(translated[5]), tags$td(if(length(translated)>=6) translated[6] else "-")
                                      )
                                    }))
                )
              }
            }
          }
          
          # --- CONSTRUCT UI CARD ---
          card(class="visit-card shadow-sm border-0",
               card_header(class="bg-white d-flex justify-content-between align-items-center",
                           span(strong(clean_v_date), class="text-primary visit-date"),
                           tags$button(class="btn btn-sm btn-outline-secondary", onclick="copyClinicalVisit(this)", "Copy Visit")
               ),
               card_body(
                 div(class="vitals-strip d-flex gap-4 mb-3 vitals-text",
                     span(strong("BP: "), v_json$vitals$bp %||% "-"), 
                     span(strong("P: "), v_json$vitals$hr %||% "-"), 
                     span(strong("Wt: "), v_json$vitals$weight %||% "-", "kg")),
                 
                 layout_column_wrap(width = 1,
                                    div(tags$h6("Examination & Plan", class="border-bottom text-muted pb-1"), 
                                        div(class="clinical-text exam-notes", v_json$clinic_notes %||% this_visit$clinical_notes %||% "No notes.")),
                                    
                                    div(tags$h6("Latest Lab Parameters", class="text-info border-bottom pb-1"),
                                        div(class="lab-text lab-results-text", 
                                            style="background: #f0f9ff; border-left: 4px solid #0dcaf0;",
                                            latest_lab_display)),
                                    
                                    div(tags$h6("Prescriptions", class="text-success border-bottom pb-1"), rx_ui)
                 )
               )
          )
        })
      )
    })
  })
}