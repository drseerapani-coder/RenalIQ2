library(shiny)
library(bslib)
library(DBI)
library(jsonlite)

# --- UI FUNCTION ---
timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .visit-card { border-left: 6px solid #0d6efd; margin-bottom: 30px; border-radius: 8px; }
      .vitals-strip { background: #f8f9fa; border-radius: 6px; padding: 12px; font-size: 0.9rem; border: 1px solid #e9ecef; }
      .clinical-text { white-space: pre-wrap; font-size: 1rem; line-height: 1.6; color: #2c3e50; }
      .rx-table { width: 100%; font-size: 0.85rem; margin-top: 10px; border-collapse: collapse; background: white; }
      .rx-table th { background: #f1f3f5; color: #198754; font-weight: 600; padding: 8px; border-bottom: 2px solid #dee2e6; text-align: left; }
      .rx-table td { padding: 8px; border-bottom: 1px solid #eee; vertical-align: top; }
      .freq-badge { font-weight: 600; color: #0d6efd; background: #e7f1ff; padding: 2px 5px; border-radius: 4px; border: 1px solid #cfe2ff; }
    ")),
    
    tags$script(HTML("
      /* GLOBAL FUNCTION: Added to window to ensure visibility to buttons */
      window.copyClinicalVisit = function(btn) {
        try {
            var card = btn.closest('.card');
            if (!card) return;
            
            // Helper to get text by selector
            var getT = function(selector) {
                var el = card.querySelector(selector);
                return el ? el.innerText.trim() : 'N/A';
            };
            
            var date = getT('.visit-date');
            var vitals = getT('.vitals-text');
            var notes = getT('.exam-notes');
            
            // Build Prescription Text
            var rows = card.querySelectorAll('.rx-table tbody tr');
            var medsText = '';
            
            if (rows.length > 0) {
                medsText = Array.from(rows).map(function(tr) {
                    var c = Array.from(tr.querySelectorAll('td')).map(function(td) {
                        return td.innerText.trim().replace(/\\n/g, ' ');
                    });
                    // Format for readability in Word/Email
                    return c[0] + ' (' + c[1] + ')\\n   Dose: ' + c[2] + ' | Freq: ' + c[3] + ' | Route: ' + c[4] + ' | Dur: ' + c[5];
                }).join('\\n\\n');
            } else {
                medsText = 'No prescriptions recorded.';
            }
            
            var fullText = \"--- CLINICAL ENCOUNTER: \" + date + \" ---\\n\" +
                           \"VITALS: \" + vitals + \"\\n\\n\" +
                           \"EXAM & PLAN:\\n\" + notes + \"\\n\\n\" +
                           \"PRESCRIPTIONS:\\n\" + medsText;
            
            // Execution of Copy
            var textArea = document.createElement('textarea');
            textArea.value = fullText;
            document.body.appendChild(textArea);
            textArea.select();
            
            var successful = document.execCommand('copy');
            document.body.removeChild(textArea);
            
            if (successful) {
                var oldHtml = btn.innerHTML;
                btn.innerHTML = '✓ Copied';
                btn.classList.replace('btn-outline-secondary', 'btn-success');
                setTimeout(function() {
                    btn.innerHTML = oldHtml;
                    btn.classList.replace('btn-success', 'btn-outline-secondary');
                }, 2000);
            }
        } catch (err) {
            console.error('Copy failed:', err);
            alert('Unable to copy. Please check console.');
        }
      };
    ")),
    uiOutput(ns("timeline_render"))
  )
}

# --- SERVER FUNCTION ---
timeline_server <- function(id, pool, current_pt, refresh_trigger = reactive(0)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    freq_lookup <- reactive({
      tryCatch({
        df <- read.csv("freq_list.csv", header = FALSE, stringsAsFactors = FALSE)
        setNames(trimws(as.character(df[[2]])), trimws(as.character(df[[1]])))
      }, error = function(e) { NULL })
    })
    
    clinical_data <- reactive({
      req(current_pt(), pool)
      refresh_trigger()
      pid <- as.character(current_pt()$id)
      tryCatch({
        list(
          visits = dbGetQuery(pool, paste0("SELECT visit_date::text as v_date, visit_json, clinical_notes FROM visitsmodule WHERE patient_id::text = '", pid, "' ORDER BY visit_date DESC")),
          rx = dbGetQuery(pool, paste0("SELECT visit_date::text as v_date, meds_json FROM prescriptions WHERE patient_id::text = '", pid, "'"))
        )
      }, error = function(e) { NULL })
    })
    
    output$timeline_render <- renderUI({
      data <- clinical_data()
      f_map <- freq_lookup()
      if (is.null(data) || is.null(data$visits) || nrow(data$visits) == 0) return(div(class="p-4 text-center text-muted", "No encounters found."))
      
      tagList(
        lapply(1:nrow(data$visits), function(i) {
          this_v_date <- data$visits$v_date[i]
          v_json <- tryCatch(jsonlite::fromJSON(data$visits$visit_json[i], simplifyVector = FALSE), error = function(e) list())
          day_rx <- data$rx[data$rx$v_date == this_v_date, ]
          
          rx_ui <- if(nrow(day_rx) > 0) {
            m_df <- tryCatch(jsonlite::fromJSON(day_rx$meds_json[1]), error = function(e) NULL)
            if(is.data.frame(m_df) && nrow(m_df) > 0) {
              tags$table(class="rx-table border",
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
            } else { div(class="p-2 text-muted", "Format error.") }
          } else { div(class="p-2 text-muted", "None.") }
          
          card(class="visit-card shadow-sm border-0",
               card_header(class="bg-white d-flex justify-content-between align-items-center",
                           span(strong(format(as.Date(this_v_date), "%d %b %Y")), class="text-primary visit-date"),
                           tags$button(class="btn btn-sm btn-outline-secondary", onclick="copyClinicalVisit(this)", "Copy Visit")
               ),
               card_body(
                 div(class="vitals-strip d-flex gap-4 mb-3 vitals-text",
                     span(strong("BP: "), v_json$vitals$bp %||% "-"), 
                     span(strong("P: "), v_json$vitals$hr %||% "-"), 
                     span(strong("Wt: "), v_json$vitals$weight %||% "-", "kg")),
                 layout_column_wrap(width = 1,
                                    div(tags$h6("Examination & Plan", class="border-bottom text-muted pb-1"), 
                                        div(class="clinical-text exam-notes", v_json$clinic_notes %||% data$visits$clinical_notes[i] %||% "No notes.")),
                                    div(tags$h6("Prescriptions", class="text-success border-bottom pb-1"), rx_ui)
                 )
               )
          )
        })
      )
    })
  })
}