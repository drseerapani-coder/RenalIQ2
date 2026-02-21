library(shiny)
library(bslib)
library(DBI)
library(jsonlite)

# --- UI FUNCTION ---
timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML("
      function copyVisit(btn) {
        var card = btn.closest('.card');
        
        const getPureText = (selector) => {
          var el = card.querySelector(selector);
          if (!el) return '';
          // Using innerText to get clean, rendered text
          return el.innerText.trim();
        };

        var date = getPureText('.visit-date');
        var followUp = getPureText('.follow-up-text');
        var vitals = getPureText('.vitals-strip').replace(/\\s+/g, ' ').trim();
        var pmhx = getPureText('.pmhx-snapshot');
        var notes = getPureText('.exam-notes');
        
        // Process medications (Already comma-separated from Server)
        var medsArr = Array.from(card.querySelectorAll('.rx-list li'));
        var meds = medsArr.length > 0 ? 
                   medsArr.map(li => li.innerText.trim()).join('\\n- ') : 
                   'No medications';
        
        var fullText = '--- CLINICAL VISIT: ' + date + ' ---\\n' +
                       followUp + '\\n' +
                       'VITALS: ' + vitals + '\\n' +
                       'PMHx: ' + pmhx + '\\n\\n' +
                       'EXAMINATION & PLAN:\\n' + notes + '\\n\\n' +
                       'PRESCRIPTIONS:\\n- ' + meds;
        
        // Force Plain Text via Textarea Buffer
        var textArea = document.createElement('textarea');
        textArea.value = fullText;
        textArea.style.position = 'fixed';
        textArea.style.left = '-9999px';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        
        try {
          document.execCommand('copy');
          var originalHtml = btn.innerHTML;
          btn.innerHTML = '✓ Copied!';
          btn.classList.replace('btn-outline-secondary', 'btn-success');
          setTimeout(function() {
            btn.innerHTML = originalHtml;
            btn.classList.replace('btn-success', 'btn-outline-secondary');
          }, 2000);
        } catch (err) { console.error('Copy failed', err); }
        
        document.body.removeChild(textArea);
      }
    ")),
    
    card(
      full_screen = TRUE,
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span(icon("file-medical-alt"), " Longitudinal Patient Summary"),
            span(class = "badge bg-info", "Chronological Order"))
      ),
      card_body(
        style = "background-color: #f4f6f9;",
        uiOutput(ns("timeline_container"))
      )
    )
  )
}

# --- SERVER FUNCTION ---
# --- SERVER FUNCTION ---
timeline_server <- function(id, pool, current_pt) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$timeline_container <- renderUI({
      req(current_pt())
      pid <- current_pt()$id
      
      # 1. Fetch PMHx & Visits & Rx
      pmhx_data <- dbGetQuery(pool, "SELECT condition_text FROM past_medical_history WHERE registration_id::text = $1", list(as.character(pid)))
      pmhx_summary <- if(nrow(pmhx_data) > 0) paste(pmhx_data$condition_text, collapse = ", ") else "None recorded"
      
      visits <- dbGetQuery(pool, "SELECT visit_date, visit_json FROM visitsmodule WHERE patient_id::text = $1 ORDER BY visit_date DESC", list(as.character(pid)))
      all_rx <- dbGetQuery(pool, "SELECT visit_date, meds_json FROM prescriptions WHERE patient_id::text = $1", list(as.character(pid)))
      
      if (nrow(visits) == 0) return(div(class="text-center p-5", h4("No clinical records found.", class="text-muted")))
      
      tagList(
        lapply(1:nrow(visits), function(i) {
          v_date <- as.Date(visits$visit_date[i])
          # simplifyVector = FALSE prevents the $ operator error from earlier
          v_data <- jsonlite::fromJSON(visits$visit_json[i], simplifyVector = FALSE)
          
          # --- Process Dynamic Template Fields ---
          template_ui <- if (!is.null(v_data$template_fields) && length(v_data$template_fields) > 0) {
            tags$div(class = "template-responses mb-2 p-2 rounded", style = "background-color: #f8f9fa; border-left: 3px solid #0d6efd;",
                     tags$small(class="text-muted fw-bold text-uppercase", "Systemic Examination:"),
                     tags$ul(class="list-unstyled mb-0", style="font-size: 0.9rem;",
                             lapply(v_data$template_fields, function(f) {
                               if (is.list(f) && !is.null(f$label) && nzchar(f$value %||% "")) {
                                 tags$li(tags$strong(paste0(f$label, ": ")), f$value)
                               } else if (is.character(f) && nzchar(f)) {
                                 # Fallback for old simple-string formats
                                 tags$li(f)
                               } else { NULL }
                             })
                     )
            )
          } else { NULL }
          
          # --- Process Medications ---
          day_rx <- all_rx[as.Date(all_rx$visit_date) == v_date, ]
          rx_ui <- if(nrow(day_rx) > 0 && nzchar(day_rx$meds_json[1])) {
            rx_df <- tryCatch({ jsonlite::fromJSON(day_rx$meds_json[1]) }, error = function(e) NULL)
            if (!is.null(rx_df) && nrow(rx_df) > 0) {
              tags$ul(class="ps-3 mb-0 rx-list", 
                      lapply(1:nrow(rx_df), function(j) {
                        b_name <- rx_df[j, "brand_name"] %||% rx_df[j, "brand"] %||% "Unknown"
                        med_string <- paste(na.omit(c(b_name, rx_df[j, "dose"], rx_df[j, "freq"])), collapse = ", ")
                        tags$li(med_string)
                      })
              )
            } else { span("No medications found.", class="text-muted italic") }
          } else { span("No medications prescribed.", class="text-muted italic") }
          
          # --- CARD UI ---
          div(class = "card mb-4 border-0 shadow-sm",
              div(class = "card-header d-flex justify-content-between bg-white border-bottom align-items-center",
                  div(
                    span(strong(format(v_date, "%d %b %Y")), class = "text-primary visit-date"),
                    tags$button(class = "btn btn-sm btn-outline-secondary ms-3", onclick = "copyVisit(this)", icon("copy"), " Copy")
                  ),
                  span(class = "text-danger follow-up-text", strong("Follow-up: "), v_data$followup_date %||% "N/A")
              ),
              div(class = "card-body",
                  # Vitals Strip
                  div(class = "vitals-strip p-2 mb-3 rounded-2 d-flex flex-wrap gap-3", style="background-color: #f1f3f5; font-size: 0.85rem; border-left: 4px solid #adb5bd;",
                      span(strong("BP: "), v_data$vitals$bp %||% "-"),
                      span(strong("Pulse: "), v_data$vitals$hr %||% "-"),
                      span(strong("Temp: "), v_data$vitals$temp %||% "-"),
                      span(strong("Wt: "), v_data$vitals$weight %||% "-", "kg")
                  ),
                  layout_column_wrap(
                    width = 1/2,
                    div(
                      div(class="mb-2", tags$small(class="text-uppercase fw-bold text-muted", "PMHx: "), tags$small(pmhx_summary, class="pmhx-snapshot")),
                      tags$h6("Examination & Plan", class="border-bottom pb-1 mt-2"),
                      
                      # NEW: Injected Template Data
                      template_ui, 
                      
                      p(v_data$clinic_notes, class="mt-2 exam-notes", style="white-space: pre-wrap; font-size: 0.95rem;")
                    ),
                    div(
                      tags$h6("Prescriptions", class="border-bottom pb-1 text-success"),
                      div(class = "p-2 bg-light rounded", rx_ui)
                    )
                  )
              )
          )
        })
      )
    })
  })
}