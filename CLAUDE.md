# CLAUDE.md — RenalIQ2 Codebase Guide

This document is intended for AI assistants (Claude and similar) to understand the RenalIQ2 codebase: its structure, conventions, workflows, and key patterns.

---

## Project Overview

**RenalIQ2** is a clinical patient management system focused on nephrology/renal health. It is a modular R/Shiny web application deployed via Docker on DigitalOcean.

- **Language:** R
- **Framework:** Shiny (web UI framework for R)
- **Database:** PostgreSQL (DigitalOcean Managed Database, SSL-required)
- **AI Integration:** OpenAI GPT-4o-mini for AI-powered lab document ingestion
- **Deployment:** Docker container (`rocker/shiny` base image), port 8080
- **~3,000 lines of R** across 13 module files

---

## Repository Structure

```
RenalIQ2/
├── app.R                    # Main entry point — UI layout + server orchestration
├── helpers.R                # Shared utilities: DB helpers, data parsing, UI helpers
├── auth_module.R            # Login/logout authentication module
├── mod_registration.R       # Patient registration & search
├── mod_clinical.R           # Clinical visit notes & vitals (primary)
├── mod_clinical_templates.R # Template-driven clinical notes (alternate)
├── mod_mobile_rx_1.R        # Prescription management (current version)
├── mod_mobile_rx.R          # Prescription management (legacy/alternate)
├── mod_lab_ingestion.R      # AI-powered lab PDF/image ingestion
├── mod_lab_flowsheet.R      # Lab results flowsheet (editable table)
├── mod_lab_flowsheet_1.R    # Lab flowsheet (alternate version)
├── mod_user_mgmt.R          # Admin: user management & daily review panel
├── clinical_summary.R       # Patient visit timeline module
├── Dockerfile               # Docker build configuration
├── lab_targets.csv          # Reference: 61 lab tests with normal ranges & units
├── freq_list.csv            # Reference: 57 medication frequency code mappings
├── templates.xlsx           # Excel file: clinical examination templates
├── ca-certificate.crt       # SSL certificate for PostgreSQL connection
└── .gitignore               # R standard ignores (.Rproj.user, .Rhistory, etc.)
```

---

## Technology Stack

### Core R Packages

| Package | Purpose |
|---|---|
| `shiny` | Web application framework |
| `bslib` | Bootstrap 5 theming (teal `#26A69A` primary color) |
| `pool` | Database connection pooling |
| `DBI` + `RPostgres` | PostgreSQL database interface |
| `dplyr`, `tidyr` | Data manipulation |
| `lubridate` | Date/time operations |
| `stringr` | String manipulation |
| `DT` | DataTables for interactive tables |
| `shinyjs` | JavaScript integration in Shiny |
| `jsonlite` | JSON parsing/serialization |
| `glue` | String templating (`glue("Hello {name}")`) |
| `rhandsontable` | Editable spreadsheet-style tables |
| `shinycssloaders` | Loading spinners |
| `pdftools` | PDF text extraction |
| `tesseract` | OCR (optical character recognition) |
| `openai` | OpenAI API client |
| `sodium` | Password hashing (Argon2/bcrypt) |
| `readxl` | Reading Excel files (templates.xlsx) |

---

## Architecture: Shiny Module Pattern

The app uses **Shiny's namespaced module pattern**. Every module has two exported functions:

```r
# UI function — takes an `id` argument, uses NS() for namespacing
module_name_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements use ns("element_id") for namespacing
    textInput(ns("field"), "Label")
  )
}

# Server function — takes id, pool (DB), and shared reactives
module_name_server <- function(id, pool, current_pt) {
  moduleServer(id, function(input, output, session) {
    # Server logic here
  })
}
```

### Module Wiring in `app.R`

Modules are instantiated in `server()` with consistent IDs:

```r
auth_server("auth", pool)
reg_server("reg", pool, current_pt)
clin_server("clin", pool, current_pt)
rx_server("rx", pool, current_pt)
lab_server("lab", pool, current_pt)
lab_ingest_server("lab_ingest", pool, current_pt)
pt_timeline_server("timeline", pool, current_pt)
user_mgmt_server("user_mgmt", pool)
```

### Global State

`current_pt` is the central reactive value passed to all modules:

```r
current_pt <- reactiveVal(NULL)  # NULL = no patient selected
# Set when user selects a patient in Registration module
# All other modules observe current_pt() to load patient data
```

---

## Database

### Connection

Defined in `app.R` via `pool::dbPool()`:

```r
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname   = Sys.getenv("DO_DB_NAME"),
  host     = Sys.getenv("DO_DB_HOST"),
  user     = Sys.getenv("DO_DB_USER"),
  password = Sys.getenv("DO_DB_PASSWORD"),
  port     = as.integer(Sys.getenv("DO_DB_PORT", 25060)),
  sslmode  = "require",
  sslrootcert = "ca-certificate.crt"
)
```

### Database Helper (from `helpers.R`)

Always use `.with_conn()` to safely borrow a connection from the pool:

```r
.with_conn <- function(pool, expr) {
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con))
  expr(con)
}

# Usage pattern:
.with_conn(pool, function(con) {
  dbGetQuery(con, "SELECT * FROM registrations WHERE id = $1", list(patient_id))
})
```

**Never use `pool` directly with `DBI::dbGetQuery()`** — always go through `.with_conn()`.

### Tables

| Table | Description |
|---|---|
| `registrations` | Patient master data (demographics, allergies, comments) |
| `users` | Authentication (username, password_hash, role) |
| `visitsmodule` | Clinical visits (patient_id, visit_date, visit_json JSON blob) |
| `past_medical_history` | Patient PMHx (global, not per-visit) |
| `labs` | Lab results (patient_id, test_name, test_date, num_val, value_text) |
| `audit_logs` | Audit trail for all data changes |

### Key Schema Notes

- Visit data is stored as a JSON blob in `visitsmodule.visit_json`
- Lab results have both `num_val` (numeric) and `value_text` (text) columns
- `registrations.hospital_number` = UHID (Unique Hospital ID)
- All user lookups use `LOWER()` for case-insensitive username comparison

### Audit Logging

All data mutations must call `log_audit()`:

```r
log_audit <- function(pool, user_id, action_type, table_name, record_id) {
  # Inserts a row into audit_logs
}
```

---

## Environment Variables

These must be set for the app to run:

```
DO_DB_NAME        PostgreSQL database name
DO_DB_HOST        PostgreSQL host (DigitalOcean managed DB)
DO_DB_USER        PostgreSQL username
DO_DB_PASSWORD    PostgreSQL password
DO_DB_PORT        PostgreSQL port (default: 25060)
OPENAI_API_KEY    OpenAI API key (required for AI lab ingestion)
```

No `.env` file is committed. Set via Docker environment or deployment platform config.

---

## Key Modules — Detailed Notes

### `auth_module.R` — Authentication

- Uses `sodium` for password verification (never store plaintext passwords)
- Queries `users` table with `LOWER(username) = LOWER($1)`
- Returns `list(logged_in = TRUE/FALSE, user_id, username, role)`
- Role values: `"Clinician"`, `"Administrator"`

### `mod_registration.R` — Patient Registration

- Search accepts: name fragments, phone number, UHID
- Results limited to 10 rows
- Selecting a row sets `current_pt()` reactive
- New patient form validates: first_name, last_name, dob, phone (required)
- `clean_phone()` from helpers.R normalizes phone numbers (strips India +91 prefix)

### `mod_clinical.R` — Clinical Notes

- One visit = one row in `visitsmodule` with a JSON blob for all fields
- Vitals: BP (systolic/diastolic), HR, weight, temperature
- Follow-up scheduling automatically skips Sundays
- Visit state: `locked` (read-only) vs unlocked (editable)
- PMHx is patient-global — changes affect all visits
- Quick follow-up intervals: `10D`, `1W`, `2W`, `3W`, `1M`, `6W`, `3M`, `4M`, `6M`, `1Y`

### `mod_mobile_rx_1.R` — Prescriptions (Current)

- Medication row columns: `brand_name`, `generic`, `dose`, `freq`, `route`, `duration`
- Frequency codes are composite strings (e.g., `od8` = once daily at 8AM, `bd8` = 8AM-8PM)
- `freq_list.csv` maps codes to human-readable labels
- BD splits into two time slots; TDS/QID into 3-4 slots

### `mod_lab_ingestion.R` — AI Lab Ingestion

- Accepts PDF, PNG, JPEG uploads (multiple files)
- Pipeline: File upload → OCR (pdftools/tesseract) → GPT-4o-mini → parsed JSON → validation
- OpenAI model: `gpt-4o-mini`, temperature: `0` (deterministic output)
- Cross-validates extracted tests against `lab_targets.csv`
- Synonym redirects: e.g., `"Albumin: Creatinine"` → `"UACR"`, `"Protein: Creatinine"` → `"UPCR"`
- User must confirm before saving to DB

### `mod_lab_flowsheet.R` — Lab Flowsheet

- Displays labs in wide pivot format: rows = test names, columns = dates
- Uses `rhandsontable` for inline editing
- Date display format: `DD-Mon-YY` (e.g., `25-Feb-26`)
- First column (Parameter) is read-only
- Right-click on column header to delete a date column
- Reference ranges loaded from `lab_targets.csv`

### `mod_user_mgmt.R` — Admin Panel

- Role-restricted: only `"Administrator"` role sees this panel
- User creation: inserts into `users` table with hashed password (`sodium::password_store()`)
- Daily review panel: shows visits by date + follow-ups due by date

### `clinical_summary.R` — Timeline

- Chronological list of all visits for the selected patient
- Each card shows: date, vitals, lab results, clinical notes, prescriptions
- "Copy" button uses JavaScript to copy formatted text to clipboard

---

## Data Reference Files

### `lab_targets.csv`

61 lab tests organized by category. Columns: `category`, `test_name`, `low_limit`, `high_limit`, `unit`, `type`

Categories: Renal/Electrolytes, Minerals/Bone, Liver Function, Hematology, Metabolic/Iron, Inflammatory/Misc, Immunosuppression, Urine, Specialized

Key renal tests: Creatinine, eGFR, BUN, Potassium, Sodium, Bicarbonate, Phosphorus, Calcium, UACR, UPCR

### `freq_list.csv`

57 medication frequency codes. Format: `code,description`

Examples: `od8` → `8AM`, `bd8` → `8AM - 8PM`, `tds` → `8AM - 2PM - 10PM`, `weekly` → `Weekly once`

### `templates.xlsx`

Excel-based clinical examination templates loaded by `mod_clinical_templates.R` for structured note taking.

---

## Utility Functions (`helpers.R`)

```r
# Null-coalescing operator
value %||% default

# Safe DB connection checkout
.with_conn(pool, function(con) { ... })

# Audit logging
log_audit(pool, user_id, action_type, table_name, record_id)

# Phone number normalization
clean_phone(phone_string)   # strips +91, spaces, dashes

# Parse patient demographics from unstructured text
parse_patient_text(text)    # returns list: uhid, name, gender, dob, phone, address

# Smart input field update (handles dates, gender selects, text)
safe_update_input(session, id, value)
```

---

## Development Workflow

### Running Locally

```r
# In R or RStudio, from the project root directory:
shiny::runApp()

# Or with specific port:
shiny::runApp(port = 3838)
```

Set environment variables before running:
```bash
export DO_DB_NAME=your_db
export DO_DB_HOST=your_host
export DO_DB_USER=your_user
export DO_DB_PASSWORD=your_password
export DO_DB_PORT=25060
export OPENAI_API_KEY=sk-...
```

### Docker Build & Run

```bash
# Build image
docker build -t renaliq2 .

# Run container
docker run -p 8080:8080 \
  -e DO_DB_NAME=... \
  -e DO_DB_HOST=... \
  -e DO_DB_USER=... \
  -e DO_DB_PASSWORD=... \
  -e DO_DB_PORT=25060 \
  -e OPENAI_API_KEY=sk-... \
  renaliq2
```

App is accessible at `http://localhost:8080`

### Adding a New Module

1. Create `mod_<name>.R` with `mod_name_ui(id)` and `mod_name_server(id, pool, current_pt)` functions
2. Add `source("mod_<name>.R")` at the top of `app.R`
3. Add UI panel to `navbarPage()` in `app.R` UI section
4. Call `mod_name_server("name", pool, current_pt)` in the `server()` function in `app.R`
5. Follow the namespace pattern: all input/output IDs go through `ns()`

---

## Coding Conventions

### Shiny Namespacing

Always use `ns <- NS(id)` in UI functions and wrap all IDs:

```r
# Correct
textInput(ns("my_field"), "Label")

# Wrong — will break in module context
textInput("my_field", "Label")
```

### Reactive Dependencies

- Use `observeEvent()` for side effects (DB writes, notifications)
- Use `reactive()` for computed values consumed by other reactives
- Use `reactiveVal()` for mutable single values
- Guard patient-dependent operations: `req(current_pt())` before accessing patient data

### Database Queries

- Use parameterized queries with `$1, $2, ...` placeholders (never string interpolation):

```r
# Correct — parameterized
dbGetQuery(con, "SELECT * FROM labs WHERE patient_id = $1 AND test_name = $2",
           list(pt_id, test_name))

# Wrong — SQL injection risk
dbGetQuery(con, paste0("SELECT * FROM labs WHERE patient_id = ", pt_id))
```

### Error Handling

Wrap all DB operations in `tryCatch()` and show notifications:

```r
tryCatch({
  .with_conn(pool, function(con) {
    dbExecute(con, "INSERT INTO ...", list(...))
  })
  showNotification("Saved successfully", type = "message")
}, error = function(e) {
  showNotification(paste("Error:", e$message), type = "error")
})
```

### Notifications

Use `showNotification()` for user feedback (not `message()` or `print()`):
- `type = "message"` — green success
- `type = "warning"` — yellow warning
- `type = "error"` — red error

---

## Security Conventions

- **Password storage:** Always use `sodium::password_store()` for new passwords, `sodium::password_verify()` for checking
- **Never commit credentials:** All secrets via environment variables only
- **SQL injection prevention:** Always use parameterized queries (`$1` placeholders)
- **DB encryption:** SSL required for all PostgreSQL connections (`sslmode = "require"`)
- **Audit trail:** Call `log_audit()` after every data mutation (INSERT/UPDATE/DELETE)
- **Role-based access:** Check `user_role()` before rendering admin UI components

---

## Testing

There is no automated test suite. Testing is done manually via the Shiny UI. When making changes:

1. Test the affected module by running the app locally
2. Verify DB operations complete without errors
3. Check that `showNotification()` messages appear correctly
4. Test with a real patient record to verify data persistence

---

## Deployment

- **Platform:** DigitalOcean App Platform (Docker-based)
- **Container:** `rocker/shiny:latest` base image
- **Port:** 8080 (exposed in Dockerfile, mapped in deployment config)
- **SSL Certificate:** `ca-certificate.crt` is bundled in the repo for PostgreSQL SSL
- **Process:** Push to `main` branch triggers deployment

---

## Common Pitfalls for AI Assistants

1. **Do not use `pool` directly with DBI functions** — always use `.with_conn(pool, fn)` wrapper
2. **Do not use string concatenation in SQL** — always use parameterized queries
3. **Always namespace UI element IDs** with `ns()` inside module UI functions
4. **`current_pt()` can be NULL** — always guard with `req(current_pt())` before patient-specific queries
5. **Visit data is JSON** in `visitsmodule.visit_json` — use `jsonlite::toJSON()` / `fromJSON()` when serializing
6. **Medication frequencies are encoded strings** (`od8`, `bd8`, etc.) — refer to `freq_list.csv` for the full mapping
7. **Lab test names must match exactly** against `lab_targets.csv` — check synonyms in `mod_lab_ingestion.R`
8. **Follow-up dates skip Sundays** — this logic is in `mod_clinical.R` and should be preserved
9. **Admin panel checks role** — wrap admin-only features in `if (user_role() == "Administrator")`
10. **The `ca-certificate.crt` file must be present** at the app root for DB connections to succeed
