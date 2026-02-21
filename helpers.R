# helpers.R

# --- Database Wrapper ---
# Ensures safe checkout/return of connections from the pool
.with_conn <- function(pool, code) {
  if (is.null(pool)) stop("Database pool is NULL")
  
  if (inherits(pool, "Pool")) {
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)
  } else {
    con <- pool
  }
  
  force(code)
}

# --- String & Data Cleaning ---
clean_phone <- function(phone_str) {
  if (is.null(phone_str) || is.na(phone_str)) return("")
  nums <- stringr::str_replace_all(phone_str, "[^0-9]", "")
  if (nchar(nums) == 12 && stringr::str_starts(nums, "91")) nums <- stringr::str_sub(nums, 3)
  if (nchar(nums) > 10) nums <- stringr::str_sub(nums, -10)
  return(nums)
}

parse_patient_text <- function(txt) {
  res <- list()
  res$hospital_number <- stringr::str_extract(txt, "(?i)UHID\\s*:\\s*([A-Z0-9.]+)") %>% 
    stringr::str_remove("(?i)UHID\\s*:\\s*")
  
  name_match <- stringr::str_extract(txt, "(?i)(Mr\\.|Ms\\.|Mrs\\.|Master)\\s+([^.\\n]+)")
  if (!is.na(name_match)) {
    full_name <- stringr::str_remove(name_match, "(?i)(Mr\\.|Ms\\.|Mrs\\.|Master)\\s+") %>% trimws()
    parts <- stringr::str_split(full_name, "\\s+")[[1]]
    res$first_name <- parts[1]
    res$last_name <- if(length(parts) > 1) paste(parts[2:length(parts)], collapse=" ") else ""
  }
  
  if (stringr::str_detect(txt, "(?i)Male")) res$gender <- "male"
  else if (stringr::str_detect(txt, "(?i)Female")) res$gender <- "female"
  
  dob_match <- stringr::str_extract(txt, "\\d{1,2}-[A-Za-z]{3}-\\d{4}")
  if (!is.na(dob_match)) res$dob <- as.Date(dob_match, format = "%d-%b-%Y")
  
  res$phone <- stringr::str_extract(txt, "(?i)Phone\\s*:\\s*([0-9-]+)") %>% 
    stringr::str_remove("(?i)Phone\\s*:\\s*")
  
  addr_match <- stringr::str_extract(txt, "(?i)Address\\s*:\\s*(.*?)(\\n|Phone|$)") %>% 
    stringr::str_remove("(?i)Address\\s*:\\s*") %>% trimws()
  res$address1 <- addr_match
  return(res)
}

# --- UI Helpers ---
safe_update_input <- function(session, id, value) {
  lname <- tolower(id)
  is_empty <- is.null(value) || is.na(value) || (is.character(value) && !nzchar(trimws(value)))
  
  if (grepl("date|dob|birth", lname)) {
    updateDateInput(session, id, value = if (is_empty) NA else format(as.Date(value), "%Y-%m-%d"))
  } else if (grepl("gender", lname)) {
    updateRadioButtons(session, id, selected = if (is_empty) character(0) else tolower(as.character(value)))
  } else {
    updateTextInput(session, id, value = if (is_empty) "" else toupper(as.character(value)))
  }
}

# helpers.R

#' Log an action to the audit_logs table
#' @param con An active DBI connection
#' @param user_id The username/ID of the current user
#' @param action "CREATE", "UPDATE", or "DELETE"
#' @param table The table being modified
#' @param record_id The ID of the record affected
log_audit <- function(con, user_id, action, table, record_id) {
  tryCatch({
    DBI::dbExecute(con, 
                   "INSERT INTO audit_logs (user_id, action_type, table_name, record_id, timestamp) 
       VALUES ($1, $2, $3, $4, NOW())",
                   list(user_id, action, table, as.character(record_id))
    )
  }, error = function(e) {
    warning("Audit log failed: ", e$message)
  })
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b