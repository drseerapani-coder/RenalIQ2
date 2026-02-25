# migrate.R — Database migration runner for RenalIQ2
#
# Usage (called automatically from app.R on startup):
#   run_migrations(pool)
#
# How it works:
#   1. Creates a `schema_migrations` tracking table if it does not exist.
#   2. Reads all *.sql files from the migrations/ directory in sorted order.
#   3. Skips files already recorded in schema_migrations.
#   4. Applies each pending file inside a transaction (BEGIN / COMMIT).
#      Rolls back and stops on any error to prevent partial migrations.
#   5. Records the filename in schema_migrations after successful apply.
#
# Adding a new migration:
#   Create migrations/NNN_description.sql (e.g. 003_add_patient_notes.sql).
#   The file will be picked up automatically on the next app startup.
#
# Notes:
#   - All CREATE TABLE / CREATE INDEX statements use IF NOT EXISTS, so
#     migration 001 is safe to run against an already-provisioned database.
#   - SQL files may contain multiple statements separated by semicolons.
#     Comment lines (--) are stripped before execution.

run_migrations <- function(pool) {
  if (is.null(pool)) {
    message("[migrate] Pool is NULL — skipping migrations.")
    return(invisible(NULL))
  }

  con <- tryCatch(
    pool::poolCheckout(pool),
    error = function(e) {
      message("[migrate] Cannot check out DB connection: ", e$message)
      NULL
    }
  )
  if (is.null(con)) return(invisible(NULL))
  on.exit(pool::poolReturn(con), add = TRUE)

  tryCatch({

    # ── 1. Ensure the tracking table exists ──────────────────────────────────
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS schema_migrations (
        id         SERIAL       PRIMARY KEY,
        filename   VARCHAR(255) NOT NULL UNIQUE,
        applied_at TIMESTAMPTZ  NOT NULL DEFAULT NOW()
      )
    ")

    # ── 2. Locate migration files ─────────────────────────────────────────────
    mig_dir <- file.path(getwd(), "migrations")
    if (!dir.exists(mig_dir)) {
      message("[migrate] No migrations/ directory found — skipping.")
      return(invisible(NULL))
    }

    files <- sort(list.files(mig_dir, pattern = "^[0-9]+.*\\.sql$"))
    if (length(files) == 0) {
      message("[migrate] No migration files found.")
      return(invisible(NULL))
    }

    # ── 3. Determine which files are pending ──────────────────────────────────
    applied <- DBI::dbGetQuery(con, "SELECT filename FROM schema_migrations")$filename
    pending <- files[!files %in% applied]

    if (length(pending) == 0) {
      message("[migrate] All ", length(files), " migration(s) already applied.")
      return(invisible(NULL))
    }

    message("[migrate] ", length(pending), " pending migration(s) out of ",
            length(files), " total.")

    # ── 4. Apply each pending migration in its own transaction ────────────────
    for (filename in pending) {
      filepath <- file.path(mig_dir, filename)
      sql_raw  <- paste(readLines(filepath, warn = FALSE), collapse = "\n")

      # Split on semicolons; discard chunks that are blank or comment-only
      stmts <- strsplit(sql_raw, ";")[[1]]
      stmts <- Filter(
        function(s) nzchar(trimws(gsub("--[^\n]*", "", s))),
        stmts
      )

      message("[migrate] Applying: ", filename, " (", length(stmts), " statement(s))")

      DBI::dbBegin(con)
      tryCatch({
        for (stmt in stmts) {
          DBI::dbExecute(con, trimws(stmt))
        }
        DBI::dbExecute(
          con,
          "INSERT INTO schema_migrations (filename) VALUES ($1)",
          list(filename)
        )
        DBI::dbCommit(con)
        message("[migrate] Applied:  ", filename)
      }, error = function(e) {
        DBI::dbRollback(con)
        stop(paste0("Migration failed [", filename, "]: ", e$message))
      })
    }

    message("[migrate] Done. Applied ", length(pending), " migration(s).")

  }, error = function(e) {
    message("[migrate] ERROR — ", e$message)
    stop(e)
  })
}
