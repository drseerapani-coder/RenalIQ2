-- Migration 001: Initial schema for RenalIQ2
-- Creates all application tables using IF NOT EXISTS.
-- Safe to run against an existing database -- no data is touched.

-- Users must be created first (other tables reference it via created_by text).
CREATE TABLE IF NOT EXISTS users (
  id            SERIAL       PRIMARY KEY,
  username      VARCHAR(100) NOT NULL UNIQUE,
  full_name     VARCHAR(200),
  role          VARCHAR(50)  NOT NULL DEFAULT 'Clinician',
  password_hash TEXT         NOT NULL,
  created_at    TIMESTAMPTZ  NOT NULL DEFAULT NOW()
);

-- Patient master data
CREATE TABLE IF NOT EXISTS registrations (
  id              SERIAL      PRIMARY KEY,
  hospital_number TEXT,
  first_name      TEXT        NOT NULL,
  last_name       TEXT        NOT NULL,
  dob             DATE        NOT NULL,
  gender          VARCHAR(10),
  phone           TEXT        NOT NULL,
  address1        TEXT,
  allergies       TEXT,
  comments        TEXT,
  created_by      TEXT,
  updated_by      TEXT,
  updated_at      TIMESTAMPTZ,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Clinical visits (vitals + notes stored as a JSON blob in visit_json)
CREATE TABLE IF NOT EXISTS visitsmodule (
  id             SERIAL  PRIMARY KEY,
  patient_id     INTEGER NOT NULL REFERENCES registrations(id) ON DELETE CASCADE,
  visit_date     DATE    NOT NULL,
  visit_json     JSONB,
  clinical_notes TEXT,
  created_by     TEXT,
  updated_by     TEXT,
  updated_at     TIMESTAMPTZ,
  created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Patient past medical history (global, not per-visit)
CREATE TABLE IF NOT EXISTS past_medical_history (
  id              SERIAL  PRIMARY KEY,
  registration_id INTEGER NOT NULL REFERENCES registrations(id) ON DELETE CASCADE,
  condition_text  TEXT,
  onset_date      TEXT,   -- stored as a free-text year string, e.g. "2019"
  created_by      TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Lab results (one row per test per date; UNIQUE enforces upsert logic)
CREATE TABLE IF NOT EXISTS labs (
  id         SERIAL  PRIMARY KEY,
  patient_id INTEGER NOT NULL REFERENCES registrations(id) ON DELETE CASCADE,
  test_name  TEXT    NOT NULL,
  test_date  DATE    NOT NULL,
  num_val    NUMERIC,
  unit       TEXT,
  value_text TEXT,
  created_by TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_by TEXT,
  updated_at TIMESTAMPTZ,
  UNIQUE (patient_id, test_date, test_name)
);

-- Prescriptions (one row per patient per visit date; UNIQUE enforces upsert logic)
CREATE TABLE IF NOT EXISTS prescriptions (
  id         SERIAL  PRIMARY KEY,
  patient_id INTEGER NOT NULL REFERENCES registrations(id) ON DELETE CASCADE,
  visit_date DATE    NOT NULL,
  meds_json  JSONB,
  created_by TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (patient_id, visit_date)
);

-- Drug reference library
CREATE TABLE IF NOT EXISTS drug_master (
  id         SERIAL PRIMARY KEY,
  brand_name TEXT,
  generic    TEXT,
  dose       TEXT,
  freq       TEXT,
  route      TEXT,
  duration   TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Audit trail for all data mutations
CREATE TABLE IF NOT EXISTS audit_logs (
  id          SERIAL PRIMARY KEY,
  user_id     TEXT,
  action_type TEXT        NOT NULL,
  table_name  TEXT,
  record_id   TEXT,
  timestamp   TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
