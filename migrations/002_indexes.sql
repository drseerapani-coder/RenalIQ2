-- Migration 002: Performance indexes for RenalIQ2
-- All use IF NOT EXISTS so they are safe to apply to an existing database.

-- registrations: patient search by name, phone, and UHID
CREATE INDEX IF NOT EXISTS idx_reg_last_name  ON registrations (lower(last_name));
CREATE INDEX IF NOT EXISTS idx_reg_first_name ON registrations (lower(first_name));
CREATE INDEX IF NOT EXISTS idx_reg_phone      ON registrations (phone);
CREATE INDEX IF NOT EXISTS idx_reg_hosp_num   ON registrations (hospital_number);

-- visitsmodule: load visit history per patient, and date-based daily review
CREATE INDEX IF NOT EXISTS idx_visits_patient ON visitsmodule (patient_id);
CREATE INDEX IF NOT EXISTS idx_visits_date    ON visitsmodule (visit_date);

-- labs: load lab flowsheet per patient, and date-based column queries
CREATE INDEX IF NOT EXISTS idx_labs_patient   ON labs (patient_id);
CREATE INDEX IF NOT EXISTS idx_labs_date      ON labs (test_date);

-- prescriptions: load prescriptions per patient
CREATE INDEX IF NOT EXISTS idx_rx_patient     ON prescriptions (patient_id);

-- past_medical_history: load PMHx per patient
CREATE INDEX IF NOT EXISTS idx_pmhx_patient   ON past_medical_history (registration_id);

-- audit_logs: activity lookups by user and time
CREATE INDEX IF NOT EXISTS idx_audit_user     ON audit_logs (user_id);
CREATE INDEX IF NOT EXISTS idx_audit_time     ON audit_logs (timestamp DESC);
