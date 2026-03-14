library(shiny)
library(bslib)
library(jsonlite)

# ============================================================
# SPECIALIST CHECKLIST DEFINITIONS
# Based on: KDIGO 2022, AST 2019, AUA 2019, EAU 2022,
#           ESH-ESC 2023, UNOS/OPTN guidelines, CDC ACIP
# ============================================================

CHECKLIST_DEFS <- list(

  # ----------------------------------------------------------
  # 1. RENAL TRANSPLANT RECIPIENT – PRE-TRANSPLANT WORKUP
  #    Based on: KDIGO 2020 Clinical Practice Guideline on the
  #    Evaluation and Management of Candidates for Kidney Transplantation
  #
  #    HOW TO ADD NEW FIELDS:
  #    - History item:  append  list(id="...", label="...", type="text/checkbox/select", ...)
  #                     to the relevant section's items = list(...)
  #    - Investigation: append  a test name string to the relevant group's tests = c(...)
  #    - New group:     append  list(group="...", tests=c(...))  to the investigations items list
  #    - New section:   append  a full list(heading=..., type=..., items=list(...))  to sections
  # ----------------------------------------------------------
  transplant_recipient = list(
    title = "Renal Transplant Recipient — Pre-transplant Workup",
    icon  = "kidney",
    sections = list(

      # ── Section 1: Renal Disease & Listing Criteria (KDIGO Ch. 1) ────────────
      list(heading = "Renal Disease & Listing Criteria", type = "history", items = list(
        list(id="rd_etiology",       label="Etiology of renal disease",                     type="text",     placeholder="e.g. IgA nephropathy, FSGS, diabetic nephropathy, ADPKD"),
        list(id="rd_ckd_stage",      label="Current CKD stage / eGFR at referral",          type="text",     placeholder="e.g. CKD G5 — eGFR 8 mL/min/1.73m²"),
        list(id="rd_dialysis_status",label="Dialysis status",                                type="select",
             choices=c("Not yet on dialysis (preemptive listing)","Haemodialysis","Peritoneal dialysis","Previously on dialysis — now transplanted"), selected="Not yet on dialysis (preemptive listing)"),
        list(id="rd_dialysis_dur",   label="Dialysis modality & duration (if applicable)",  type="text",     placeholder="HD 3× weekly since Jan 2022 / CAPD since 2021"),
        list(id="rd_access",         label="Current dialysis access",                        type="text",     placeholder="AV fistula left forearm / Permcath / PD catheter"),
        list(id="rd_prev_tx",        label="Previous transplants (number, dates, graft loss reason)", type="text", placeholder="e.g. 1st Tx 2015 — graft loss 2022 (chronic rejection)"),
        list(id="rd_residual_urine", label="Residual urine output (≥100 mL/day)",           type="checkbox", checked=FALSE),
        list(id="rd_anuria",         label="Anuric",                                         type="checkbox", checked=FALSE),
        list(id="rd_contraindication", label="Absolute contraindication identified",        type="checkbox", checked=FALSE),
        list(id="rd_contra_detail",  label="Contraindication details (if any)",             type="text",     placeholder="e.g. active malignancy, uncontrolled infection, non-compliance")
      )),

      # ── Section 2: Cardiovascular Assessment (KDIGO Ch. 3) ───────────────────
      # NOTE: CVD is the leading cause of death post-transplant
      list(heading = "Cardiovascular Assessment", type = "history", items = list(
        list(id="cv_htn",            label="Hypertension",                                  type="checkbox", checked=FALSE),
        list(id="cv_htn_detail",     label="BP control / medications",                     type="text",     placeholder="e.g. 145/90 — amlodipine 10mg + ramipril 5mg"),
        list(id="cv_dm",             label="Diabetes mellitus",                             type="checkbox", checked=FALSE),
        list(id="cv_dm_detail",      label="DM type / duration / current HbA1c",           type="text",     placeholder="Type 2, 12 years, HbA1c 7.8%; on insulin"),
        list(id="cv_cad",            label="Coronary artery disease",                       type="checkbox", checked=FALSE),
        list(id="cv_cad_detail",     label="CAD details (MI / stent / CABG / dates)",      type="text",     placeholder="STEMI 2019 — DES to LAD; CABG 2021"),
        list(id="cv_hf",             label="Heart failure",                                 type="checkbox", checked=FALSE),
        list(id="cv_hf_ef",          label="HF — ejection fraction / NYHA class",         type="text",     placeholder="EF 35% — NYHA II; on bisoprolol + furosemide"),
        list(id="cv_af",             label="Atrial fibrillation / other arrhythmia",        type="checkbox", checked=FALSE),
        list(id="cv_af_detail",      label="AF details / anticoagulation",                 type="text",     placeholder="Persistent AF — on apixaban; rate-controlled"),
        list(id="cv_stroke",         label="CVA / TIA",                                     type="checkbox", checked=FALSE),
        list(id="cv_stroke_detail",  label="CVA/TIA details",                              type="text",     placeholder="Ischaemic CVA right MCA 2020 — minimal deficit"),
        list(id="cv_pvd",            label="Peripheral vascular disease",                  type="checkbox", checked=FALSE),
        list(id="cv_pvd_detail",     label="PVD details (claudication, ABI, interventions)", type="text",  placeholder="Claudication 200m — ABI 0.7 bilaterally; angioplasty 2021"),
        list(id="cv_dyslip",         label="Dyslipidemia",                                 type="checkbox", checked=FALSE),
        list(id="cv_smoking",        label="Smoking status",                               type="select",
             choices=c("Never","Ex-smoker (specify quit date)","Current smoker"), selected="Never"),
        list(id="cv_smoking_detail", label="Pack-year history / cessation support offered",type="text",     placeholder="20 pack-years; quit 2020 / referred to cessation clinic")
      )),

      # ── Section 3: Immunological Profile (KDIGO Ch. 2) ────────────────────────
      list(heading = "Immunological Profile", type = "history", items = list(
        list(id="im_blood_group",    label="ABO blood group (confirmed)",                  type="text",     placeholder="A Rh positive"),
        list(id="im_sensitise_hx",   label="Sensitising events",                           type="text",     placeholder="e.g. Blood transfusions: 4 units 2019; Pregnancies: 2; Prior Tx: 1"),
        list(id="im_pra_peak",       label="Peak PRA / cPRA (%)",                          type="text",     placeholder="e.g. 85% — highly sensitised"),
        list(id="im_pra_current",    label="Current PRA / cPRA (%)",                       type="text",     placeholder="e.g. 30%"),
        list(id="im_dsa",            label="Known DSA identified",                         type="checkbox", checked=FALSE),
        list(id="im_dsa_detail",     label="DSA specificities (HLA antigen)",             type="text",     placeholder="e.g. Anti-DR11, Anti-DQ7 (MFI >3000)"),
        list(id="im_unacceptable",   label="Unacceptable antigens listed with registry",  type="checkbox", checked=FALSE),
        list(id="im_crossmatch_plan",label="Crossmatch plan",                              type="text",     placeholder="e.g. Prospective CDC + flow crossmatch required for all donors")
      )),

      # ── Section 4: Infectious Disease History ─────────────────────────────────
      list(heading = "Infectious Disease History", type = "history", items = list(
        list(id="inf_hiv",           label="HIV positive",                                 type="checkbox", checked=FALSE),
        list(id="inf_hiv_detail",    label="HIV — ART regimen, VL, CD4 count",            type="text",     placeholder="TDF/FTC/DTG; VL undetectable; CD4 550 cells/µL"),
        list(id="inf_hbv",           label="Hepatitis B status",                           type="text",     placeholder="HBsAg positive / HBcAb positive only / Immune (vaccinated) / Naïve"),
        list(id="inf_hbv_tx",        label="HBV — on antiviral / treatment plan",         type="text",     placeholder="Tenofovir for suppression — HBV DNA undetectable"),
        list(id="inf_hcv",           label="Hepatitis C status",                           type="text",     placeholder="HCV Ab positive — RNA undetectable (SVR12 post DAA therapy 2022)"),
        list(id="inf_tb",            label="Previous TB or LTBI diagnosed",               type="checkbox", checked=FALSE),
        list(id="inf_tb_detail",     label="TB/LTBI — treatment details",                 type="text",     placeholder="LTBI — completed 9H isoniazid 2021 / Active TB treated RIPE 2018"),
        list(id="inf_cmv",           label="CMV serostatus",                               type="text",     placeholder="IgG positive / negative"),
        list(id="inf_ebv",           label="EBV serostatus",                               type="text",     placeholder="VCA IgG positive / negative"),
        list(id="inf_vzv",           label="VZV immune status",                            type="text",     placeholder="IgG positive (immune) / negative (susceptible) / vaccinated"),
        list(id="inf_endemic",       label="Endemic exposure risk (region / travel)",      type="text",     placeholder="Strongyloides (SE Asia) / Chagas (Latin America) / Histo (Ohio Valley) / Cocci (SW USA)"),
        list(id="inf_active",        label="Current active infection",                     type="checkbox", checked=FALSE),
        list(id="inf_active_detail", label="Active infection details",                     type="text",     placeholder="Must be fully treated before transplant")
      )),

      # ── Section 5: Malignancy History & Screening (KDIGO Ch. 4 / Table 13) ────
      list(heading = "Malignancy History & Screening", type = "history", items = list(
        list(id="mal_prior",         label="Prior malignancy",                             type="checkbox", checked=FALSE),
        list(id="mal_detail",        label="Type, stage, treatment, remission date",       type="text",     placeholder="Colorectal Ca pT3N0M0 — resection Aug 2019, in remission; waiting period ≥5 yrs"),
        list(id="mal_waiting_period",label="KDIGO minimum waiting period met",             type="checkbox", checked=FALSE),
        list(id="mal_skin",          label="Skin cancer history",                          type="checkbox", checked=FALSE),
        list(id="mal_skin_detail",   label="Skin cancer type / treatment",                 type="text",     placeholder="BCC excised 2021 — no waiting period required / SCC — 2-year wait recommended"),
        list(id="mal_screen_done",   label="Age/sex-appropriate cancer screening complete",type="checkbox", checked=FALSE),
        list(id="mal_screen_detail", label="Screening dates (colonoscopy, Pap, mammogram, PSA, LDCT)", type="text", placeholder="Colonoscopy 2023 (normal); Pap 2024 (normal); Mammogram 2023 (BI-RADS 1)")
      )),

      # ── Section 6: Psychosocial Assessment (KDIGO Ch. 9) ─────────────────────
      list(heading = "Psychosocial Assessment", type = "history", items = list(
        list(id="ps_support",        label="Adequate social support & caregiver identified", type="checkbox", checked=FALSE),
        list(id="ps_support_detail", label="Caregiver / support details",                  type="text",     placeholder="Spouse primary carer; 2 adult children available"),
        list(id="ps_adherence",      label="Medication non-adherence concerns documented", type="checkbox", checked=FALSE),
        list(id="ps_adherence_detail",label="Adherence issues / interventions",            type="text",     placeholder="Missed dialysis sessions ×3 in 2023 — social worker engaged"),
        list(id="ps_psych",          label="Psychiatric / mental health history",          type="text",     placeholder="Depression — on sertraline, reviewed by psychiatry 2024"),
        list(id="ps_alcohol",        label="Alcohol use",                                  type="select",
             choices=c("None","Social / low risk","Hazardous (>14 units/week)","Dependent / AUD"), selected="None"),
        list(id="ps_substance",      label="Illicit substance use",                        type="text",     placeholder="None / cannabis (quit 2023) / opioids — specify last use"),
        list(id="ps_financial",      label="Financial / insurance planning reviewed",      type="checkbox", checked=FALSE),
        list(id="ps_sw_referral",    label="Social work referral completed",               type="checkbox", checked=FALSE),
        list(id="ps_education",      label="Pre-transplant education program completed",   type="checkbox", checked=FALSE)
      )),

      # ── Section 7: Urological & Surgical History ─────────────────────────────
      list(heading = "Urological & Surgical History", type = "history", items = list(
        list(id="ur_voiding",        label="Voiding dysfunction / LUTS",                  type="checkbox", checked=FALSE),
        list(id="ur_voiding_detail", label="Voiding symptoms / urodynamics performed",    type="text",     placeholder="Neurogenic bladder — urodynamics 2023: low compliance"),
        list(id="ur_utis",           label="Recurrent urinary tract infections",           type="checkbox", checked=FALSE),
        list(id="ur_stones",         label="Nephrolithiasis history",                      type="checkbox", checked=FALSE),
        list(id="ur_stones_detail",  label="Stone type / metabolic workup",               type="text",     placeholder="Calcium oxalate stones ×3 — 24hr urine: hyperoxaluria"),
        list(id="ur_nephrectomy",    label="Native nephrectomy planned / completed",       type="checkbox", checked=FALSE),
        list(id="ur_nephrectomy_reason", label="Indication for native nephrectomy",       type="text",     placeholder="Massive ADPKD / recurrent infections / refractory HTN"),
        list(id="ur_abdo_surgery",   label="Previous abdominal / pelvic surgery",          type="text",     placeholder="Appendicectomy 2010 / Hysterectomy 2018 / Ileal conduit"),
        list(id="ur_bmi",            label="BMI (kg/m²)",                                  type="text",     placeholder="e.g. 32.4 — obesity counselling offered"),
        list(id="ur_bmi_plan",       label="Weight management plan if BMI >30",           type="text",     placeholder="Referred bariatric surgery / dietitian / target BMI <35 before listing"),
        list(id="ur_dental",         label="Dental clearance obtained",                    type="checkbox", checked=FALSE),
        list(id="ur_ophthal",        label="Ophthalmology review (diabetic / high-risk)",  type="checkbox", checked=FALSE)
      )),

      # ── Section 8: Investigations — Haematology & Biochemistry ───────────────
      list(heading = "Investigations: Haematology & Biochemistry", type = "investigations", items = list(
        list(group = "Full Blood Count & Coagulation", tests = c(
          "FBC with differential (Hb, WCC, platelets)",
          "Coagulation screen (PT / APTT / INR)",
          "Blood film (if indicated)"
        )),
        list(group = "Renal Function & Electrolytes", tests = c(
          "Serum creatinine, urea, eGFR (CKD-EPI 2021)",
          "Electrolytes: Na, K, Cl, HCO3",
          "Serum Mg, Phosphate",
          "Uric acid"
        )),
        list(group = "Metabolic & Endocrine", tests = c(
          "Fasting glucose",
          "HbA1c",
          "Fasting lipid profile (TC, LDL, HDL, TG)",
          "Liver function tests (ALT, AST, ALP, GGT, Albumin, Bilirubin)",
          "Iron studies (serum iron, TIBC, transferrin saturation, ferritin)",
          "Thyroid function (TSH ± free T4)"
        )),
        list(group = "Bone & Mineral Metabolism", tests = c(
          "PTH (intact)",
          "Calcium (corrected)",
          "Phosphate",
          "25-OH Vitamin D",
          "Alkaline phosphatase"
        )),
        list(group = "Urine", tests = c(
          "Urinalysis with microscopy & culture",
          "Urine protein-to-creatinine ratio (UPCR)",
          "24-hour urine creatinine clearance (if eGFR uncertainty)"
        ))
      )),

      # ── Section 9: Investigations — Blood Group & Immunology (KDIGO Ch. 2) ───
      list(heading = "Investigations: Blood Group & Immunology", type = "investigations", items = list(
        list(group = "Blood Group & Compatibility", tests = c(
          "ABO & Rh blood group (repeat confirmation)",
          "Antibody screen"
        )),
        list(group = "HLA Typing (KDIGO 2.1–2.2)", tests = c(
          "HLA Class I typing: HLA-A",
          "HLA Class I typing: HLA-B",
          "HLA Class I typing: HLA-C",
          "HLA Class II typing: HLA-DR",
          "HLA Class II typing: HLA-DQ",
          "HLA Class II typing: HLA-DP"
        )),
        list(group = "Sensitisation & DSA (KDIGO 2.3–2.5)", tests = c(
          "Panel Reactive Antibody (PRA) — current",
          "PRA — peak historical value",
          "Solid phase assay (Luminex single antigen beads) — Class I",
          "Solid phase assay (Luminex single antigen beads) — Class II",
          "Donor-Specific Antibody (DSA) identification",
          "Complement-binding DSA (C1q / C3d assay) — if DSA positive"
        )),
        list(group = "Crossmatch (when donor identified)", tests = c(
          "Complement-dependent cytotoxicity crossmatch (CDC-XM)",
          "AHG-enhanced CDC crossmatch (AHG-CDC)",
          "Flow cytometry crossmatch (FCXM) — T-cell",
          "Flow cytometry crossmatch (FCXM) — B-cell",
          "Virtual crossmatch (based on unacceptable antigens & donor HLA)"
        ))
      )),

      # ── Section 10: Investigations — Infectious Disease (KDIGO Table 11) ─────
      list(heading = "Investigations: Infectious Disease Screening", type = "investigations", items = list(
        list(group = "Viral Serology", tests = c(
          "HIV Ab/Ag (4th generation combination assay)",
          "Hepatitis B surface Ag (HBsAg)",
          "Hepatitis B core Ab (anti-HBc IgG)",
          "Hepatitis B surface Ab (anti-HBs) — titre (IU/L)",
          "Hepatitis C Ab",
          "Hepatitis C RNA (if Ab positive or recent high-risk exposure)",
          "CMV IgG",
          "EBV VCA IgG",
          "EBV EBNA IgG",
          "Varicella-Zoster IgG (VZV)",
          "HSV IgG (Type 1 & 2)",
          "Toxoplasma IgG",
          "HTLV I/II Ab (if from endemic region)"
        )),
        list(group = "Bacterial & Treponemal", tests = c(
          "VDRL / RPR (syphilis screening)",
          "Treponemal confirmatory test (TPPA / EIA) — if RPR positive"
        )),
        list(group = "TB Screening (KDIGO Table 11)", tests = c(
          "IGRA: QuantiFERON-TB Gold Plus OR T-SPOT.TB (preferred)",
          "Tuberculin skin test (TST / Mantoux) — if IGRA unavailable",
          "Chest X-ray — PA view (all candidates)"
        )),
        list(group = "Endemic Infections (if applicable)", tests = c(
          "Strongyloides serology (if from endemic region: SE Asia, Africa, Latin America)",
          "Chagas serology (Trypanosoma cruzi) — if from endemic region: Latin America",
          "Coccidioidomycosis serology — if from endemic region (SW USA, Mexico)",
          "WNV NAT (deceased donor — seasonal)"
        ))
      )),

      # ── Section 11: Investigations — Cardiovascular (KDIGO Ch. 3) ────────────
      list(heading = "Investigations: Cardiovascular Assessment", type = "investigations", items = list(
        list(group = "Baseline Cardiac", tests = c(
          "12-lead ECG",
          "Echocardiogram — LV systolic function (EF)",
          "Echocardiogram — wall motion abnormalities",
          "Echocardiogram — valvular disease",
          "Echocardiogram — pulmonary artery pressure (PASP)",
          "Echocardiogram — LV hypertrophy / diastolic function"
        )),
        list(group = "Ischaemia Testing (if CAD risk or symptoms)", tests = c(
          "Dobutamine stress echocardiography (DSE)",
          "Nuclear perfusion scan (MPS) — if DSE contraindicated or equivocal",
          "Exercise ECG stress test (if functional capacity adequate)",
          "CT coronary angiography / calcium score (CAC)",
          "Invasive coronary angiogram — if high-risk or positive stress test"
        )),
        list(group = "Peripheral & Cerebrovascular", tests = c(
          "Ankle-Brachial Index (ABI) — bilateral",
          "Carotid Doppler ultrasound (cerebrovascular risk)",
          "24-hour ambulatory BP monitoring (ABPM)"
        )),
        list(group = "Vascular for Surgical Planning", tests = c(
          "Iliac artery Doppler ultrasound",
          "CT angiography — iliac vessels (if calcification, prior surgery, or asymmetric femoral pulses)"
        ))
      )),

      # ── Section 12: Investigations — Pulmonary & Other Imaging ───────────────
      list(heading = "Investigations: Pulmonary & Other Imaging", type = "investigations", items = list(
        list(group = "Pulmonary", tests = c(
          "Chest X-ray — PA (and lateral if indicated)",
          "Spirometry / PFTs — if symptomatic or smoking history",
          "Polysomnography (sleep study) — if OSA symptoms",
          "CT chest — if abnormal CXR or smoking history (low-dose lung screen ≥50 yrs)"
        )),
        list(group = "Abdominal & Urological Imaging", tests = c(
          "Renal ultrasound — bilateral native kidneys (size, corticomedullary differentiation, cysts, obstruction)",
          "Voiding cystourethrogram (VCUG) — if voiding dysfunction / recurrent UTI",
          "Urodynamic studies — if VCUG abnormal or neurogenic bladder suspected",
          "CT abdomen/pelvis — if prior surgery, stones, or anatomical concern"
        )),
        list(group = "Bone Density", tests = c(
          "DEXA scan (bone mineral density) — if CKD-MBD concern, steroid use, post-menopausal, or age >50"
        ))
      )),

      # ── Section 13: Investigations — Malignancy Screening (KDIGO Table 13) ───
      list(heading = "Investigations: Malignancy Screening", type = "investigations", items = list(
        list(group = "Gynaecological (women)", tests = c(
          "Pap smear / liquid-based cytology (LBC)",
          "HPV co-test (if ≥30 years)",
          "Mammogram (women ≥40)"
        )),
        list(group = "Colorectal", tests = c(
          "Colonoscopy (age ≥45, or earlier if family history / symptoms)",
          "Faecal occult blood test (FOBT) / FIT — if colonoscopy not due"
        )),
        list(group = "Prostate (men)", tests = c(
          "PSA (men ≥50; shared decision-making; earlier if family history or African ancestry)"
        )),
        list(group = "Lung", tests = c(
          "Low-dose CT chest (LDCT) — if age 50–80 yrs with ≥20 pack-year smoking history"
        )),
        list(group = "Skin", tests = c(
          "Dermatology review — skin examination for pre-malignant lesions (esp. if sun-exposed, Fitzpatrick I–II)"
        )),
        list(group = "Other Cancer Screening (as indicated)", tests = c(
          "Liver ultrasound + AFP — if HBV/HCV-related cirrhosis (HCC surveillance)",
          "Upper GI endoscopy — if H. pylori history, Barrett's, or dyspepsia",
          "Thyroid ultrasound — if palpable nodule or neck radiation history"
        ))
      )),

      # ── Section 14: Investigations — Vaccination Status (KDIGO Table 12) ─────
      list(heading = "Investigations: Vaccination Status", type = "investigations", items = list(
        list(group = "Annual / Seasonal (safe pre- and post-transplant)", tests = c(
          "Influenza (inactivated) — annual dose confirmed",
          "COVID-19 (mRNA/recombinant) — primary series + booster per current guidance"
        )),
        list(group = "Hepatitis", tests = c(
          "Anti-HBs titre (target ≥10 IU/L; ≥100 IU/L preferred pre-Tx)",
          "HBV vaccine: primary series or booster (double-dose 40 µg in CKD/dialysis)",
          "Hepatitis A — 2 doses (if anti-HAV IgG negative)"
        )),
        list(group = "Pneumococcal (complete before immunosuppression)", tests = c(
          "PCV20 (single dose, preferred) — OR",
          "PCV15 followed by PPSV23 (at least 8 weeks apart)",
          "PPSV23 5-yearly booster (if previously received PPSV23 only)"
        )),
        list(group = "LIVE Vaccines (PRE-TRANSPLANT ONLY — ≥4 weeks before IS)", tests = c(
          "MMR — 2 doses (confirm measles, mumps, rubella immunity; CONTRAINDICATED post-Tx on IS)",
          "Varicella (Varivax) — 2 doses if VZV IgG negative (CONTRAINDICATED post-Tx on IS)"
        )),
        list(group = "Zoster", tests = c(
          "RZV (Shingrix) — recombinant, adjuvanted: Dose 1",
          "RZV (Shingrix) — Dose 2 (2 months after dose 1); safe pre- and post-transplant"
        )),
        list(group = "Tdap / Td", tests = c(
          "Tdap (tetanus-diphtheria-acellular pertussis) — 1 dose if not received in adulthood",
          "Td booster — every 10 years"
        )),
        list(group = "HPV", tests = c(
          "HPV vaccine (Gardasil 9) — 3 doses (shared decision; up to age 45; or all transplant candidates per AST)"
        )),
        list(group = "Meningococcal", tests = c(
          "MenACWY (MCV4) — if asplenia, complement deficiency, or eculizumab use",
          "MenB — if asplenia or age <25"
        ))
      ))

    )
  ),

  # ----------------------------------------------------------
  # 2. RENAL TRANSPLANT DONOR – PRE-TRANSPLANT  (KDIGO 2017)
  # ----------------------------------------------------------
  # AMENDMENT GUIDE:
  #   History:  append list(id="dn_xxx", label="...", type="checkbox"|"text"|"select", ...) to any section items
  #   Inv:      append test name string to a group's tests vector, or add list(group="...", tests=c(...))
  # ----------------------------------------------------------
  transplant_donor = list(
    title = "Renal Transplant Donor — Pre-transplant (KDIGO 2017)",
    icon  = "person-circle-check",
    sections = list(

      # ── 1. Donor Profile & Consent ────────────────────────
      list(heading = "Donor Profile & Consent", type = "history", items = list(
        list(id="dn_type",          label="Donor type",                                  type="select",
             choices=c("Living related","Living unrelated (directed)","Living non-directed","Kidney paired donation (KPD)","Deceased (DBD)","Deceased (DCD)"), selected="Living related"),
        list(id="dn_relationship",  label="Relationship to recipient",                   type="text",     placeholder="e.g. spouse, sibling, friend"),
        list(id="dn_age",           label="Age / sex",                                   type="text",     placeholder="35 / Male"),
        list(id="dn_bmi",           label="BMI (kg/m²) — weight / height",               type="text",     placeholder="e.g. 24.5 (75 kg / 175 cm)"),
        list(id="dn_waist",         label="Waist circumference (if BMI >30)",             type="text",     placeholder="cm — M >102 cm / F >88 cm = metabolic risk"),
        list(id="dn_consent",       label="Informed consent obtained in absence of recipient / family", type="checkbox", checked=FALSE),
        list(id="dn_capacity",      label="Capacity to understand risks, benefits and consequences confirmed", type="checkbox", checked=FALSE),
        list(id="dn_voluntarism",   label="Voluntarism confirmed — no undue pressure, coercion or financial inducement", type="checkbox", checked=FALSE),
        list(id="dn_advocate",      label="Independent living donor advocate (ILDA) involvement documented", type="checkbox", checked=FALSE),
        list(id="dn_motivation",    label="Stated motivation for donation",               type="text",     placeholder="Altruistic / relationship / other")
      )),

      # ── 2. Cardiovascular & Metabolic History ─────────────
      list(heading = "Cardiovascular & Metabolic History", type = "history", items = list(
        list(id="dn_htn",           label="Hypertension",                                type="checkbox", checked=FALSE),
        list(id="dn_htn_agents",    label="No. antihypertensive agents (if HTN)",         type="text",     placeholder="0 / 1 / 2  (>2 agents or TOD = decline)"),
        list(id="dn_htn_tod",       label="Hypertensive target organ damage (LVH, AER >30 mg/d, GFR <60, retinopathy)", type="checkbox", checked=FALSE),
        list(id="dn_dm_t1",         label="Type 1 diabetes mellitus (contraindication to donation)", type="checkbox", checked=FALSE),
        list(id="dn_dm_t2",         label="Type 2 diabetes mellitus (individualize — long-term risk)", type="checkbox", checked=FALSE),
        list(id="dn_prediabetes",   label="Prediabetes / impaired fasting glucose / impaired glucose tolerance", type="checkbox", checked=FALSE),
        list(id="dn_gdm",           label="History of gestational diabetes",              type="checkbox", checked=FALSE),
        list(id="dn_dm_family",     label="Family history of diabetes in 1st-degree relative", type="checkbox", checked=FALSE),
        list(id="dn_lipids",        label="Dyslipidaemia",                               type="checkbox", checked=FALSE),
        list(id="dn_smoking",       label="Smoking status",                              type="select",
             choices=c("Never","Ex-smoker (>4 wk abstinent)","Current smoker"), selected="Never"),
        list(id="dn_alcohol",       label="Alcohol use",                                 type="select",
             choices=c("None","Moderate","Heavy (>14 units/week)"), selected="None"),
        list(id="dn_cvd_hx",        label="Prior cardiovascular event (MI, stroke, PVD, heart failure)", type="checkbox", checked=FALSE),
        list(id="dn_cvd_family",    label="Family history of premature CVD (<55 M / <65 F)", type="checkbox", checked=FALSE),
        list(id="dn_bariatric",     label="History of bariatric surgery (assess nephrolithiasis risk)", type="checkbox", checked=FALSE)
      )),

      # ── 3. Kidney & Urological History ────────────────────
      list(heading = "Kidney & Urological History", type = "history", items = list(
        list(id="dn_ckd_personal",  label="Personal history of CKD / kidney disease",    type="text",     placeholder="None / specify condition"),
        list(id="dn_ckd_family",    label="Family history of CKD (type and relationship)", type="text",    placeholder="None / ADPKD / FSGS / IgA / Alport / other"),
        list(id="dn_adpkd",         label="Family history of ADPKD in 1st-degree relative", type="checkbox", checked=FALSE),
        list(id="dn_apol1",         label="Sub-Saharan African ancestry (APOL1 G1/G2 genotyping indicated)", type="checkbox", checked=FALSE),
        list(id="dn_stones",        label="History of nephrolithiasis / kidney stones",  type="checkbox", checked=FALSE),
        list(id="dn_stones_type",   label="Stone type / recurrence details (if yes)",    type="text",     placeholder="Calcium oxalate / uric acid / struvite / unknown"),
        list(id="dn_haematuria",    label="History of microscopic haematuria (persistent)", type="checkbox", checked=FALSE),
        list(id="dn_proteinuria",   label="History of proteinuria",                      type="checkbox", checked=FALSE),
        list(id="dn_uti_recurrent", label="Recurrent UTIs / pyelonephritis",             type="checkbox", checked=FALSE),
        list(id="dn_gout",          label="Hyperuricaemia / gout",                       type="checkbox", checked=FALSE),
        list(id="dn_iga",           label="Known IgA nephropathy (contraindication — do not donate)", type="checkbox", checked=FALSE),
        list(id="dn_renal_artery",  label="Known renal artery stenosis / fibromuscular dysplasia", type="checkbox", checked=FALSE)
      )),

      # ── 4. Infection & Exposure History ───────────────────
      list(heading = "Infection & Exposure History", type = "history", items = list(
        list(id="dn_tb_risk",       label="TB risk factors (endemic country, healthcare worker, incarceration, homeless)", type="checkbox", checked=FALSE),
        list(id="dn_tb_detail",     label="TB risk — country of origin / occupation / known contacts", type="text", placeholder="Detail here"),
        list(id="dn_geo_exposure",  label="Geographic / endemic exposures (travel/residence — Strongyloides, Chagas, WNV, Histo, Cocci)", type="text", placeholder="Countries / regions / duration"),
        list(id="dn_phs_risk",      label="US PHS increased-risk behaviour for HIV/HBV/HCV (window-period risk — see KDIGO Table 16)", type="checkbox", checked=FALSE),
        list(id="dn_prior_malignancy", label="Prior malignancy (type, date, treatment, remission status)", type="text", placeholder="None / specify"),
        list(id="dn_active_malignancy", label="Active malignancy identified (general contraindication)", type="checkbox", checked=FALSE)
      )),

      # ── 5. Current Medications ────────────────────────────
      list(heading = "Current Medications", type = "history", items = list(
        list(id="dn_meds",          label="Current medications (list all)",              type="text",     placeholder="Name, dose, indication"),
        list(id="dn_nsaids",        label="Regular NSAIDs / nephrotoxins",               type="checkbox", checked=FALSE),
        list(id="dn_anticoag",      label="Anticoagulants / antiplatelet agents",        type="checkbox", checked=FALSE),
        list(id="dn_ocp_hrt",       label="OCP / HRT (VTE risk — plan perioperative management)", type="checkbox", checked=FALSE),
        list(id="dn_allergies",     label="Drug allergies / contrast allergy",           type="text",     placeholder="NKDA / specify allergy and reaction")
      )),

      # ── 6. Psychosocial Evaluation ────────────────────────
      list(heading = "Psychosocial Evaluation", type = "history", items = list(
        list(id="dn_psych_eval",    label="In-person psychosocial evaluation by social worker / psychologist completed", type="checkbox", checked=FALSE),
        list(id="dn_psych_mh",      label="Mental health history (depression, anxiety, psychiatric illness)", type="text", placeholder="None / specify"),
        list(id="dn_substance",     label="Substance use history (tobacco, alcohol, illicit drugs)", type="text", placeholder="None / specify"),
        list(id="dn_financial",     label="Financially independent — no material gain from donation", type="checkbox", checked=FALSE),
        list(id="dn_support",       label="Adequate social support for perioperative recovery", type="checkbox", checked=FALSE),
        list(id="dn_coping",        label="Able to cope with adverse outcomes (graft failure, surgical complications)", type="checkbox", checked=FALSE),
        list(id="dn_postdon_plan",  label="Postdonation long-term follow-up plan established", type="checkbox", checked=FALSE)
      )),

      # ── 7. Female Donor — Additional ──────────────────────
      list(heading = "Female Donor — Additional", type = "history", items = list(
        list(id="dn_f_pregnant",    label="Not currently pregnant — beta-hCG negative before radiologic tests / nuclear medicine GFR", type="checkbox", checked=FALSE),
        list(id="dn_f_plans",       label="Future childbearing plans discussed and counselled (postdonation obstetric risks)", type="checkbox", checked=FALSE),
        list(id="dn_f_htn_preg",    label="History of hypertensive disorder of pregnancy (pre-eclampsia / eclampsia)", type="checkbox", checked=FALSE),
        list(id="dn_f_gdm_detail",  label="Prior gestational diabetes — details",        type="text",     placeholder="Year / managed with diet / insulin / resolved")
      )),

      # ── INVESTIGATIONS ────────────────────────────────────
      # ── Inv 1: GFR & Albuminuria ──────────────────────────
      list(heading = "Kidney Function & Albuminuria", type = "investigations", items = list(
        list(group = "GFR Assessment (KDIGO 2012 / 2017)", tests = c(
          "Serum creatinine — IDMS-traceable assay",
          "eGFR CKD-EPI creatinine (eGFRcr) — initial screen  [≥90 = acceptable; 60-89 = individualize; <60 = decline]",
          "Serum cystatin C + eGFR CKD-EPI creatinine-cystatin (eGFRcr-cys) — if eGFRcr inaccurate (extremes of muscle mass, race)",
          "mGFR confirmation (iohexol / iothalamate / 51Cr-EDTA / 99mTc-DTPA plasma clearance) — if decision-critical",
          "24-h urine creatinine clearance (mCrCl) — acceptable if mGFR unavailable (overestimates mGFR ~15%)"
        )),
        list(group = "Albuminuria (2-stage testing)", tests = c(
          "Spot urine ACR — early morning, random untimed  [SCREEN: <3 mg/mmol (<30 mg/g) = acceptable]",
          "24-h urine albumin excretion rate (AER) — timed specimen  [CONFIRM: <30 mg/day = acceptable; 30-100 = individualize; >100 = decline]",
          "Repeat ACR if timed AER unavailable"
        )),
        list(group = "Urine Assessment", tests = c(
          "Urinalysis with microscopy — haematuria (RBCs, casts), pyuria",
          "Urine culture — at evaluation AND repeat within 2 weeks of planned donation date",
          "Spot urine protein-to-creatinine ratio (PCR / UPCR)",
          "24-h urine stone panel: Ca, oxalate, citrate, phosphate, uric acid, Na, creatinine — if stones or haematuria"
        ))
      )),

      # ── Inv 2: Haematology & Biochemistry ─────────────────
      list(heading = "Haematology & Biochemistry", type = "investigations", items = list(
        list(group = "Haematology", tests = c(
          "FBC with differential (Hb, WCC, platelets)",
          "Coagulation screen: PT, APTT, fibrinogen — if personal/family bleeding history"
        )),
        list(group = "Biochemistry", tests = c(
          "Fasting glucose",
          "HbA1c",
          "2-hour OGTT (75 g) — if elevated fasting glucose / GDM history / family Hx DM in 1st-degree relative",
          "Fasting lipid profile: total cholesterol, LDL-C, HDL-C, triglycerides",
          "Liver function tests: ALT, AST, ALP, GGT, bilirubin, albumin",
          "Serum uric acid",
          "Calcium, phosphate",
          "Electrolytes: Na, K, bicarbonate",
          "PSA — men ≥40 (shared decision per local guidelines)"
        ))
      )),

      # ── Inv 3: Blood Pressure ─────────────────────────────
      list(heading = "Blood Pressure Assessment", type = "investigations", items = list(
        list(group = "BP Measurement (KDIGO Ch. 10)", tests = c(
          "Office BP ×2 occasions — trained staff, calibrated equipment, proper technique  [<140/90 mmHg acceptable; uncontrolled or TOD = decline]",
          "ABPM (24-h ambulatory BP monitoring) — if BP borderline, white-coat effect suspected, or on antihypertensives",
          "Target organ damage screen (ECG, urine ACR, fundoscopy, echo if LVH suspected) — if HTN present"
        ))
      )),

      # ── Inv 4: Blood Group, HLA & Crossmatch ──────────────
      list(heading = "Blood Group, HLA & Crossmatch", type = "investigations", items = list(
        list(group = "ABO / HLA (KDIGO Ch. 3)", tests = c(
          "ABO & Rh blood typing — TWICE before donation (to reduce ABO-incompatible transplant risk)",
          "Blood group A subtyping (A1 vs A2) — if recipient has anti-A antibodies",
          "HLA typing: MHC Class I (A, B, C) and Class II (DP, DQ, DR)",
          "Donor-specific HLA antibodies (DSA) screened in intended recipient",
          "Flow cytometry crossmatch + Luminex DSA — if DSA present or high recipient PRA"
        ))
      )),

      # ── Inv 5: Serology — Mandatory (All Donors) ──────────
      list(heading = "Serology — Mandatory (All Donors)", type = "investigations", items = list(
        list(group = "HIV / HBV / HCV — within 28 days of donation", tests = c(
          "HIV: Ag/Ab 4th-generation combination assay + HIV NAT",
          "HBV: HBsAg + anti-HBcAb (IgG) + anti-HBsAb (immunity)",
          "HBV NAT — if anti-HBcAb positive (stratify transmission risk)",
          "HCV: anti-HCV Ab + HCV RNA NAT"
        )),
        list(group = "Other Mandatory Serology", tests = c(
          "CMV IgG — stratifies recipient prophylaxis (D+/R- = highest CMV disease risk)",
          "EBV IgG/IgM — D+/R- = PTLD risk especially in paediatric recipients",
          "Syphilis: RPR or VDRL (non-treponemal screen) + confirmatory treponemal antibody assay",
          "HTLV I/II — if from endemic region (Caribbean, Japan, West Africa, South America, SE Asia)"
        ))
      )),

      # ── Inv 6: Serology — Targeted (Risk-Based) ───────────
      list(heading = "Serology — Targeted by Risk (KDIGO Ch. 12)", type = "investigations", items = list(
        list(group = "Tuberculosis Screening", tests = c(
          "CXR — all candidates (active / prior TB, pulmonary pathology)",
          "TST (tuberculin skin test) OR IGRA (QuantiFERON / T-SPOT) — if TB risk factors present",
          "AFB smear + culture / GeneXpert — if active TB clinically suspected"
        )),
        list(group = "Geographic / Endemic Infections (if indicated)", tests = c(
          "West Nile Virus NAT — within 7-14 days before donation (seasonal: summer/fall; geographic: Midwest/South US)",
          "Strongyloides IgG Ab (ELISA) — if born / lived in tropical/subtropical countries or Appalachian US",
          "Trypanosoma cruzi (Chagas) Ab — if born / lived in Mexico, Central or South America",
          "Histoplasma urine antigen + complement fixation Ab — if from Ohio / Mississippi River valley region",
          "Coccidioides Ab (complement fixation / immunodiffusion) — if from US Southwest desert areas"
        ))
      )),

      # ── Inv 7: Cardiac & Respiratory ──────────────────────
      list(heading = "Cardiac & Respiratory", type = "investigations", items = list(
        list(group = "Cardiac", tests = c(
          "12-lead ECG — all donors",
          "Echocardiogram — if age >50, hypertension, diabetes, CVD symptoms or risk factors",
          "Non-invasive stress test (ETT / stress echo / nuclear) — only if symptomatic CAD or high cardiovascular risk (NOT routine)"
        )),
        list(group = "Respiratory & Perioperative", tests = c(
          "Spirometry / PFTs — if current/ex-smoker, COPD, or respiratory symptoms",
          "Perioperative VTE risk (Rogers / Caprini score) — plan DVT prophylaxis accordingly",
          "Anaesthesia pre-assessment — airway, OSA, allergy, bleeding history, drug interactions"
        ))
      )),

      # ── Inv 8: Kidney Imaging ─────────────────────────────
      list(heading = "Kidney Imaging", type = "investigations", items = list(
        list(group = "Anatomical & Vascular (KDIGO Ch. 17)", tests = c(
          "CT renal angiography with IV contrast — gold standard (vascular anatomy, number of arteries, cysts, masses, stones)",
          "Consent for IV iodinated contrast — confirm eGFR and absence of allergy; hold nephrotoxins 24-48 h",
          "Renal ultrasound — if CT unavailable; assess cortical thickness, cysts, echogenicity",
          "MAG3 / 99mTc-DTPA split renal function scan — if kidney size asymmetry >10% or eGFR 60-89 (select more affected kidney for donation)",
          "DEXA scan — if elderly donor, prolonged corticosteroid use, or metabolic bone disease"
        )),
        list(group = "Imaging Findings to Document", tests = c(
          "Bosniak cyst classification: I = leave in donor; IIF = follow; II/IIF = individualize; III or IV = decline / surgical review",
          "Renal artery anatomy: single/multiple, early branching, atherosclerosis, fibromuscular dysplasia — bilateral FMD = contraindication",
          "Kidney length and symmetry — asymmetry >10% in size or function = split scan indicated",
          "No renal masses / incidental findings requiring management prior to donation"
        ))
      )),

      # ── Inv 9: Cancer Screening ────────────────────────────
      list(heading = "Cancer Screening (KDIGO Ch. 13)", type = "investigations", items = list(
        list(group = "Age / Sex Appropriate (current per national guidelines)", tests = c(
          "Cervical smear / HPV testing — women (per national age-based guidelines)",
          "Mammogram — women ≥40 (or per national guidelines)",
          "Colorectal: colonoscopy or FIT / FOBT — age ≥45 (or per guidelines)",
          "PSA — men ≥40 (shared decision per local guidelines)",
          "Low-dose CT chest — smokers (≥20 pack-years, age 50-80) or per guidelines",
          "Skin examination — personal/family history of melanoma or high UV exposure",
          "All age-appropriate screening documented as current at time of evaluation"
        ))
      )),

      # ── Inv 10: Genetic Evaluation (if indicated) ──────────
      list(heading = "Genetic Evaluation (if indicated)", type = "investigations", items = list(
        list(group = "Targeted Genetic Testing (KDIGO Ch. 14)", tests = c(
          "ADPKD: Pei 2015 age-specific ultrasound criteria — if family Hx ADPKD; PKD1/PKD2 gene sequencing if imaging inconclusive",
          "APOL1 genotyping (G1 / G2 risk alleles) — if sub-Saharan African ancestry; 2 risk alleles = significantly elevated lifetime ESKD risk",
          "Alport syndrome panel (COL4A3/A4/A5) — if family history haematuria, CKD, sensorineural hearing loss, ocular lesions",
          "Broader hereditary nephropathy panel — per clinical suspicion and pedigree"
        ))
      ))

    )
  ),

  # ----------------------------------------------------------
  # 3. POST RENAL TRANSPLANT FOLLOW-UP
  # ----------------------------------------------------------
  post_transplant = list(
    title = "Post Renal Transplant — Follow-up",
    icon  = "notes-medical",
    sections = list(

      list(heading = "Transplant & Immunosuppression Status", type = "history", items = list(
        list(id="pt_date",          label="Transplant date",                         type="text",     placeholder="DD/MM/YYYY"),
        list(id="pt_donor_type",    label="Donor type",                              type="select",
             choices=c("Living related","Living unrelated","Deceased DBD","Deceased DCD"), selected="Living related"),
        list(id="pt_is_regimen",    label="Current immunosuppression regimen",       type="text",     placeholder="Tacrolimus / MMF / Prednisolone"),
        list(id="pt_tacro_level",   label="Latest tacrolimus trough level",          type="text",     placeholder="ng/mL — target range per protocol"),
        list(id="pt_adherence",     label="Medication adherence concerns",           type="checkbox", checked=FALSE),
        list(id="pt_is_side_fx",    label="Immunosuppression side effects",          type="text",     placeholder="Neurotoxicity, GI, tremor, alopecia")
      )),

      list(heading = "Rejection & Graft Function", type = "history", items = list(
        list(id="gr_episodes",      label="Acute rejection episodes (number/dates)", type="text",     placeholder="None / T-cell mediated, treated with pulse steroids 2023"),
        list(id="gr_biopsy",        label="Protocol biopsies done",                  type="text",     placeholder="1-year biopsy: IF/TA Grade I"),
        list(id="gr_dsas",          label="DSA monitoring results",                  type="text",     placeholder="DNDSAs: none detected"),
        list(id="gr_cni_tox",       label="CNI toxicity suspected/confirmed",        type="checkbox", checked=FALSE),
        list(id="gr_trend",         label="eGFR trend (improving/stable/declining)", type="select",
             choices=c("Stable","Improving","Slowly declining","Rapidly declining"), selected="Stable")
      )),

      list(heading = "Infectious Complications", type = "history", items = list(
        list(id="ic_utis",          label="Recurrent UTIs",                          type="checkbox", checked=FALSE),
        list(id="ic_bk_pcr",        label="BK viraemia — peak level",                type="text",     placeholder="BKV PCR peak log copies/mL"),
        list(id="ic_bkn",           label="BK nephropathy confirmed on biopsy",      type="checkbox", checked=FALSE),
        list(id="ic_cmv",           label="CMV disease — episode(s)",                type="text",     placeholder="Syndrome / tissue invasive — treated"),
        list(id="ic_eos",           label="Episodes of opportunistic infections",    type="text",     placeholder="PCP, fungal, etc."),
        list(id="ic_covid",         label="COVID-19 episodes post-transplant",       type="text",     placeholder="Date, severity, treatment")
      )),

      list(heading = "Cardiovascular & Metabolic", type = "history", items = list(
        list(id="cm_htn",           label="Hypertension — current BP control",       type="text",     placeholder="130/80 — amlodipine 5mg"),
        list(id="cm_pts_dm",        label="Post-transplant diabetes (PTDM)",         type="checkbox", checked=FALSE),
        list(id="cm_pts_dm_mgmt",   label="PTDM management",                         type="text",     placeholder="Diet / metformin / insulin"),
        list(id="cm_dyslip",        label="Dyslipidemia — on statin",                type="checkbox", checked=FALSE),
        list(id="cm_anemia",        label="Post-transplant anemia (Hb level)",       type="text",     placeholder="Hb g/dL"),
        list(id="cm_bone",          label="Bone disease / fracture history",         type="text",     placeholder="DEXA T-score, fractures")
      )),

      list(heading = "Malignancy Surveillance", type = "history", items = list(
        list(id="ms_skin",          label="Skin cancers detected",                   type="text",     placeholder="NMSC, melanoma — date/type"),
        list(id="ms_ptld",          label="PTLD suspected or diagnosed",             type="checkbox", checked=FALSE),
        list(id="ms_other",         label="Other malignancy post-transplant",        type="text",     placeholder="Type / date")
      )),

      list(heading = "Investigations", type = "investigations", items = list(
        list(group = "Kidney Function & Drug Monitoring", tests = c(
          "Serum creatinine, eGFR",
          "Electrolytes (Na, K, HCO3, Mg, Phos)",
          "Tacrolimus trough level",
          "Cyclosporine C0 or C2 (if applicable)",
          "MPA trough (if MMF dose adjustment)",
          "Urine protein-to-creatinine ratio (UPCR)",
          "Urinalysis with microscopy"
        )),
        list(group = "Haematology & Biochemistry", tests = c(
          "FBC with differential",
          "Liver function tests",
          "Fasting glucose / HbA1c",
          "Fasting lipids",
          "Calcium, Phosphate, Vitamin D, PTH",
          "Uric acid"
        )),
        list(group = "Viral Monitoring", tests = c(
          "BK virus PCR (blood)",
          "CMV PCR (blood) — if seropositive D+/R-",
          "EBV PCR (if PTLD concern)"
        )),
        list(group = "Imaging", tests = c(
          "Transplant kidney ultrasound (Doppler)",
          "Echocardiogram (annual if CVD risk)",
          "DEXA scan (2-yearly)"
        )),
        list(group = "Cancer Surveillance", tests = c(
          "Annual skin examination by dermatologist",
          "Pap smear (women — annual for 3 years then 3-yearly)",
          "Mammogram (women ≥40 — annual)",
          "Colonoscopy (≥45 or per risk)",
          "PSA (men ≥50)"
        ))
      ))
    )
  ),

  # ----------------------------------------------------------
  # 4. VACCINATIONS CHECKLIST
  # ----------------------------------------------------------
  vaccinations = list(
    title = "Vaccinations Assessment",
    icon  = "syringe",
    sections = list(

      list(heading = "Patient Context", type = "history", items = list(
        list(id="vc_status",        label="Transplant / immunosuppression status",   type="select",
             choices=c("Pre-transplant (not yet immunosuppressed)",
                       "Post-transplant (on IS)",
                       "CKD — non-transplant",
                       "Dialysis",
                       "Immunocompetent"), selected="CKD — non-transplant"),
        list(id="vc_spleen",        label="Asplenic / functional asplenia",          type="checkbox", checked=FALSE),
        list(id="vc_notes",         label="Additional context (HIV, biologics, etc.)","type"="text",  placeholder="")
      )),

      list(heading = "Vaccine Status Review", type = "investigations", items = list(
        list(group = "Annual / Seasonal", tests = c(
          "Influenza (inactivated) — annual",
          "COVID-19 (mRNA/recombinant) — booster per current guidance"
        )),
        list(group = "Hepatitis", tests = c(
          "Hepatitis B Ab titre (anti-HBs) — check immunity",
          "Hepatitis B vaccine series (if non-immune: 0/1/6 months; double-dose 40µg for CKD/dialysis)",
          "Hepatitis A — 2 doses (if non-immune; indicated in CKD/transplant)"
        )),
        list(group = "Pneumococcal (AST / ACIP)", tests = c(
          "PCV20 (preferred) OR PCV15 + PPSV23 at 8 weeks",
          "PPSV23 every 5 years (if only PPSV23 used in past)",
          "For post-transplant: avoid live vaccines; PCV recommended pre-transplant"
        )),
        list(group = "MMR & Varicella (Live — Pre-transplant ONLY)", tests = c(
          "MMR — 2 doses (if non-immune; ≥4 weeks before transplant)",
          "Varicella — 2 doses (if non-immune VZV IgG; ≥4 weeks before transplant)",
          "CONTRAINDICATED post-transplant on IS"
        )),
        list(group = "Zoster", tests = c(
          "RZV (Shingrix) — recombinant adjuvanted: 2 doses ×2 months apart",
          "Preferred in immunocompromised patients",
          "Avoid Zostavax (live) in transplant recipients on IS"
        )),
        list(group = "Meningococcal", tests = c(
          "MenACWY (MCV4) — 1–2 doses (asplenia, complement deficiency, eculizumab use)",
          "MenB — 2–3 doses (age <25 or asplenia)"
        )),
        list(group = "Tdap / Td", tests = c(
          "Tdap (tetanus-diphtheria-pertussis) — 1 dose if not received in adulthood",
          "Td booster — every 10 years"
        )),
        list(group = "HPV", tests = c(
          "HPV vaccine (Gardasil 9) — up to age 45 (shared decision); 3 doses in immunocompromised"
        ))
      ))
    )
  ),

  # ----------------------------------------------------------
  # 5. SECONDARY HYPERTENSION WORKUP
  # ----------------------------------------------------------
  secondary_htn = list(
    title = "Secondary Hypertension Workup",
    icon  = "heart-pulse",
    sections = list(

      list(heading = "Clinical Assessment", type = "history", items = list(
        list(id="sh_onset",         label="Age at onset of hypertension",            type="text",     placeholder="e.g. <30 yrs — low threshold for workup"),
        list(id="sh_severity",      label="BP severity (current readings)",          type="text",     placeholder="e.g. 180/110 mmHg"),
        list(id="sh_resistant",     label="Resistant hypertension (≥3 drugs including diuretic at max dose)", type="checkbox", checked=FALSE),
        list(id="sh_abrupt",        label="Abrupt onset / previously well-controlled now deteriorating", type="checkbox", checked=FALSE),
        list(id="sh_meds",          label="Current antihypertensive medications",    type="text",     placeholder="List drugs and doses")
      )),

      list(heading = "Renovascular Disease (RAS / FMD)", type = "history", items = list(
        list(id="rv_bruit",         label="Abdominal / flank bruit on auscultation", type="checkbox", checked=FALSE),
        list(id="rv_flash_pe",      label="Flash pulmonary oedema episodes",         type="checkbox", checked=FALSE),
        list(id="rv_acei_cr",       label="ACEi/ARB causing acute creatinine rise ≥30%", type="checkbox", checked=FALSE),
        list(id="rv_asymm",         label="Asymmetric kidneys on imaging",           type="checkbox", checked=FALSE),
        list(id="rv_fmd",           label="Young female, fibromuscular dysplasia suspected", type="checkbox", checked=FALSE)
      )),

      list(heading = "Primary Hyperaldosteronism (PA)", type = "history", items = list(
        list(id="pa_hypok",         label="Spontaneous hypokalaemia",                type="checkbox", checked=FALSE),
        list(id="pa_hypok_val",     label="Serum K+ value",                          type="text",     placeholder="mmol/L"),
        list(id="pa_adrenal",       label="Adrenal incidentaloma on imaging",        type="checkbox", checked=FALSE),
        list(id="pa_fam",           label="Family history of early HTN / PA",        type="checkbox", checked=FALSE),
        list(id="pa_stroke",        label="Stroke at age <40",                       type="checkbox", checked=FALSE)
      )),

      list(heading = "Phaeochromocytoma / Paraganglioma", type = "history", items = list(
        list(id="ph_triad",         label="Classic triad: headache, sweating, palpitations", type="checkbox", checked=FALSE),
        list(id="ph_parox",         label="Paroxysmal hypertension",                 type="checkbox", checked=FALSE),
        list(id="ph_fam",           label="Family history of phaeochromocytoma / MEN2 / VHL / NF1", type="checkbox", checked=FALSE),
        list(id="ph_adrenal",       label="Adrenal mass on imaging",                 type="checkbox", checked=FALSE)
      )),

      list(heading = "Cushing's Syndrome", type = "history", items = list(
        list(id="cs_weight",        label="Central weight gain / moon face / buffalo hump", type="checkbox", checked=FALSE),
        list(id="cs_striae",        label="Purple striae, easy bruising, proximal myopathy", type="checkbox", checked=FALSE),
        list(id="cs_steroids",      label="Exogenous steroid use",                   type="checkbox", checked=FALSE),
        list(id="cs_dm",            label="Diabetes + HTN + osteoporosis cluster",   type="checkbox", checked=FALSE)
      )),

      list(heading = "Other Causes", type = "history", items = list(
        list(id="oc_osa",           label="Obstructive sleep apnoea (snoring, EDS, witnessed apnoeas)", type="checkbox", checked=FALSE),
        list(id="oc_thyroid",       label="Thyroid disease symptoms",                type="text",     placeholder="Hypothyroid (bradycardia, cold) / Hyper (tachycardia, tremor)"),
        list(id="oc_coarct",        label="Aortic coarctation features (lower limb BP lower, radio-femoral delay)", type="checkbox", checked=FALSE),
        list(id="oc_meds",          label="Drug-induced HTN (NSAIDs, OCP, decongestants, calcineurin inhibitors, venlafaxine)", type="text", placeholder="List offending drugs"),
        list(id="oc_ckd",           label="Renal parenchymal disease (CKD)",         type="checkbox", checked=FALSE)
      )),

      list(heading = "Investigations", type = "investigations", items = list(
        list(group = "First-Line (All Patients)", tests = c(
          "Serum electrolytes (Na, K, Cl, HCO3)",
          "Serum creatinine, eGFR",
          "Fasting glucose",
          "FBC",
          "Urinalysis with microscopy",
          "12-lead ECG",
          "Renal ultrasound (size, cortical thickness, scarring)"
        )),
        list(group = "Renovascular Disease", tests = c(
          "Renal artery Doppler ultrasound",
          "CT renal angiography (CTA)",
          "MR angiography (MRA — if contrast contraindicated)",
          "Captopril renogram (selected cases)"
        )),
        list(group = "Primary Hyperaldosteronism", tests = c(
          "Plasma aldosterone-to-renin ratio (ARR) — morning, seated",
          "Note: stop spironolactone, eplerenone ≥6 weeks; stop ACEi/ARB/diuretics ≥2 weeks before test",
          "Confirmatory test: fludrocortisone suppression OR IV saline infusion",
          "Adrenal CT (thin-slice, non-contrast) if ARR positive",
          "Adrenal vein sampling (AVS) — if bilateral disease or surgical candidate"
        )),
        list(group = "Phaeochromocytoma", tests = c(
          "24-hour urine metanephrines + normetanephrines + catecholamines",
          "OR plasma free metanephrines / normetanephrines",
          "CT abdomen/pelvis (adrenal + sympathetic chain)",
          "MIBG scintigraphy or DOTATATE PET (if CT equivocal or metastatic concern)",
          "Genetic testing (RET, VHL, SDHB/C/D, NF1) — especially if age <45 or bilateral"
        )),
        list(group = "Cushing's Syndrome", tests = c(
          "Overnight 1 mg dexamethasone suppression test (DST)",
          "24-hour urinary free cortisol (×2)",
          "Late-night salivary cortisol (×2)",
          "ACTH level (if confirmed hypercortisolaemia)",
          "MRI pituitary (if ACTH-dependent)",
          "CT chest/abdomen/pelvis (if ectopic ACTH)"
        )),
        list(group = "Other", tests = c(
          "TSH, Free T4 (thyroid disease)",
          "Sleep study (polysomnography) — OSA",
          "Echocardiogram + CT aorta (coarctation)",
          "BP in all four limbs (coarctation: lower limb BP lower)"
        ))
      ))
    )
  ),

  # ----------------------------------------------------------
  # 6. RENAL STONE METABOLIC WORKUP
  # ----------------------------------------------------------
  renal_stone = list(
    title = "Renal Stone Metabolic Workup",
    icon  = "magnifying-glass",
    sections = list(

      list(heading = "Stone History", type = "history", items = list(
        list(id="rs_first",         label="Age at first stone",                      type="text",     placeholder="Years"),
        list(id="rs_number",        label="Number of stone episodes",                type="text",     placeholder=""),
        list(id="rs_type",          label="Stone composition (if analysed)",         type="select",
             choices=c("Unknown","Calcium oxalate monohydrate","Calcium oxalate dihydrate",
                       "Calcium phosphate (apatite / brushite)","Uric acid","Struvite / infection",
                       "Cystine","Mixed"), selected="Unknown"),
        list(id="rs_location",      label="Stone location (current/prior)",          type="text",     placeholder="Right ureter / bilateral / calyceal"),
        list(id="rs_tx",            label="Treatment received",                      type="text",     placeholder="SWL / URS / PCNL / spontaneous passage"),
        list(id="rs_fam",           label="Family history of kidney stones",         type="checkbox", checked=FALSE),
        list(id="rs_recur",         label="Recurrent stone former (≥2 episodes)",    type="checkbox", checked=FALSE)
      )),

      list(heading = "Dietary History", type = "history", items = list(
        list(id="dh_fluid",         label="Daily fluid intake (approximate)",        type="text",     placeholder="e.g. 1.5 L/day"),
        list(id="dh_calcium",       label="Dietary calcium intake",                  type="select",
             choices=c("Low (<500mg/day)","Normal (800-1200mg/day)","High (>1200mg/day)"), selected="Normal (800-1200mg/day)"),
        list(id="dh_oxalate",       label="High-oxalate foods (spinach, nuts, chocolate, tea)", type="checkbox", checked=FALSE),
        list(id="dh_sodium",        label="High sodium intake (>2g/day)",            type="checkbox", checked=FALSE),
        list(id="dh_protein",       label="High animal protein intake",              type="checkbox", checked=FALSE),
        list(id="dh_purines",       label="High purine diet (meat, organ meat, seafood)", type="checkbox", checked=FALSE),
        list(id="dh_citrus",        label="Low citrus fruit intake",                 type="checkbox", checked=FALSE),
        list(id="dh_vitc",          label="Vitamin C supplements >500mg/day",        type="checkbox", checked=FALSE),
        list(id="dh_vitd",          label="Vitamin D supplementation (dose)",        type="text",     placeholder="IU/day")
      )),

      list(heading = "Medical History & Risk Factors", type = "history", items = list(
        list(id="mh_hpt",           label="Hyperparathyroidism (primary)",           type="checkbox", checked=FALSE),
        list(id="mh_gout",          label="Gout / hyperuricaemia",                   type="checkbox", checked=FALSE),
        list(id="mh_rta",           label="Renal tubular acidosis (Type 1)",         type="checkbox", checked=FALSE),
        list(id="mh_ibd",           label="Inflammatory bowel disease (Crohn's)",    type="checkbox", checked=FALSE),
        list(id="mh_bariatric",     label="Bariatric surgery (RYGB especially)",     type="checkbox", checked=FALSE),
        list(id="mh_msk",           label="Medullary sponge kidney",                 type="checkbox", checked=FALSE),
        list(id="mh_cystinuria",    label="Cystinuria (family history / early onset)", type="checkbox", checked=FALSE),
        list(id="mh_sarcoid",       label="Sarcoidosis",                             type="checkbox", checked=FALSE),
        list(id="mh_osteo",         label="Osteoporosis / immobilisation",           type="checkbox", checked=FALSE),
        list(id="mh_dm",            label="Diabetes mellitus / insulin resistance",  type="checkbox", checked=FALSE),
        list(id="mh_meds",          label="Lithogenic medications",                  type="text",
             placeholder="Topiramate, acetazolamide, triamterene, indinavir, calcium supplements, vit D")
      )),

      list(heading = "Investigations", type = "investigations", items = list(
        list(group = "Serum / Blood", tests = c(
          "Serum creatinine, eGFR",
          "Serum electrolytes (Na, K, Cl, HCO3)",
          "Serum calcium (corrected)",
          "Serum phosphate",
          "Serum uric acid",
          "Serum magnesium",
          "PTH (intact)",
          "25-OH Vitamin D",
          "Fasting glucose",
          "Serum oxalate (if primary hyperoxaluria suspected)"
        )),
        list(group = "Urine", tests = c(
          "Urinalysis with microscopy & pH",
          "Urine culture",
          "Spot urine calcium-to-creatinine ratio (spot screening)",
          "Cystine qualitative screen (nitroprusside test) if cystine stone suspected"
        )),
        list(group = "24-Hour Urine Metabolic Profile (AUA/EAU Recommended)", tests = c(
          "24-hour urine VOLUME (target >2.5L/day)",
          "24-hour urine CALCIUM (NR <7.5 mmol/day men; <6.25 mmol/day women)",
          "24-hour urine OXALATE (NR <0.5 mmol/day)",
          "24-hour urine URIC ACID (NR <4.5 mmol/day)",
          "24-hour urine CITRATE (NR >1.7 mmol/day; low in RTA / stone formers)",
          "24-hour urine SODIUM (target <100 mmol/day)",
          "24-hour urine PHOSPHATE",
          "24-hour urine CREATININE (validates completeness of collection)",
          "24-hour urine MAGNESIUM",
          "24-hour urine pH profile (uric acid stones: pH <5.5; RTA: persistently alkaline)",
          "Note: collect ×2 separate collections for reliability; patient on usual diet"
        )),
        list(group = "Imaging", tests = c(
          "Non-contrast CT KUB (gold standard — stone composition clues from HU)",
          "KUB X-ray (radio-opaque stones: Ca-oxalate, Ca-phosphate; radiolucent: uric acid, cystine)",
          "Renal ultrasound (follow-up, avoiding radiation)"
        )),
        list(group = "Stone Analysis", tests = c(
          "Stone composition analysis (infrared spectroscopy / X-ray crystallography) — if stone retrieved"
        )),
        list(group = "Genetic Testing (Selected Cases)", tests = c(
          "Cystinuria gene panel (SLC3A1, SLC7A9) — if cystine stone",
          "Primary hyperoxaluria panel (AGXT, GRHPR, HOGA1) — if Ca-oxalate + high urinary oxalate",
          "Dent disease / Lowe syndrome — if hypercalciuria + proteinuria + young male"
        ))
      ))
    )
  )
)

# ============================================================
# UI FUNCTION
# ============================================================

checklists_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("
      .checklist-section-heading {
        font-size: 0.8rem; font-weight: 700; color: #495057;
        text-transform: uppercase; letter-spacing: 0.5px;
        border-bottom: 2px solid #dee2e6; padding-bottom: 4px; margin-bottom: 8px; margin-top: 12px;
      }
      .checklist-invest-group {
        font-size: 0.78rem; font-weight: 600; color: #6c757d;
        margin-top: 8px; margin-bottom: 3px;
      }
      .checklist-item { margin-bottom: 2px; }
      .checklist-item label { font-size: 0.82rem; }
      .checklist-modal-body { max-height: 70vh; overflow-y: auto; padding: 0 5px; }
      .cl-notes-area { font-size: 0.82rem; }
      .cl-result-wrap { width: 160px; flex-shrink: 0; }
      .cl-date-wrap   { width: 110px; flex-shrink: 0; }
      .cl-result-wrap .form-control,
      .cl-date-wrap .form-control { font-size: 0.78rem !important; padding: 2px 6px !important; height: auto !important; }
    "))),

    actionButton(
      ns("open_checklists"),
      label  = "Specialist Checklists",
      icon   = icon("clipboard-list"),
      class  = "btn-sm btn-outline-info"
    )
  )
}

# ============================================================
# SERVER FUNCTION
# ============================================================

checklists_server <- function(id, pool, current_pt, user_info, is_locked = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive that callers observe to get text to append to clinic notes
    notes_to_transfer <- reactiveVal("")

    # Mirror main notes lock state: disable button when locked, enable when unlocked
    observeEvent(is_locked(), {
      if (isTRUE(is_locked())) {
        shinyjs::disable("open_checklists")
      } else {
        shinyjs::enable("open_checklists")
      }
    }, ignoreNULL = FALSE)

    # ---- Open modal ----
    observeEvent(input$open_checklists, {
      # Guard: should not fire when locked (button disabled), but belt-and-braces
      if (isTRUE(is_locked())) {
        showNotification("Notes are locked. Click 'Edit Record' before opening Specialist Checklists.", type = "warning", duration = 5)
        return()
      }
      if (is.null(current_pt())) {
        showNotification("Please select a patient before opening checklists.", type = "warning", duration = 4)
        return()
      }

      cl_choices <- setNames(
        names(CHECKLIST_DEFS),
        sapply(CHECKLIST_DEFS, `[[`, "title")
      )

      showModal(modalDialog(
        title = div(icon("clipboard-list"), " Specialist Checklists"),
        size  = "xl",
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("cl_save_btn"),     "Save to Database",    icon = icon("database"),   class = "btn-success"),
          actionButton(ns("cl_transfer_btn"), "Transfer to Notes",   icon = icon("file-import"), class = "btn-primary"),
          modalButton("Close")
        ),

        div(class = "checklist-modal-body",
            selectInput(ns("cl_type"),
                        label = div(icon("list-check"), " Select Checklist"),
                        choices = cl_choices,
                        width = "100%"),
            hr(style = "margin: 8px 0;"),
            uiOutput(ns("cl_body"))
        )
      ))
    })

    # ---- Render checklist body (with previous data pre-populated) ----
    output$cl_body <- renderUI({
      cl_key <- input$cl_type
      req(cl_key, cl_key %in% names(CHECKLIST_DEFS))

      # Load most recent save for this patient + checklist type
      prev_data  <- list()
      saved_at   <- NULL
      pt <- current_pt()
      if (!is.null(pt)) {
        tryCatch({
          row <- pool::dbGetQuery(pool,
            "SELECT checklist_data, created_at FROM specialist_checklists
             WHERE patient_id = $1 AND checklist_type = $2
             ORDER BY created_at DESC LIMIT 1",
            list(as.integer(pt$id), cl_key))
          if (nrow(row) > 0) {
            prev_data <- jsonlite::fromJSON(row$checklist_data[1], simplifyVector = FALSE)
            saved_at  <- format(as.POSIXct(row$created_at[1], tz = "UTC"),
                                "%d %b %Y %H:%M", tz = Sys.timezone())
          }
        }, error = function(e) { })
      }

      defn <- CHECKLIST_DEFS[[cl_key]]
      sections_ui <- lapply(defn$sections, function(sec) {
        tagList(
          div(class = "checklist-section-heading", sec$heading),
          if (sec$type == "history") {
            render_history_section(ns, cl_key, sec$items, prev_data)
          } else {
            render_investigation_section(ns, cl_key, sec$items, prev_data)
          }
        )
      })

      addl_id  <- paste0(cl_key, "_addl_notes")
      addl_val <- prev_data_str(prev_data, addl_id, "")

      tagList(
        div(class = "d-flex justify-content-between align-items-center mb-1",
            div(style = "font-weight: 600; font-size: 0.9rem; color: #2c3e50;", defn$title),
            if (!is.null(saved_at))
              div(class = "badge bg-info text-white",
                  icon("clock-rotate-left"), " Last saved: ", saved_at)
        ),
        sections_ui,
        div(class = "checklist-section-heading", "Additional Notes"),
        textAreaInput(ns(addl_id),
                      label   = NULL,
                      value   = addl_val,
                      rows    = 3,
                      width   = "100%",
                      placeholder = "Any additional clinical notes for this checklist...",
                      resize  = "vertical") %>%
          tagAppendAttributes(class = "cl-notes-area")
      )
    })

    # ---- Save to DB ----
    observeEvent(input$cl_save_btn, {
      req(current_pt(), input$cl_type)
      pt_id  <- current_pt()$id
      cl_key <- input$cl_type

      cl_data <- collect_checklist_values(input, cl_key, ns)

      tryCatch({
        # Create table if it doesn't exist (idempotent)
        pool::dbExecute(pool,
          "CREATE TABLE IF NOT EXISTS specialist_checklists (
            id           SERIAL PRIMARY KEY,
            patient_id   INTEGER NOT NULL,
            checklist_type VARCHAR(100),
            checklist_data JSONB,
            created_by   VARCHAR(100),
            created_at   TIMESTAMPTZ DEFAULT NOW()
          )")

        pool::dbExecute(pool,
          "INSERT INTO specialist_checklists (patient_id, checklist_type, checklist_data, created_by)
           VALUES ($1, $2, $3::jsonb, $4)",
          list(
            as.integer(pt_id),
            cl_key,
            jsonlite::toJSON(cl_data, auto_unbox = TRUE),
            tryCatch(user_info()$username, error = function(e) "unknown")
          ))

        showNotification("Checklist saved to database.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Save error:", e$message), type = "error", duration = 8)
      })
    })

    # ---- Transfer to Notes ----
    observeEvent(input$cl_transfer_btn, {
      req(input$cl_type)
      cl_key <- input$cl_type
      defn   <- CHECKLIST_DEFS[[cl_key]]

      cl_data <- collect_checklist_values(input, cl_key, ns)
      text    <- format_checklist_as_text(defn, cl_data, cl_key, input, ns)

      if (nzchar(trimws(text))) {
        notes_to_transfer(text)
        showNotification("Checklist transferred to clinic notes.", type = "message", duration = 3)
        removeModal()
      } else {
        showNotification("No data to transfer — please fill in the checklist first.", type = "warning")
      }
    })

    # Return interface
    list(notes_to_transfer = notes_to_transfer)
  })
}

# ============================================================
# HELPER: Safe value extractor from prev_data (fromJSON list)
# ============================================================

prev_data_str <- function(prev_data, key, default = "") {
  val <- prev_data[[key]]
  if (is.null(val)) return(default)
  v <- tryCatch(as.character(val), error = function(e) default)
  if (length(v) == 0 || is.na(v) || v == "NA") default else v
}

prev_data_bool <- function(prev_data, key, default = FALSE) {
  val <- prev_data[[key]]
  if (is.null(val)) return(default)
  v <- tryCatch(as.logical(val), error = function(e) default)
  if (length(v) == 0 || is.na(v)) default else v
}

# ============================================================
# HELPER: Render history section items
# ============================================================

render_history_section <- function(ns, cl_key, items, prev_data = list()) {
  lapply(items, function(it) {
    input_id <- ns(paste0(cl_key, "_", it$id))
    data_key <- paste0(cl_key, "_", it$id)
    div(class = "checklist-item",
      switch(it$type,
        "checkbox" = checkboxInput(input_id, label = it$label,
                                   value = prev_data_bool(prev_data, data_key, isTRUE(it$checked))),
        "text"     = div(style = "display: flex; align-items: baseline; gap: 8px;",
                         tags$label(`for` = input_id,
                                    style = "font-size: 0.82rem; min-width: 260px; font-weight: 500;",
                                    it$label),
                         textInput(input_id, label = NULL,
                                   value       = prev_data_str(prev_data, data_key, ""),
                                   placeholder = if (!is.null(it$placeholder)) it$placeholder else "",
                                   width = "100%") %>%
                           tagAppendAttributes(style = "margin-bottom: 0;")
                    ),
        "select"   = div(style = "display: flex; align-items: baseline; gap: 8px;",
                         tags$label(`for` = input_id,
                                    style = "font-size: 0.82rem; min-width: 260px; font-weight: 500;",
                                    it$label),
                         selectInput(input_id, label = NULL,
                                     choices  = it$choices,
                                     selected = prev_data_str(prev_data, data_key, it$selected),
                                     width    = "100%") %>%
                           tagAppendAttributes(style = "margin-bottom: 0;")
                    ),
        tags$span("unsupported type")
      )
    )
  })
}

# ============================================================
# HELPER: Render investigation section
# ============================================================

render_investigation_section <- function(ns, cl_key, groups, prev_data = list()) {
  lapply(groups, function(grp) {
    tagList(
      div(class = "checklist-invest-group", grp$group),
      lapply(grp$tests, function(test_name) {
        data_key   <- paste0(cl_key, "_inv_", gsub("[^A-Za-z0-9]", "_", test_name))
        result_key <- paste0(data_key, "_result")
        date_key   <- paste0(data_key, "_date")
        safe_id    <- ns(data_key)
        result_id  <- ns(result_key)
        date_id    <- ns(date_key)
        div(class = "d-flex align-items-center gap-2 checklist-item",
            div(style = "flex: 1; min-width: 0;",
                checkboxInput(safe_id, label = test_name,
                              value = prev_data_bool(prev_data, data_key, FALSE))
            ),
            div(class = "cl-result-wrap",
                textInput(result_id, label = NULL,
                          value       = prev_data_str(prev_data, result_key, ""),
                          placeholder = "Result",
                          width       = "100%") %>%
                  tagAppendAttributes(style = "margin: 0;")
            ),
            div(class = "cl-date-wrap",
                textInput(date_id, label = NULL,
                          value       = prev_data_str(prev_data, date_key, ""),
                          placeholder = "Date",
                          width       = "100%") %>%
                  tagAppendAttributes(style = "margin: 0;")
            )
        )
      })
    )
  })
}

# ============================================================
# HELPER: Collect all checklist values from input
# ============================================================

collect_checklist_values <- function(input, cl_key, ns) {
  defn <- CHECKLIST_DEFS[[cl_key]]
  out  <- list()

  for (sec in defn$sections) {
    if (sec$type == "history") {
      for (it in sec$items) {
        full_id    <- paste0(cl_key, "_", it$id)
        shiny_id   <- full_id   # without ns() — input$ already in module context
        val        <- input[[shiny_id]]
        out[[full_id]] <- val
      }
    } else {
      for (grp in sec$items) {
        for (test_name in grp$tests) {
          full_id    <- paste0(cl_key, "_inv_", gsub("[^A-Za-z0-9]", "_", test_name))
          result_key <- paste0(full_id, "_result")
          date_key   <- paste0(full_id, "_date")
          out[[full_id]]    <- input[[full_id]]
          out[[result_key]] <- input[[result_key]]
          out[[date_key]]   <- input[[date_key]]
        }
      }
    }
  }

  addl_id <- paste0(cl_key, "_addl_notes")
  out[[addl_id]] <- input[[addl_id]]

  out
}

# ============================================================
# HELPER: Format filled checklist as readable text
# ============================================================

format_checklist_as_text <- function(defn, cl_data, cl_key, input, ns) {
  lines <- character(0)
  lines <- c(lines, paste0("=== ", defn$title, " ==="))
  lines <- c(lines, paste0("Date: ", format(Sys.Date(), "%d %b %Y")))
  lines <- c(lines, "")

  for (sec in defn$sections) {
    section_lines <- character(0)

    if (sec$type == "history") {
      for (it in sec$items) {
        val <- cl_data[[paste0(cl_key, "_", it$id)]]
        if (is.null(val)) next

        if (it$type == "checkbox") {
          if (isTRUE(val)) {
            section_lines <- c(section_lines, paste0("  [+] ", it$label))
          }
        } else if (it$type %in% c("text", "select")) {
          val_str <- trimws(as.character(val))
          # Skip if empty, NA, or default "select" value that isn't meaningful
          if (!is.na(val_str) && nzchar(val_str) && val_str != "NA") {
            section_lines <- c(section_lines, paste0("  ", it$label, ": ", val_str))
          }
        }
      }
    } else {
      # Investigations: include checked items or items with a result filled in
      checked_tests <- character(0)
      for (grp in sec$items) {
        grp_lines <- character(0)
        for (test_name in grp$tests) {
          full_id    <- paste0(cl_key, "_inv_", gsub("[^A-Za-z0-9]", "_", test_name))
          result_key <- paste0(full_id, "_result")
          date_key   <- paste0(full_id, "_date")
          checked    <- isTRUE(cl_data[[full_id]])
          result_raw <- cl_data[[result_key]]
          date_raw   <- cl_data[[date_key]]
          result_txt <- if (!is.null(result_raw)) trimws(as.character(result_raw)) else ""
          date_txt   <- if (!is.null(date_raw))   trimws(as.character(date_raw))   else ""
          has_result <- !is.na(result_txt) && nzchar(result_txt) && result_txt != "NA"
          has_date   <- !is.na(date_txt)   && nzchar(date_txt)   && date_txt   != "NA"
          if (checked || has_result || has_date) {
            line <- paste0("    - ", test_name)
            if (has_result) line <- paste0(line, ": ", result_txt)
            if (has_date)   line <- paste0(line, " (", date_txt, ")")
            grp_lines <- c(grp_lines, line)
          }
        }
        if (length(grp_lines) > 0) {
          checked_tests <- c(checked_tests, paste0("  [", grp$group, "]"))
          checked_tests <- c(checked_tests, grp_lines)
        }
      }
      if (length(checked_tests) > 0) {
        section_lines <- checked_tests
      }
    }

    if (length(section_lines) > 0) {
      lines <- c(lines, paste0("--- ", sec$heading, " ---"))
      lines <- c(lines, section_lines)
      lines <- c(lines, "")
    }
  }

  # Additional notes
  addl <- trimws(as.character(cl_data[[paste0(cl_key, "_addl_notes")]]))
  if (!is.na(addl) && nzchar(addl) && addl != "NA") {
    lines <- c(lines, "--- Additional Notes ---")
    lines <- c(lines, paste0("  ", addl))
    lines <- c(lines, "")
  }

  paste(lines, collapse = "\n")
}
