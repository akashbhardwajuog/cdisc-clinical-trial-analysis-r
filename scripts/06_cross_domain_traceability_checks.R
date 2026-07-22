# Project: Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
# Author: Akash Bhardwaj 
# Purpose: Perform cross-domain subject, record, and traceability checks 
# between selected SDTM and ADaM pilot datasets. 

library(tidyverse) 
library(haven) 
library(here) 

# ------------------------------------------------------------ 
# Define local CDISC Pilot Project source location 
# ------------------------------------------------------------ 

pilot_root <- here( 
  "data_raw", 
  "cdisc_pilot_source", 
  "updated-pilot-submission-package", 
  "900172", 
  "m5", 
  "datasets", 
  "cdiscpilot01" 
) 

# ------------------------------------------------------------ 
# Read required source datasets 
# ------------------------------------------------------------ 

adsl <- read_xpt( 
  file.path( 
    pilot_root, 
    "analysis", 
    "adam", 
    "datasets", 
    "adsl.xpt" 
  ) 
) 

adae <- read_xpt( 
  file.path( 
    pilot_root, 
    "analysis", 
    "adam", 
    "datasets", 
    "adae.xpt" 
  ) 
) 

adtte <- read_xpt( 
  file.path( 
    pilot_root, 
    "analysis", 
    "adam", 
    "datasets", 
    "adtte.xpt" 
  ) 
) 

dm <- read_xpt( 
  file.path( 
    pilot_root, 
    "tabulations", 
    "sdtm", 
    "dm.xpt" 
  ) 
) 

ae <- read_xpt( 
  file.path( 
    pilot_root, 
    "tabulations", 
    "sdtm", 
    "ae.xpt" 
  ) 
) 

# ------------------------------------------------------------ 
# Restrict ADaM datasets to safety-population records 
# ------------------------------------------------------------ 

adsl_safety <- adsl %>% 
  filter(SAFFL == "Y") 

adae_safety <- adae %>% 
  filter(SAFFL == "Y") 

adtte_safety <- adtte %>% 
  filter(SAFFL == "Y") 

# ------------------------------------------------------------ 
# Subject-level coverage checks 
# ------------------------------------------------------------ 

subject_coverage_checks <- tibble( 
  comparison = c( 
    "ADSL safety population represented in SDTM DM", 
    "ADAE subjects represented in SDTM AE", 
    "ADTTE subjects represented in ADSL safety population" 
  ), 
  source_subject_count = c( 
    n_distinct(adsl_safety$USUBJID), 
    n_distinct(adae_safety$USUBJID), 
    n_distinct(adtte_safety$USUBJID) 
  ), 
  matched_subject_count = c( 
    sum(unique(adsl_safety$USUBJID) %in% unique(dm$USUBJID)), 
    sum(unique(adae_safety$USUBJID) %in% unique(ae$USUBJID)), 
    sum(unique(adtte_safety$USUBJID) %in% unique(adsl_safety$USUBJID)) 
  ) 
) %>% 
  mutate( 
    unmatched_subject_count = 
      source_subject_count - matched_subject_count 
  ) 

print(subject_coverage_checks) 

write_csv( 
  subject_coverage_checks, 
  here( 
    "outputs", 
    "tables", 
    "cross_domain_subject_coverage_checks.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Treatment-value comparison: ADSL versus SDTM DM 
# ------------------------------------------------------------ 

treatment_comparison <- adsl_safety %>% 
  select( 
    USUBJID, 
    adsl_planned_treatment = TRT01P, 
    adsl_actual_treatment = TRT01A 
  ) %>% 
  left_join( 
    dm %>% 
      select( 
        USUBJID, 
        sdtm_planned_arm = ARM, 
        sdtm_actual_arm = ACTARM 
      ), 
    by = "USUBJID" 
  ) %>% 
  mutate( 
    planned_treatment_match = 
      adsl_planned_treatment == sdtm_planned_arm, 
    actual_treatment_match = 
      adsl_actual_treatment == sdtm_actual_arm 
  ) 

treatment_alignment_summary <- treatment_comparison %>% 
  summarise( 
    safety_population_subjects = n(), 
    missing_dm_record = sum(is.na(sdtm_planned_arm)), 
    planned_treatment_matches = sum( 
      planned_treatment_match, 
      na.rm = TRUE 
    ), 
    planned_treatment_mismatches = sum( 
      !planned_treatment_match, 
      na.rm = TRUE 
    ), 
    actual_treatment_matches = sum( 
      actual_treatment_match, 
      na.rm = TRUE 
    ), 
    actual_treatment_mismatches = sum( 
      !actual_treatment_match, 
      na.rm = TRUE 
    ) 
  ) 

print(treatment_alignment_summary) 

write_csv( 
  treatment_alignment_summary, 
  here( 
    "outputs", 
    "tables", 
    "cross_domain_treatment_alignment_summary.csv" 
  ) 
) 

# Save any treatment mismatches for review 

treatment_mismatches <- treatment_comparison %>% 
  filter( 
    is.na(planned_treatment_match) | 
      is.na(actual_treatment_match) | 
      !planned_treatment_match | 
      !actual_treatment_match 
  ) 

write_csv( 
  treatment_mismatches, 
  here( 
    "outputs", 
    "tables", 
    "cross_domain_treatment_mismatches.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# ADAE-to-AE record traceability checks 
# ------------------------------------------------------------ 

adae_ae_traceability <- adae_safety %>% 
  select( 
    USUBJID, 
    AESEQ, 
    TRTEMFL, 
    AEDECOD, 
    AESEV 
  ) %>% 
  left_join( 
    ae %>% 
      select( 
        USUBJID, 
        AESEQ, 
        sdtm_ae_term = AETERM, 
        sdtm_preferred_term = AEDECOD, 
        sdtm_severity = AESEV 
      ), 
    by = c("USUBJID", "AESEQ") 
  ) %>% 
  mutate( 
    source_ae_record_found = !is.na(sdtm_ae_term), 
    preferred_term_match = AEDECOD == sdtm_preferred_term, 
    severity_match = AESEV == sdtm_severity 
  ) 

adae_ae_traceability_summary <- adae_ae_traceability %>% 
  summarise( 
    adae_records = n(), 
    linked_sdtm_ae_records = sum(source_ae_record_found), 
    unlinked_sdtm_ae_records = sum(!source_ae_record_found), 
    preferred_term_matches = sum( 
      preferred_term_match, 
      na.rm = TRUE 
    ), 
    severity_matches = sum( 
      severity_match, 
      na.rm = TRUE 
    ) 
  ) 

print(adae_ae_traceability_summary) 

write_csv( 
  adae_ae_traceability_summary, 
  here( 
    "outputs", 
    "tables", 
    "adae_to_ae_traceability_summary.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# ADTTE event-source traceability checks 
# ------------------------------------------------------------ 

adtte_event_records <- adtte_safety %>% 
  filter( 
    PARAMCD == "TTDE", 
    CNSR == 0 
  ) 

adtte_ae_traceability <- adtte_event_records %>% 
  select( 
    USUBJID, 
    PARAM, 
    PARAMCD, 
    AVAL, 
    CNSR, 
    EVNTDESC, 
    SRCDOM, 
    SRCVAR, 
    SRCSEQ 
  ) %>% 
  left_join( 
    ae %>% 
      select( 
        USUBJID, 
        AESEQ, 
        AETERM, 
        AEDECOD, 
        AESEV 
      ), 
    by = c( 
      "USUBJID" = "USUBJID", 
      "SRCSEQ" = "AESEQ" 
    ) 
  ) %>% 
  mutate( 
    source_ae_record_found = !is.na(AETERM) 
  ) 

adtte_ae_traceability_summary <- adtte_ae_traceability %>% 
  summarise( 
    adtte_event_records = n(), 
    event_records_with_source_ae_match = sum( 
      source_ae_record_found 
    ), 
    event_records_without_source_ae_match = sum( 
      !source_ae_record_found 
    ) 
  ) 

print(adtte_ae_traceability_summary) 

write_csv( 
  adtte_ae_traceability_summary, 
  here( 
    "outputs", 
    "tables", 
    "adtte_to_ae_traceability_summary.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Create a concise QC issue log 
# ------------------------------------------------------------ 

qc_issue_log <- bind_rows( 
  subject_coverage_checks %>% 
    filter(unmatched_subject_count > 0) %>% 
    transmute( 
      check_area = "Subject coverage", 
      severity = "Review", 
      issue = comparison, 
      count = unmatched_subject_count, 
      recommended_action = 
        "Review population definitions and source-to-analysis dataset coverage." 
    ), 
  
  treatment_alignment_summary %>% 
    pivot_longer( 
      cols = c( 
        missing_dm_record, 
        planned_treatment_mismatches, 
        actual_treatment_mismatches 
      ), 
      names_to = "issue", 
      values_to = "count" 
    ) %>% 
    filter(count > 0) %>% 
    transmute( 
      check_area = "Treatment alignment", 
      severity = "Review", 
      issue, 
      count, 
      recommended_action = 
        "Review treatment variables and documented derivation rules." 
    ), 
  
  adae_ae_traceability_summary %>% 
    transmute( 
      check_area = "ADAE-to-AE traceability", 
      severity = if_else( 
        unlinked_sdtm_ae_records > 0, 
        "Review", 
        "Pass" 
      ), 
      issue = "ADAE records without linked SDTM AE source record", 
      count = unlinked_sdtm_ae_records, 
      recommended_action = 
        "Review AESEQ and USUBJID linkage if unmatched records are present." 
    ) %>% 
    filter(count > 0), 
  
  adtte_ae_traceability_summary %>% 
    transmute( 
      check_area = "ADTTE-to-AE traceability", 
      severity = if_else( 
        event_records_without_source_ae_match > 0, 
        "Review", 
        "Pass" 
      ), 
      issue = "ADTTE event records without linked SDTM AE source record", 
      count = event_records_without_source_ae_match, 
      recommended_action = 
        "Review source-domain, source-sequence, and event derivation metadata." 
    ) %>% 
    filter(count > 0) 
) 

if (nrow(qc_issue_log) == 0) { 
  qc_issue_log <- tibble( 
    check_area = "Cross-domain traceability", 
    severity = "Pass", 
    issue = "No mismatches identified by the implemented checks", 
    count = 0L, 
    recommended_action = 
      "Retain output as evidence of the completed programmed checks." 
  ) 
} 

print(qc_issue_log) 

write_csv( 
  qc_issue_log, 
  here( 
    "outputs", 
    "tables", 
    "cross_domain_qc_issue_log.csv" 
  ) 
) 

cat("Cross-domain traceability checks completed successfully.\n")