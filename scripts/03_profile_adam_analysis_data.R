# Project: Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
# Author: Akash Bhardwaj 
# Purpose: Profile key ADaM datasets and define analysis-ready extracts 
# for baseline, safety, and time-to-event analysis. 

library(tidyverse) 
library(haven) 
library(here) 

# Define the local CDISC Pilot Project location 

pilot_root <- here( 
  "data_raw", 
  "cdisc_pilot_source", 
  "updated-pilot-submission-package", 
  "900172", 
  "m5", 
  "datasets", 
  "cdiscpilot01" 
) 

# Define paths to the three ADaM datasets used in this project 

adsl_file <- file.path( 
  pilot_root, 
  "analysis", 
  "adam", 
  "datasets", 
  "adsl.xpt" 
) 

adae_file <- file.path( 
  pilot_root, 
  "analysis", 
  "adam", 
  "datasets", 
  "adae.xpt" 
) 

adtte_file <- file.path( 
  pilot_root, 
  "analysis", 
  "adam", 
  "datasets", 
  "adtte.xpt" 
) 

# Read the source ADaM datasets 

adsl <- read_xpt(adsl_file) 
adae <- read_xpt(adae_file) 
adtte <- read_xpt(adtte_file) 

# ------------------------------------------------------------ 
# ADSL: subject-level and baseline analysis population 
# ------------------------------------------------------------ 

adsl_analysis <- adsl %>% 
  filter(SAFFL == "Y") %>% 
  transmute( 
    USUBJID, 
    SITEID, 
    planned_treatment = TRT01P, 
    actual_treatment = TRT01A, 
    actual_treatment_code = TRT01AN, 
    age = AGE, 
    age_group = AGEGR1, 
    sex = SEX, 
    race = RACE, 
    safety_population_flag = SAFFL, 
    intent_to_treat_flag = ITTFL, 
    efficacy_population_flag = EFFFL, 
    baseline_bmi = BMIBL, 
    baseline_height_cm = HEIGHTBL, 
    baseline_weight_kg = WEIGHTBL, 
    treatment_start_date = TRTSDT, 
    treatment_end_date = TRTEDT, 
    treatment_duration_days = TRTDUR 
  ) 

# Confirm one subject-level record per subject 

stopifnot(nrow(adsl_analysis) == n_distinct(adsl_analysis$USUBJID)) 

# Treatment-arm counts in the safety population 

treatment_counts <- adsl_analysis %>% 
  count( 
    planned_treatment, 
    actual_treatment, 
    name = "number_of_subjects" 
  ) 

print(treatment_counts) 

write_csv( 
  treatment_counts, 
  here("outputs", "tables", "adam_treatment_arm_counts.csv") 
) 

# Summary of key baseline variables 

baseline_missingness <- adsl_analysis %>% 
  summarise( 
    across( 
      everything(), 
      ~ sum(is.na(.x)) 
    ) 
  ) %>% 
  pivot_longer( 
    cols = everything(), 
    names_to = "variable", 
    values_to = "missing_values" 
  ) %>% 
  arrange(desc(missing_values), variable) 

print(baseline_missingness) 

write_csv( 
  baseline_missingness, 
  here("outputs", "tables", "adam_adsl_missingness_summary.csv") 
) 

# ------------------------------------------------------------ 
# ADAE: treatment-emergent adverse-event profile 
# ------------------------------------------------------------ 

adae_safety <- adae %>% 
  filter(SAFFL == "Y") %>% 
  transmute( 
    USUBJID, 
    actual_treatment = TRTA, 
    actual_treatment_code = TRTAN, 
    age = AGE, 
    sex = SEX, 
    adverse_event_term = AETERM, 
    preferred_term = AEDECOD, 
    severity = AESEV, 
    serious_event_flag = AESER, 
    relatedness = AEREL, 
    treatment_emergent_flag = TRTEMFL, 
    event_start_date = ASTDT, 
    event_end_date = AENDT, 
    event_duration_days = ADURN, 
    outcome = AEOUT 
  ) 

# Check treatment-emergent flag values before defining safety summaries 

treatment_emergent_flag_counts <- adae_safety %>% 
  count( 
    treatment_emergent_flag, 
    name = "number_of_adverse_event_records" 
  ) 

print(treatment_emergent_flag_counts) 

write_csv( 
  treatment_emergent_flag_counts, 
  here("outputs", "tables", "adam_adae_treatment_emergent_flag_counts.csv") 
) 

# Count subjects and AE records by treatment arm. 
# This is descriptive; a full safety summary will be created later. 

adae_subject_summary <- adae_safety %>% 
  group_by(actual_treatment) %>% 
  summarise( 
    adverse_event_records = n(), 
    subjects_with_any_ae = n_distinct(USUBJID), 
    .groups = "drop" 
  ) 

print(adae_subject_summary) 

write_csv( 
  adae_subject_summary, 
  here("outputs", "tables", "adam_adae_subject_summary.csv") 
) 

# ------------------------------------------------------------ 
# ADTTE: time-to-event endpoint profile 
# ------------------------------------------------------------ 

adtte_analysis <- adtte %>% 
  filter(SAFFL == "Y") %>% 
  transmute( 
    USUBJID, 
    actual_treatment = TRTA, 
    actual_treatment_code = TRTAN, 
    parameter = PARAM, 
    parameter_code = PARAMCD, 
    analysis_time = AVAL, 
    censoring_indicator = CNSR, 
    event_description = EVNTDESC, 
    analysis_start_date = STARTDT, 
    analysis_date = ADT, 
    source_domain = SRCDOM, 
    source_variable = SRCVAR, 
    safety_population_flag = SAFFL 
  ) 

# Verify whether each subject has exactly one record per endpoint 

adtte_endpoint_counts <- adtte_analysis %>% 
  count( 
    parameter, 
    parameter_code, 
    name = "number_of_records" 
  ) 

print(adtte_endpoint_counts) 

write_csv( 
  adtte_endpoint_counts, 
  here("outputs", "tables", "adam_adtte_endpoint_counts.csv") 
) 

# Inspect censoring values and event descriptions by endpoint 

adtte_censoring_profile <- adtte_analysis %>% 
  count( 
    parameter, 
    parameter_code, 
    censoring_indicator, 
    event_description, 
    name = "number_of_subjects" 
  ) %>% 
  arrange( 
    parameter_code, 
    censoring_indicator, 
    desc(number_of_subjects) 
  ) 

print(adtte_censoring_profile) 

write_csv( 
  adtte_censoring_profile, 
  here("outputs", "tables", "adam_adtte_censoring_profile.csv") 
) 

# Check treatment-arm counts in the selected time-to-event data 

adtte_treatment_counts <- adtte_analysis %>% 
  count( 
    parameter, 
    parameter_code, 
    actual_treatment, 
    name = "number_of_subjects" 
  ) 

print(adtte_treatment_counts) 

write_csv( 
  adtte_treatment_counts, 
  here("outputs", "tables", "adam_adtte_treatment_counts.csv") 
) 

# Save analysis-ready extracts locally. 
# These are derived files; source XPT files remain unchanged. 

write_csv( 
  adsl_analysis, 
  here("data_processed", "adam_adsl_analysis_ready.csv") 
) 

write_csv( 
  adae_safety, 
  here("data_processed", "adam_adae_safety_ready.csv") 
) 

write_csv( 
  adtte_analysis, 
  here("data_processed", "adam_adtte_analysis_ready.csv") 
) 

cat("ADaM analysis-data profiling completed successfully.\n") 
cat( 
  "Safety population subjects in ADSL:", 
  nrow(adsl_analysis), 
  "\n" 
) 
cat( 
  "Subjects in ADTTE analysis extract:", 
  n_distinct(adtte_analysis$USUBJID), 
  "\n" 
) 
