# Project: Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
# Author: Akash Bhardwaj 
# Purpose: Review actual-treatment mismatches between ADaM ADSL and SDTM DM 

library(tidyverse) 
library(haven) 
library(here) 

# Define the CDISC Pilot Project source location 

pilot_root <- here( 
  "data_raw", 
  "cdisc_pilot_source", 
  "updated-pilot-submission-package", 
  "900172", 
  "m5", 
  "datasets", 
  "cdiscpilot01" 
) 

# Read ADSL and DM source datasets 

adsl <- read_xpt( 
  file.path( 
    pilot_root, 
    "analysis", 
    "adam", 
    "datasets", 
    "adsl.xpt" 
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

# Restrict ADSL to the safety population and compare treatment variables 

treatment_review <- adsl %>% 
  filter(SAFFL == "Y") %>% 
  select( 
    USUBJID, 
    SITEID, 
    planned_treatment_adsl = TRT01P, 
    actual_treatment_adsl = TRT01A, 
    treatment_start_date = TRTSDT, 
    treatment_end_date = TRTEDT, 
    treatment_duration_days = TRTDUR, 
    discontinuation_flag = DISCONFL, 
    discontinuation_reason = DCDECOD, 
    discontinuation_reason_code = DCREASCD 
  ) %>% 
  left_join( 
    dm %>% 
      select( 
        USUBJID, 
        planned_arm_sdtm = ARM, 
        actual_arm_sdtm = ACTARM 
      ), 
    by = "USUBJID" 
  ) %>% 
  mutate( 
    planned_treatment_match = 
      planned_treatment_adsl == planned_arm_sdtm, 
    
    actual_treatment_match = 
      actual_treatment_adsl == actual_arm_sdtm 
  ) 

# Keep only records where actual treatment differs 

actual_treatment_mismatches <- treatment_review %>% 
  filter( 
    !is.na(actual_treatment_match), 
    !actual_treatment_match 
  ) %>% 
  arrange( 
    planned_treatment_adsl, 
    actual_treatment_adsl 
  ) 

print(actual_treatment_mismatches) 

write_csv( 
  actual_treatment_mismatches, 
  here( 
    "outputs", 
    "tables", 
    "actual_treatment_alignment_review.csv" 
  ) 
) 

# Summarise the pattern of actual-treatment mismatches 

mismatch_pattern_summary <- actual_treatment_mismatches %>% 
  count( 
    planned_treatment_adsl, 
    actual_treatment_adsl, 
    actual_arm_sdtm, 
    name = "number_of_subjects" 
  ) %>% 
  arrange( 
    desc(number_of_subjects) 
  ) 

print(mismatch_pattern_summary) 

write_csv( 
  mismatch_pattern_summary, 
  here( 
    "outputs", 
    "tables", 
    "actual_treatment_mismatch_patterns.csv" 
  ) 
) 

# Summarise discontinuation information among mismatched records 

mismatch_discontinuation_summary <- actual_treatment_mismatches %>% 
  count( 
    discontinuation_flag, 
    discontinuation_reason, 
    name = "number_of_subjects" 
  ) %>% 
  arrange( 
    desc(number_of_subjects) 
  ) 

print(mismatch_discontinuation_summary) 

write_csv( 
  mismatch_discontinuation_summary, 
  here( 
    "outputs", 
    "tables", 
    "actual_treatment_mismatch_discontinuation_summary.csv" 
  ) 
) 

cat("Treatment-alignment review completed successfully.\n") 
cat( 
  "Actual-treatment mismatches reviewed:", 
  nrow(actual_treatment_mismatches), 
  "\n" 
) 
