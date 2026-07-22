# Project: Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
# Author: Akash Bhardwaj 
# Purpose: Inspect selected SDTM and ADaM datasets from the public CDISC Pilot Project 

library(tidyverse) 
library(haven) 
library(here) 

# Define the location of the locally downloaded CDISC Pilot Project package 

pilot_root <- here( 
  "data_raw", 
  "cdisc_pilot_source", 
  "updated-pilot-submission-package", 
  "900172", 
  "m5", 
  "datasets", 
  "cdiscpilot01" 
) 

# Define dataset file paths 

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

dm_file <- file.path( 
  pilot_root, 
  "tabulations", 
  "sdtm", 
  "dm.xpt" 
) 

ae_file <- file.path( 
  pilot_root, 
  "tabulations", 
  "sdtm", 
  "ae.xpt" 
) 

# Confirm that all required files exist before reading them 

required_files <- c( 
  adsl_file, 
  adae_file, 
  adtte_file, 
  dm_file, 
  ae_file 
) 

stopifnot(all(file.exists(required_files))) 

# Read SAS Transport datasets 

adsl <- read_xpt(adsl_file) 
adae <- read_xpt(adae_file) 
adtte <- read_xpt(adtte_file) 
dm <- read_xpt(dm_file) 
ae <- read_xpt(ae_file) 

# Store dataset names and data frames in lists 

dataset_list <- list( 
  ADSL = adsl, 
  ADAE = adae, 
  ADTTE = adtte, 
  DM = dm, 
  AE = ae 
) 

dataset_type <- c( 
  ADSL = "ADaM", 
  ADAE = "ADaM", 
  ADTTE = "ADaM", 
  DM = "SDTM", 
  AE = "SDTM" 
) 

# Create a dataset-level inventory: 
# rows, columns, and unique subject IDs where USUBJID is available 

dataset_inventory <- imap_dfr( 
  dataset_list, 
  function(data, dataset_name) { 
    
    tibble( 
      dataset = dataset_name, 
      dataset_standard = dataset_type[[dataset_name]], 
      number_of_rows = nrow(data), 
      number_of_columns = ncol(data), 
      unique_subjects = if ("USUBJID" %in% names(data)) { 
        n_distinct(data$USUBJID) 
      } else { 
        NA_integer_ 
      } 
    ) 
  } 
) 

print(dataset_inventory) 

write_csv( 
  dataset_inventory, 
  here("outputs", "tables", "cdisc_dataset_inventory.csv") 
) 

# Create a variable-level inventory. 
# Labels come from metadata embedded in the XPT files. 

variable_inventory <- imap_dfr( 
  dataset_list, 
  function(data, dataset_name) { 
    
    tibble( 
      dataset = dataset_name, 
      variable = names(data), 
      data_type = map_chr(data, ~ class(.x)), 
      variable_label = map_chr( 
        data, 
        ~ { 
          label_value <- attr(.x, "label") 
          
          if (is.null(label_value)) { 
            "" 
          } else { 
            as.character(label_value) 
          } 
        } 
      ) 
    ) 
  } 
) 

write_csv( 
  variable_inventory, 
  here("references", "cdisc_selected_dataset_variable_inventory.csv") 
) 

# Print column names for review 

cat("\nADSL variable names:\n") 
print(names(adsl)) 

cat("\nADAE variable names:\n") 
print(names(adae)) 

cat("\nADTTE variable names:\n") 
print(names(adtte)) 

cat("\nDM variable names:\n") 
print(names(dm)) 

cat("\nAE variable names:\n") 
print(names(ae)) 

# Inspect the first rows of the key ADaM datasets 

cat("\nFirst rows of ADSL:\n") 
print(head(adsl)) 

cat("\nFirst rows of ADAE:\n") 
print(head(adae)) 

cat("\nFirst rows of ADTTE:\n") 
print(head(adtte)) 

# Check whether all ADSL subjects are represented in each selected analysis dataset 

subject_coverage <- tibble( 
  dataset = c("ADAE", "ADTTE"), 
  adsl_subjects = n_distinct(adsl$USUBJID), 
  subjects_in_dataset = c( 
    n_distinct(adae$USUBJID), 
    n_distinct(adtte$USUBJID) 
  ), 
  adsl_subjects_missing_from_dataset = c( 
    length(setdiff(adsl$USUBJID, adae$USUBJID)), 
    length(setdiff(adsl$USUBJID, adtte$USUBJID)) 
  ) 
) 

print(subject_coverage) 

write_csv( 
  subject_coverage, 
  here("outputs", "tables", "cdisc_subject_coverage_checks.csv") 
) 

cat("\nCDISC pilot-data inspection completed successfully.\n") 
