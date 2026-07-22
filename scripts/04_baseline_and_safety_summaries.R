# Project: Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
# Author: Akash Bhardwaj 
# Purpose: Create baseline and treatment-emergent adverse-event summaries 
# using analysis-ready ADaM extracts 

library(tidyverse) 
library(here) 

# ------------------------------------------------------------ 
# Load analysis-ready datasets created by Script 03 
# ------------------------------------------------------------ 

adsl_analysis <- read_csv( 
  here("data_processed", "adam_adsl_analysis_ready.csv"), 
  show_col_types = FALSE 
) 

adae_safety <- read_csv( 
  here("data_processed", "adam_adae_safety_ready.csv"), 
  show_col_types = FALSE 
) 

# Define a clear treatment-arm order for tables and figures 

treatment_order <- c( 
  "Placebo", 
  "Xanomeline Low Dose", 
  "Xanomeline High Dose" 
) 

adsl_analysis <- adsl_analysis %>% 
  mutate( 
    actual_treatment = factor( 
      actual_treatment, 
      levels = treatment_order 
    ) 
  ) 

adae_safety <- adae_safety %>% 
  mutate( 
    actual_treatment = factor( 
      actual_treatment, 
      levels = treatment_order 
    ) 
  ) 

# ------------------------------------------------------------ 
# Baseline characteristics summary 
# ------------------------------------------------------------ 

# Treatment-group denominators 

treatment_denominators <- adsl_analysis %>% 
  count( 
    actual_treatment, 
    name = "number_of_subjects" 
  ) 

print(treatment_denominators) 

write_csv( 
  treatment_denominators, 
  here("outputs", "tables", "baseline_treatment_denominators.csv") 
) 

# Continuous baseline characteristics: 
# Report n, mean, standard deviation, median, minimum, and maximum. 

baseline_continuous_summary <- adsl_analysis %>% 
  pivot_longer( 
    cols = c( 
      age, 
      baseline_bmi, 
      baseline_height_cm, 
      baseline_weight_kg 
    ), 
    names_to = "variable", 
    values_to = "value" 
  ) %>% 
  group_by( 
    actual_treatment, 
    variable 
  ) %>% 
  summarise( 
    number_with_nonmissing_value = sum(!is.na(value)), 
    mean = mean(value, na.rm = TRUE), 
    standard_deviation = sd(value, na.rm = TRUE), 
    median = median(value, na.rm = TRUE), 
    minimum = min(value, na.rm = TRUE), 
    maximum = max(value, na.rm = TRUE), 
    .groups = "drop" 
  ) 

print(baseline_continuous_summary) 

write_csv( 
  baseline_continuous_summary, 
  here( 
    "outputs", 
    "tables", 
    "baseline_continuous_characteristics.csv" 
  ) 
) 

# Categorical baseline characteristics: 
# Report count and within-treatment percentage. 

baseline_categorical_summary <- adsl_analysis %>% 
  select( 
    actual_treatment, 
    sex, 
    race, 
    age_group 
  ) %>% 
  pivot_longer( 
    cols = c( 
      sex, 
      race, 
      age_group 
    ), 
    names_to = "variable", 
    values_to = "category" 
  ) %>% 
  count( 
    actual_treatment, 
    variable, 
    category, 
    name = "category_count" 
  ) %>% 
  left_join( 
    treatment_denominators %>% 
      rename( 
        treatment_group_total = number_of_subjects 
      ), 
    by = "actual_treatment" 
  ) %>% 
  mutate( 
    percentage = round( 
      100 * category_count / treatment_group_total, 
      1 
    ) 
  ) %>% 
  select( 
    actual_treatment, 
    variable, 
    category, 
    category_count, 
    percentage 
  ) 


print(baseline_categorical_summary) 

write_csv( 
  baseline_categorical_summary, 
  here( 
    "outputs", 
    "tables", 
    "baseline_categorical_characteristics.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Treatment-emergent adverse-event summaries 
# ------------------------------------------------------------ 

# Keep only treatment-emergent adverse events. 

teae_data <- adae_safety %>% 
  filter( 
    treatment_emergent_flag == "Y" 
  ) 

# Subject-level summary: 
# Number and percentage of subjects with at least one TEAE. 

subjects_with_teae <- teae_data %>% 
  distinct( 
    USUBJID, 
    actual_treatment 
  ) %>% 
  count( 
    actual_treatment, 
    name = "subjects_with_at_least_one_teae" 
  ) 

teae_subject_summary <- treatment_denominators %>% 
  left_join( 
    subjects_with_teae, 
    by = "actual_treatment" 
  ) %>% 
  mutate( 
    subjects_with_at_least_one_teae = 
      replace_na(subjects_with_at_least_one_teae, 0L), 
    percentage_with_at_least_one_teae = round( 
      100 * subjects_with_at_least_one_teae / number_of_subjects, 
      1 
    ) 
  ) 

print(teae_subject_summary) 

write_csv( 
  teae_subject_summary, 
  here( 
    "outputs", 
    "tables", 
    "teae_subject_summary.csv" 
  ) 
) 

# Serious treatment-emergent adverse-event summary. 
# AESER = "Y" indicates a serious adverse event. 

serious_teae_subjects <- teae_data %>% 
  filter( 
    serious_event_flag == "Y" 
  ) %>% 
  distinct( 
    USUBJID, 
    actual_treatment 
  ) %>% 
  count( 
    actual_treatment, 
    name = "subjects_with_serious_teae" 
  ) 

serious_teae_summary <- treatment_denominators %>% 
  left_join( 
    serious_teae_subjects, 
    by = "actual_treatment" 
  ) %>% 
  mutate( 
    subjects_with_serious_teae = 
      replace_na(subjects_with_serious_teae, 0L), 
    percentage_with_serious_teae = round( 
      100 * subjects_with_serious_teae / number_of_subjects, 
      1 
    ) 
  ) 

print(serious_teae_summary) 

write_csv( 
  serious_teae_summary, 
  here( 
    "outputs", 
    "tables", 
    "serious_teae_subject_summary.csv" 
  ) 
) 

# Severity summary by treatment arm. 
# This counts adverse-event records, not unique subjects. 

teae_severity_summary <- teae_data %>% 
  count( 
    actual_treatment, 
    severity, 
    name = "treatment_emergent_adverse_event_records" 
  ) %>% 
  arrange( 
    actual_treatment, 
    severity 
  ) 

print(teae_severity_summary) 

write_csv( 
  teae_severity_summary, 
  here( 
    "outputs", 
    "tables", 
    "teae_severity_summary.csv" 
  ) 
) 

# Most common treatment-emergent preferred terms. 
# This is a record-level summary. 

top_teae_terms <- teae_data %>% 
  count( 
    actual_treatment, 
    preferred_term, 
    sort = TRUE, 
    name = "treatment_emergent_adverse_event_records" 
  ) %>% 
  group_by(actual_treatment) %>% 
  slice_head(n = 10) %>% 
  ungroup() 

print(top_teae_terms) 

write_csv( 
  top_teae_terms, 
  here( 
    "outputs", 
    "tables", 
    "top_10_teae_preferred_terms_by_treatment.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Figure: percentage of subjects with at least one TEAE 
# ------------------------------------------------------------ 

teae_plot <- ggplot( 
  teae_subject_summary, 
  aes( 
    x = actual_treatment, 
    y = percentage_with_at_least_one_teae, 
    fill = actual_treatment 
  ) 
) + 
  geom_col( 
    show.legend = FALSE 
  ) + 
  geom_text( 
    aes( 
      label = paste0( 
        percentage_with_at_least_one_teae, 
        "%" 
      ) 
    ), 
    vjust = -0.4, 
    size = 4 
  ) + 
  labs( 
    title = "Subjects With at Least One Treatment-Emergent Adverse Event", 
    x = "Actual treatment", 
    y = "Percentage of safety population subjects" 
  ) + 
  theme_minimal( 
    base_size = 12 
  ) + 
  theme( 
    axis.text.x = element_text( 
      angle = 15, 
      hjust = 1 
    ) 
  ) + 
  expand_limits( 
    y = max( 
      teae_subject_summary$percentage_with_at_least_one_teae 
    ) * 1.12 
  ) 

print(teae_plot) 

ggsave( 
  here( 
    "outputs", 
    "figures", 
    "subjects_with_teae_by_treatment.png" 
  ), 
  plot = teae_plot, 
  width = 9, 
  height = 6, 
  dpi = 300 
) 

cat("Baseline and safety summaries completed successfully.\n") 
