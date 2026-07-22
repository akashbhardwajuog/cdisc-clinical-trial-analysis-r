# Project: Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
# Author: Akash Bhardwaj 
# Purpose: Perform Kaplan-Meier and Cox proportional-hazards analysis 
# for time to first dermatologic event using ADTTE pilot data 

library(tidyverse) 
library(survival) 
library(survminer) 
library(broom) 
library(here) 

# ------------------------------------------------------------ 
# Load the analysis-ready ADTTE data created by Script 03 
# ------------------------------------------------------------ 

adtte_analysis <- read_csv( 
  here("data_processed", "adam_adtte_analysis_ready.csv"), 
  show_col_types = FALSE 
) 

adsl_analysis <- read_csv( 
  here("data_processed", "adam_adsl_analysis_ready.csv"), 
  show_col_types = FALSE 
) 


# ------------------------------------------------------------ 
# Define the endpoint and prepare analysis dataset 
# ------------------------------------------------------------ 

# This project uses the only available ADTTE endpoint: 
# Time to First Dermatologic Event. 
# 
# In ADTTE: 
# CNSR = 0 means the event occurred. 
# CNSR = 1 means the subject was censored. 

tte_data <- adtte_analysis %>% 
  filter( 
    parameter_code == "TTDE", 
    safety_population_flag == "Y" 
  ) %>% 
  left_join( 
    adsl_analysis %>% 
      select( 
        USUBJID, 
        age, 
        sex 
      ), 
    by = "USUBJID" 
  ) %>% 
  mutate( 
    actual_treatment = factor( 
      actual_treatment, 
      levels = c( 
        "Placebo", 
        "Xanomeline Low Dose", 
        "Xanomeline High Dose" 
      ) 
    ), 
    event = if_else( 
      censoring_indicator == 0, 
      1L, 
      0L 
    ) 
  ) %>% 
  select( 
    USUBJID, 
    actual_treatment, 
    age, 
    sex, 
    analysis_time, 
    event, 
    censoring_indicator, 
    event_description, 
    parameter, 
    parameter_code 
  ) 


# Confirm there is exactly one record per subject for this endpoint 

stopifnot(nrow(tte_data) == n_distinct(tte_data$USUBJID)) 

# Confirm all analysis times are positive 

stopifnot(all(tte_data$analysis_time > 0)) 

# Create an endpoint summary 

endpoint_summary <- tte_data %>% 
  summarise( 
    total_subjects = n(), 
    subjects_with_event = sum(event == 1), 
    censored_subjects = sum(event == 0), 
    median_analysis_time_days = median(analysis_time) 
  ) 

print(endpoint_summary) 

write_csv( 
  endpoint_summary, 
  here( 
    "outputs", 
    "tables", 
    "time_to_event_endpoint_summary.csv" 
  ) 
) 

# Create treatment-specific event and censoring summary 

tte_treatment_summary <- tte_data %>% 
  group_by(actual_treatment) %>% 
  summarise( 
    total_subjects = n(), 
    subjects_with_event = sum(event == 1), 
    percentage_with_event = round( 
      100 * subjects_with_event / total_subjects, 
      1 
    ), 
    censored_subjects = sum(event == 0), 
    median_analysis_time_days = median(analysis_time), 
    .groups = "drop" 
  ) 

print(tte_treatment_summary) 

write_csv( 
  tte_treatment_summary, 
  here( 
    "outputs", 
    "tables", 
    "time_to_event_summary_by_treatment.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Kaplan-Meier analysis 
# ------------------------------------------------------------ 

survival_object <- Surv( 
  time = tte_data$analysis_time, 
  event = tte_data$event 
) 

km_fit <- survfit( 
  survival_object ~ actual_treatment, 
  data = tte_data 
) 

print(summary(km_fit)) 

# Save Kaplan-Meier curve 

km_plot <- ggsurvplot( 
  km_fit, 
  data = tte_data, 
  risk.table = TRUE, 
  pval = TRUE, 
  conf.int = TRUE, 
  xlab = "Time from analysis start (days)", 
  ylab = "Probability of remaining free of dermatologic event", 
  title = "Kaplan-Meier Curve: Time to First Dermatologic Event", 
  legend.title = "Actual treatment", 
  legend.labs = c( 
    "Placebo", 
    "Xanomeline Low Dose", 
    "Xanomeline High Dose" 
  ), 
  break.time.by = 30, 
  risk.table.height = 0.25, 
  ggtheme = theme_minimal(base_size = 12) 
) 

print(km_plot) 

ggsave( 
  here( 
    "outputs", 
    "figures", 
    "kaplan_meier_time_to_first_dermatologic_event.png" 
  ), 
  plot = km_plot$plot, 
  width = 10, 
  height = 7, 
  dpi = 300 
) 

# Save the risk table separately 

ggsave( 
  here( 
    "outputs", 
    "figures", 
    "kaplan_meier_risk_table.png" 
  ), 
  plot = km_plot$table, 
  width = 10, 
  height = 3, 
  dpi = 300 
) 

# ------------------------------------------------------------ 
# Log-rank test 
# ------------------------------------------------------------ 

log_rank_test <- survdiff( 
  survival_object ~ actual_treatment, 
  data = tte_data 
) 

log_rank_p_value <- 1 - pchisq( 
  log_rank_test$chisq, 
  df = length(log_rank_test$n) - 1 
) 

log_rank_summary <- tibble( 
  test = "Log-rank test", 
  chi_squared = log_rank_test$chisq, 
  degrees_freedom = length(log_rank_test$n) - 1, 
  p_value = log_rank_p_value 
) 

print(log_rank_summary) 

write_csv( 
  log_rank_summary, 
  here( 
    "outputs", 
    "tables", 
    "log_rank_test_summary.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Cox proportional-hazards models 
# ------------------------------------------------------------ 

# Unadjusted model: treatment only. 
# Placebo is the reference treatment group. 

cox_unadjusted <- coxph( 
  survival_object ~ actual_treatment, 
  data = tte_data 
) 

# Adjusted model: 
# treatment, age, and sex are included as covariates. 
# This is an exploratory model, not a confirmatory clinical analysis. 

cox_adjusted <- coxph( 
  survival_object ~ actual_treatment + age + sex, 
  data = tte_data 
) 

print(summary(cox_unadjusted)) 
print(summary(cox_adjusted)) 

# Create a tidy table of adjusted hazard ratios. 
# exp(estimate) converts log hazard ratios to hazard ratios. 

cox_adjusted_results <- tidy( 
  cox_adjusted, 
  exponentiate = TRUE, 
  conf.int = TRUE 
) %>% 
  mutate( 
    model = "Adjusted Cox proportional-hazards model" 
  ) %>% 
  select( 
    model, 
    term, 
    estimate, 
    conf.low, 
    conf.high, 
    p.value 
  ) %>% 
  rename( 
    hazard_ratio = estimate, 
    confidence_interval_lower = conf.low, 
    confidence_interval_upper = conf.high, 
    p_value = p.value 
  ) 

print(cox_adjusted_results) 

write_csv( 
  cox_adjusted_results, 
  here( 
    "outputs", 
    "tables", 
    "adjusted_cox_hazard_ratios.csv" 
  ) 
) 

# ------------------------------------------------------------ 
# Proportional-hazards assumption check 
# ------------------------------------------------------------ 

ph_test <- cox.zph(cox_adjusted) 

print(ph_test) 

ph_test_table <- as.data.frame(ph_test$table) %>% 
  rownames_to_column("term") %>% 
  as_tibble() %>% 
  rename( 
    chi_squared = chisq, 
    degrees_freedom = df, 
    p_value = p 
  ) 

write_csv( 
  ph_test_table, 
  here( 
    "outputs", 
    "tables", 
    "cox_proportional_hazards_assumption_check.csv" 
  ) 
) 

# Save Schoenfeld residual plots. 
# These help inspect proportional-hazards assumptions visually. 

png( 
  filename = here( 
    "outputs", 
    "figures", 
    "cox_schoenfeld_residual_plots.png" 
  ), 
  width = 1800, 
  height = 1400, 
  res = 200 
) 

plot(ph_test) 

dev.off() 

# ------------------------------------------------------------ 
# Forest plot of adjusted hazard ratios 
# ------------------------------------------------------------ 

cox_plot_data <- cox_adjusted_results %>% 
  mutate( 
    variable = case_when( 
      term == "actual_treatmentXanomeline Low Dose" ~ 
        "Xanomeline Low Dose vs Placebo", 
      
      term == "actual_treatmentXanomeline High Dose" ~ 
        "Xanomeline High Dose vs Placebo", 
      
      term == "age" ~ 
        "Age (per year increase)", 
      
      term == "sexM" ~ 
        "Male vs Female", 
      
      TRUE ~ term 
    ) 
  ) 

cox_forest_plot <- ggplot( 
  cox_plot_data, 
  aes( 
    x = hazard_ratio, 
    y = reorder(variable, hazard_ratio) 
  ) 
) + 
  geom_vline( 
    xintercept = 1, 
    linetype = "dashed", 
    colour = "grey40" 
  ) + 
  geom_errorbarh( 
    aes( 
      xmin = confidence_interval_lower, 
      xmax = confidence_interval_upper 
    ), 
    height = 0.2 
  ) + 
  geom_point( 
    size = 3, 
    colour = "#0072B2" 
  ) + 
  scale_x_log10() + 
  labs( 
    title = "Adjusted Cox Model: Hazard Ratios for First Dermatologic Event", 
    x = "Hazard ratio, log scale", 
    y = NULL 
  ) + 
  theme_minimal(base_size = 12) 

print(cox_forest_plot) 

ggsave( 
  here( 
    "outputs", 
    "figures", 
    "adjusted_cox_hazard_ratio_forest_plot.png" 
  ), 
  plot = cox_forest_plot, 
  width = 9, 
  height = 5, 
  dpi = 300 
) 

# ------------------------------------------------------------ 
# Save analysis-ready time-to-event dataset 
# ------------------------------------------------------------ 

write_csv( 
  tte_data, 
  here( 
    "data_processed", 
    "time_to_first_dermatologic_event_analysis.csv" 
  ) 
) 

cat("Time-to-event analysis completed successfully.\n") 
