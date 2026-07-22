# Clinical Trial Safety and Time-to-Event Analysis Using CDISC SDTM/ADaM Pilot Data 
 
## Project objective 
 
This educational project uses public CDISC SDTM/ADaM pilot-study material to demonstrate a reproducible clinical-data analysis workflow in R. 
 
The workflow inspects SDTM-style tabulation datasets and ADaM-style analysis datasets, performs baseline and safety summaries, assesses cross-domain traceability, and conducts time-to-event analysis using Kaplan–Meier curves and Cox proportional-hazards regression. 

## Kaplan–Meier overview ![Kaplan–Meier curve for time to first dermatologic event](outputs/figures/kaplan_meier_time_to_first_dermatologic_event.png)
 
## Dataset source 
 
The project uses the public CDISC SDTM/ADaM Pilot Project package. 
 
Selected datasets include: 
 
- SDTM `DM`: demographics 
- SDTM `AE`: adverse events 
- ADaM `ADSL`: subject-level analysis data 
- ADaM `ADAE`: adverse-event analysis data 
- ADaM `ADTTE`: time-to-event analysis data 
 
The source package is public pilot material for educational and demonstration purposes. It is not a sponsor clinical-trial database, a validated SDTM/ADaM implementation, or a regulatory submission. 
 
## Analysis population 
 
The ADaM safety population included 254 subjects: 
 
| Treatment | Subjects | 
|---|---:| 
| Placebo | 86 | 
| Xanomeline Low Dose | 84 | 
| Xanomeline High Dose | 84 | 
 
Two selected baseline variables had one missing value each: 
 
- Baseline BMI 
- Baseline weight 
 
## Analysis workflow 
 
1. Downloaded the public CDISC SDTM/ADaM Pilot Project source package. 
2. Inspected selected SDTM and ADaM SAS Transport datasets. 
3. Created a dataset and variable inventory. 
4. Defined analysis-ready extracts from ADSL, ADAE, and ADTTE. 
5. Summarised baseline demographic and clinical characteristics. 
6. Identified treatment-emergent adverse events using `TRTEMFL = "Y"`. 
7. Created descriptive adverse-event summaries by actual treatment. 
8. Analysed time to first dermatologic event using Kaplan–Meier methods. 
9. Performed a log-rank test and adjusted Cox proportional-hazards regression. 
10. Checked proportional-hazards assumptions using Schoenfeld residuals. 
11. Performed SDTM-to-ADaM subject coverage, record traceability, and treatment-alignment checks. 
12. Documented treatment mismatches for contextual review. 
 
## Key safety results 
 
| Treatment | Subjects with at least one TEAE | Percentage | 
|---|---:|---:| 
| Placebo | 65 / 86 | 75.6% | 
| Xanomeline Low Dose | 77 / 84 | 91.7% | 
| Xanomeline High Dose | 76 / 84 | 90.5% | 
 
Serious treatment-emergent adverse events were uncommon: 
 
- Placebo: 0 subjects 
- Xanomeline Low Dose: 1 subject (1.2%) 
- Xanomeline High Dose: 2 subjects (2.4%) 
 
### Treatment-emergent adverse events 
 
![Subjects with at least one TEAE](outputs/figures/subjects_with_teae_by_treatment.png) 
 
These are descriptive summaries of public pilot data and do not establish causal safety effects. 
 
## Time-to-event analysis 
 
The ADTTE endpoint was: 
 
```text 
Time to First Dermatologic Event 
 

Analysis subjects: 254 

Subjects with a dermatologic event: 152 

Censored subjects: 102 

Overall median analysis time: 40 days 

Kaplan–Meier analysis 

The log-rank test showed differences in time to first dermatologic event across treatment groups: 

Chi-squared: 60.3 
Degrees of freedom: 2 
p-value: 8.18e-14 
 

Adjusted Cox model 

The exploratory Cox model adjusted for age and sex, with placebo as the reference group. 

Variable 

Hazard ratio 

95% confidence interval 

p-value 

Xanomeline Low Dose vs Placebo 

4.47 

2.84 to 7.06 

<0.001 

Xanomeline High Dose vs Placebo 

5.13 

3.24 to 8.13 

<0.001 

Age, per year 

0.99 

0.97 to 1.00 

0.118 

Male vs Female 

1.46 

1.06 to 2.02 

0.021 

The global Schoenfeld-residual test had a p-value of 0.75, so the proportional-hazards assumption was reasonably supported for this exploratory model. 

Cross-domain traceability results 

Subject coverage 

All selected subject links matched: 

254 of 254 ADSL safety-population subjects were represented in SDTM DM. 

225 of 225 ADAE subjects were represented in SDTM AE. 

254 of 254 ADTTE subjects were represented in ADSL. 

ADAE-to-AE traceability 

All 1,191 ADAE records linked to an SDTM AE source record through USUBJID and AESEQ. 

Linked records: 1,191 

Unlinked records: 0 

Preferred-term matches: 1,191 

Severity matches: 1,191 

ADTTE-to-AE traceability 

All 152 ADTTE dermatologic-event records linked to an SDTM AE source record using USUBJID and the source sequence. 

Treatment alignment 

Planned treatment matched between ADSL and DM for all 254 safety-population subjects. 

Actual treatment matched for 242 subjects. 

Twelve subjects had different actual-treatment values between ADSL and DM. 

All 12 mismatched subjects were planned for and recorded in ADSL as Xanomeline High Dose, while SDTM DM listed Xanomeline Low Dose as the actual arm. All 12 had discontinued treatment; reasons included adverse events, subject withdrawal, protocol violation, physician decision, and sponsor study termination. 

This demonstrates why treatment variables from different clinical datasets require documented derivation rules and contextual review. 

Project structure 

08_cdisc_clinical_trial_analysis/ 
├── data_raw/ 
│   └── cdisc_pilot_source/          # local official CDISC source package; ignored by Git 
├── data_processed/ 
├── scripts/ 
├── outputs/ 
│   ├── figures/ 
│   └── tables/ 
├── reports/ 
├── references/ 
├── README.md 
└── .gitignore 
 

Reproducibility 

Download the CDISC source package locally: 

git clone https://github.com/cdisc-org/sdtm-adam-pilot-project.git data_raw/cdisc_pilot_source 
 

Then run the R scripts in this order: 

scripts/01_install_packages.R 
scripts/02_inspect_cdisc_pilot_data.R 
scripts/03_profile_adam_analysis_data.R 
scripts/04_baseline_and_safety_summaries.R 
scripts/05_time_to_event_analysis.R 
scripts/06_cross_domain_traceability_checks.R 
scripts/07_review_treatment_alignment.R 
 

Limitations 

The project uses public CDISC pilot material rather than a real sponsor clinical-study database. 

The analysis is educational and does not represent validated SDTM/ADaM programming or a regulatory submission. 

No formal Statistical Analysis Plan, define.xml review, controlled-terminology validation, or independent programming validation was performed. 

Safety and time-to-event analyses are descriptive or exploratory. 

Results must not be used for clinical, regulatory, or treatment decisions. 

Technical skills demonstrated 

R and RStudio 

Clinical-data analysis 

SDTM and ADaM awareness 

SAS Transport (.xpt) data import using haven 

Subject-level analysis datasets 

Baseline-characteristics summaries 

Treatment-emergent adverse-event summaries 

Kaplan–Meier analysis 

Log-rank testing 

Cox proportional-hazards regression 

Hazard-ratio interpretation 

Schoenfeld-residual diagnostics 

SDTM-to-ADaM traceability checks 

Cross-domain data-quality checks 

Quarto reporting 

Git and GitHub 