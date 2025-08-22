# Effect of Domestic Violence on Women's Mental Health

## Overview
This project analyzes domestic violence (DV) survey data to understand prevalence rates, impacts on safety and well-being, mediating factors, and differential effects across demographic groups. The analysis provides insights into how domestic violence affects women's safety perceptions, mental health, and help-seeking behaviors.

## Project Structure
```
project-root/
│
├── data/                    
│   └── VAW RGAs_microdata (1).csv
│
├── data_processed/          # Processed data files
│   ├── Cleaned Domestic Violence Dataset.csv
│   └── Normalized_DV_Dataset.csv
│
├── analysis_results/        # Analysis output files
│   ├── Q1_DV_by_Age.csv
│   ├── Q1_DV_by_Education.csv
│   ├── Q1_DV_by_Marital.csv
│   ├── Q2_DV_vs_Safety.csv
│   ├── Q3_DVType_Safety_WellBeing.csv
│   ├── Q4_Mediation_Dataset.csv
│   ├── Q5_HelpType_Moderation.csv
│   ├── Q6_DV_vs_NonDV_Outcomes.csv
│   ├── Q7_Disability_DV_WellBeing.csv
│   ├── mediation_model.RData
│   └── model_safety.RData
│
├── plots/                   # Generated visualizations
│   └── (multiple PNG files)
│
├── scripts/                 # Analysis scripts
│   ├── data_cleaning.r
│   ├── data_normalization.r
│   ├── analysis_questions.r
│   ├── visualization.r
│   └── dashboard.r
│
└── README.md
```

## Methodology

### 1. Data Cleaning (`data_cleaning.r`)
- Loaded raw survey data with 70+ variables related to domestic violence
- Selected and renamed key variables for analysis
- Handled missing values in demographic columns
- Removed empty strings and converted them to NA
- Filtered out rows missing both DV status and mental health information
- Saved cleaned dataset for further processing

### 2. Data Normalization (`data_normalization.r`)
- Converted Yes/No binary variables to numeric (1/0)
- Recoded ordered safety categories to 0-1 scale (0 = Very safe, 1 = Not safe at all)
- Created derived variables:
  - Food Insecurity Index (normalized to 0-1)
  - DV common perception score (normalized)
  - VAW problem perception score (normalized)
  - DV Exposure binary flag
  - DV Type Count (number of DV types experienced)
  - Unsafe Score (composite measure)
  - WellBeing Score (composite measure incorporating safety, DV exposure, and food security)

### 3. Analytical Questions (`analysis_questions.r`)
Seven key research questions were addressed:

**Q1: DV Prevalence by Demographics**
- Calculated overall DV exposure prevalence (57.98%)
- Examined variation by age, marital status, and education level
- Used chi-square tests for significance

**Q2: DV Exposure and Safety Perceptions**
- Compared safety scores between DV-exposed and non-exposed groups
- Used Welch's t-test and Wilcoxon rank-sum test
- Calculated Hedges' g effect size

**Q3: Differential Impact of DV Types**
- MANOVA to test effect of DV type on safety and well-being
- Post-hoc pairwise comparisons with Bonferroni correction
- Eta-squared effect sizes

**Q4: Conflict Frequency as Mediator**
- Mediation analysis using the mediation package
- Tested if frequent conflicts mediate DV exposure → safety perception relationship
- Bootstrapping with 2000 simulations

**Q5: Help-Seeking as Moderator**
- Logistic regression for safety perceptions
- Linear regression for mobility frequency
- Tested interaction effects between DV exposure and help type

**Q6: Mental Health Strain Indicators**
- Compared food insecurity and safety perceptions between survivors and non-survivors
- T-tests and non-parametric alternatives

**Q7: Disability as Risk Factor**
- Chi-square test for DV exposure by disability status
- Logistic regression for odds ratio
- Compared well-being scores between groups

### 4. Visualization (`visualization.r`)
Created comprehensive visualizations for each research question:
- Bar charts for prevalence rates
- Box plots and density plots for group comparisons
- Mediation pathway diagrams
- Interaction effect plots
- Composite indicator visualizations

### 5. Dashboard (`dashboard.r`)
Interactive Shiny dashboard with:
- Seven tabs corresponding to research questions
- Dynamic plots with consistent styling
- Key findings summaries for each analysis
- Interactive exploration of results

## Key Findings

1. **Overall Prevalence**: 57.98% of women reported exposure to or knowledge of DV
2. **Demographic Patterns**: Highest prevalence among younger women (18-29), those with less than primary education, and separated/divorced individuals
3. **Safety Impact**: DV exposure associated with significantly higher unsafe scores (0.450 vs 0.326, p < .001)
4. **DV Types**: Physical and sexual harassment associated with highest unsafe scores
5. **Mediation**: Frequent conflicts mediate ~13% of the DV exposure → safety perception relationship
6. **Mental Health**: Survivors reported higher food insecurity (0.413 vs 0.250) and feeling unsafe (0.450 vs 0.326)
7. **Disability Risk**: Women with disabilities had higher DV exposure (61.4% vs 57.1%) and lower well-being (0.546 vs 0.643)

## Installation and Usage

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Required Packages
```r
install.packages(c(
  "dplyr", "tidyr", "tidyverse", "effsize", "effectsize", 
  "lavaan", "mediation", "broom", "jtools", "ggplot2", 
  "scales", "effects", "shiny", "shinydashboard"
))
```

### Running the Analysis
1. Place the raw data file in the `data/` directory
2. Run scripts in order:
   - `data_cleaning.r`
   - `data_normalization.r`
   - `analysis_questions.r`
   - `visualization.r`
3. Launch the dashboard with:
   ```r
   shiny::runApp("dashboard.r")
   ```

## Limitations
- Cross-sectional data limits causal inferences
- Self-reported data may be subject to recall and social desirability biases
- Missing data handled with complete-case analysis
- Disability measure was binary without granularity on type or severity

## Future Work
- Longitudinal analysis to establish causality
- Multi-level modeling to account for community-level effects
- Qualitative follow-up to contextualize quantitative findings
- Policy impact evaluation of interventions based on these findings

## Contact
For questions about this analysis, please contact the project maintainers.

## License
This project is for research purposes. Data use should comply with original survey terms and ethical guidelines for domestic violence research.
