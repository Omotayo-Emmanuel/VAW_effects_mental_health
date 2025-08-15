library(dplyr)
library(readr)
# Load the original dataset
data <- read_csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\data\\VAW RGAs_microdata (1).csv", show_col_types = FALSE)

# =============================================================================
# Column Mapping (Original → New names)
# =============================================================================
# DV-related variables
# rC30_grouped        → dv_any_lifetime        : Has experienced or knows someone who has experienced DV in lifetime
# AgeCat              → age_group              : Age group of respondent
# BR_rA01             → marital_status         : Marital status of respondent
# BR_rA02             → education_level        : Education level of respondent
# Earning_income      → earns_income           : Respondent earns own income (Yes/No)
# rA04_1              → employment_status      : Respondent's current employment status
# Disability          → disability_status      : Respondent has a disability (Yes/No)
# Earn_Spouse         → spouse_income_relation : Whether spouse earns more, less, or same as respondent
# Locality            → locality_type          : Urban or rural location
# C21                 → feels_unsafe_home      : Feels unsafe at home (Yes/No)
# C19                 → conflict_frequency     : Frequency of conflicts at home
# rA12                → food_insecurity_score  : Total food insecurity score
# C02                 → feels_safe_day         : Feels safe walking alone during the day
# C03                 → feels_safe_night       : Feels safe walking alone at night
# BR_rrC02_03         → combined_safety_index  : Combined day/night safety measure
# C10                 → dv_common_perception   : Thinks domestic violence is common in community
# C07                 → vaw_problem_perception : Thinks VAW is a problem in the community
# C24                 → mobility_frequency     : Frequency respondent goes out
# C30_1_grouped       → dv_physical            : Experienced/knows someone who experienced physical abuse
# C30_2_grouped       → dv_verbal              : Experienced/knows someone who experienced verbal abuse
# C30_3_grouped       → dv_economic            : Experienced/knows someone who experienced economic abuse
# C30_4_grouped       → dv_sexual              : Experienced/knows someone who experienced sexual harassment
# C30_5_grouped       → dv_other                : Experienced/knows someone who experienced other abuse
# C12                 → will_seek_help_dv      : Would seek help if facing DV
# C13                 → help_source_dv         : Source of help for DV
# C16                 → will_seek_help_harass  : Would seek help if facing sexual harassment
# C17                 → help_source_harass     : Source of help for harassment
# C23_1:C23_11        → unsafe_reasons_*       : Specific reasons for feeling unsafe at home
# cA12                → food_insec_: FAO Food Insecurity Experience Scale items
# =============================================================================

# Select and rename columns
cleaned_data <- data %>%
  select(
    dv_any_lifetime        = rC30_grouped,
    age_group              = AgeCat,
    marital_status         = BR_rA01,
    education_level        = BR_rA02,
    earns_income           = Earning_Income,
    employment_status      = BR_rA03,
    disability_status      = Disability,
    spouse_income_relation = rA04_1,
    locality_type          = rS11,
    feels_unsafe_home      = C21,
    conflict_frequency     = C19,
    food_insecurity_score  = rA12,
    feels_unsafe_day         = C02,
    feels_unsafe_night       = C03,
    combined_safety_index  = BR_rrC02_03,
    dv_common_perception   = C10,
    vaw_problem_perception = C07,
    mobility_frequency     = C24,
    dv_physical            = C30_1_grouped,
    dv_verbal              = C30_2_grouped,
    dv_economic            = C30_3_grouped,
    dv_sexual              = C30_4_grouped,
    dv_other               = C30_5_grouped,
    will_seek_help_dv      = C12,
    help_source_dv         = C13,
    will_seek_help_harass  = C16,
    help_source_harass     = C17,
    unsafe_reason_1        = C23_1,
    unsafe_reason_2        = C23_2,
    unsafe_reason_3        = C23_3,
    unsafe_reason_4        = C23_4,
    unsafe_reason_5        = C23_5,
    unsafe_reason_6        = C23_6,
    unsafe_reason_7        = C23_7,
    unsafe_reason_8        = C23_8,
    unsafe_reason_9        = C23_9,
    unsafe_reason_10       = C23_10,
    unsafe_reason_11       = C23_11,
    food_insec             = cA12,
  ) %>%
   # Confirming demographic column missing values 
  { print(colSums(is.na(.[c("age_group", "marital_status", "education_level",
                            "disability_status", "employment_status", "locality_type",
                            "spouse_income_relation")])) / nrow(.) * 100); . } %>%
  # Remove extra spaces from text
  mutate(across(where(is.character), ~trimws(.))) %>%
  # Remove rows with any NA in key demographic columns
  filter(
  !is.na(marital_status),
  !is.na(education_level),
  !is.na(employment_status)
  ) %>%
  # Convert empty strings to NA
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  # Drop rows missing BOTH DV status and mental health info
  filter(!(is.na(dv_any_lifetime)))

 
  
# Save cleaned file
write_csv(cleaned_data, "Cleaned Domestic Violence Dataset.csv")

cat("Cleaned dataset saved as 'Cleaned Domestic Violence Dataset.csv'\n")
