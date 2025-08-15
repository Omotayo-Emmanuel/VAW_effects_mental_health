# Loading necessary libraries
library(dplyr)
library(tidyverse)
# Creating the metrics needed for the Analysis
# Loading the dataset
df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\data_processed\\Cleaned Domestic Violence Dataset.csv")

# Viewing the dataset
View(df)
summary(df)
str(df)

# Defining the binary Yes/no columns
# These are columns where values are "Yes", "No", or NA.
# We will convert them to numeric:
# Yes → 1, No → 0, NA → remains NA

# Function to convert Yes/No variants to 1/0 and others to NA
convert_yes_no <- function(column) {
  # Convert to character to handle factors issues
    column <- as.character(column)
    # Trim leading and trailing whitespace
    column <- trimws(column)
    # Convert "Yes" to 1, "No" to 0, and all others to NA
    ifelse(grepl("Yes", column, ignore.case = TRUE), 1,
           ifelse(grepl("No", column, ignore.case = TRUE), 0, NA))
}

binary_columns <- c(
    "dv_any_lifetime",          # Has experienced or knows someone who has experienced DV in lifetime
    "earns_income",             # Respondent earns own income (Yes/No)  
    "disability_status",        # Respondent has a disability (Yes/No)  
    "feels_unsafe_home",        # Feels unsafe at home (Yes/No)
    "feels_unsafe_day",           # Feels safe walking alone during the day
    "feels_unsafe_night",         # Feels safe walking alone at night
    "will_seek_help_dv",        # Would seek help if facing DV
    "will_seek_help_harass",    # Would seek help if facing sexual harassment
    "dv_physical",              # Experienced/knows someone who experienced physical abuse
    "dv_verbal",                # Experienced/knows someone who experienced verbal abuse
    "dv_economic",              # Experienced/knows someone who experienced economic abuse
    "dv_sexual",                # Experienced/knows someone who experienced sexual harassment
    "dv_other",                 # Experienced/knows someone who experienced other abuse
    "unsafe_reason_1",         # I have a serious medical condition or disability and feel vulnerable
    "unsafe_reason_2",          #  My shelter is insecure from external threats (e.g., there are no locks on my front door)
    "unsafe_reason_3",          # I am unable to communicate/reach out for help
    "unsafe_reason_4",          # I am living with people I cannot trust
    "unsafe_reason_5",          #  There is substance abuse (e.g., alcohol or drugs) in the household
    "unsafe_reason_6",          # There is physical violence or threats of physical violence in my home
    "unsafe_reason_7",          #  There is verbal abuse in my home
    "unsafe_reason_8",          #  Other adults in the household have hurt me
    "unsafe_reason_9",           # Other women in the household have been hurt
    "unsafe_reason_10",          #  Children in the household have been hurt
    "unsafe_reason_11"           # Additional reasons for feeling unsafe at home
    ) 


# Apply the conversion function to each binary column
norm_df <- df %>%
  
  # Convert all Yes/No style binary variables to numeric (1/0), NA for others
  mutate(across(all_of(binary_columns), convert_yes_no)) %>%
  
  # Start a new mutate block for creating derived and normalized variables
  mutate(
    # Calculate the maximum DV type count across all respondents
    # This will be used to normalize DV_Type_Count later
    max_dv_count = max(rowSums(across(c(
      dv_any_lifetime, dv_physical, dv_verbal,
      dv_economic, dv_sexual, dv_other
    )), na.rm = TRUE), na.rm = TRUE),
    
    # Normalize food insecurity score (assumes max possible = 8)
    Food_Insecurity_Index = food_insec / 8,
    
    # Assign numeric scores to perception of DV commonness
    DV_common_perception_score = case_when(
      dv_common_perception == "Very uncommon" ~ 0,
      dv_common_perception == "Uncommon"      ~ 1,
      dv_common_perception == "Common"        ~ 2,
      dv_common_perception == "Very common"   ~ 3,
      dv_common_perception %in% c("Don't know", "Refused") ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Normalize DV common perception score to range 0–1
    DV_Common_Perception_norm = DV_common_perception_score / 3,
    
    # Assign numeric scores to perception of VAW as a problem
    VAW_Perception_Score = case_when(
      vaw_problem_perception == "Not at all"   ~ 0,
      vaw_problem_perception == "A little bit" ~ 1,
      vaw_problem_perception == "Somewhat"     ~ 2,
      vaw_problem_perception == "A lot"        ~ 3,
      vaw_problem_perception %in% c("Don't know", "Refused") ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Normalize VAW problem perception score to range 0–1
    VAW_Perception_norm = VAW_Perception_Score / 3,
    
    # Create a binary DV exposure flag: 1 if experienced any type of DV, else 0
    DV_Exposure = ifelse(
      dv_any_lifetime == 1 |
      dv_physical == 1 |
      dv_verbal == 1 |
      dv_economic == 1 |
      dv_sexual == 1 |
      dv_other == 1,
      1, 0
    ),
    
    # Count total number of DV types experienced per respondent
    DV_Type_Count = rowSums(across(c(
      dv_any_lifetime, dv_physical, dv_verbal,
      dv_economic, dv_sexual, dv_other
    )), na.rm = TRUE),
    
    # Calculate unsafe score as the average of selected unsafe indicators
    # Includes perception scores and safety reasons
    Unsafe_Score = rowMeans(across(c(
      feels_unsafe_day, feels_unsafe_night,
      unsafe_reason_8, unsafe_reason_11,
      unsafe_reason_9, unsafe_reason_7,
      unsafe_reason_6, unsafe_reason_4,
      unsafe_reason_3, DV_Common_Perception_norm, 
      VAW_Perception_norm
    )), na.rm = TRUE)
  ) %>%
  
  # Switch to row-wise operation for per-respondent calculations
  rowwise() %>%
  
  # Compute composite well-being score
  mutate(
    WellBeing_Score = rowMeans(
      cbind(
        1 - Unsafe_Score,                      # Invert unsafe score (high unsafe → low well-being)
        1 - (DV_Type_Count / max_dv_count),    # Normalize & invert DV type count
        1 - Food_Insecurity_Index              # Invert food insecurity
      ),
      na.rm = TRUE
    ) * ifelse(disability_status == 1, 0.9, 1) # Reduce score by 10% if disabled
  )

# Open the normalized DataFrame in a spreadsheet-like viewer
View(norm_df)



# confirming to transformation and making sure no unexpected values
# Loop through binary columns and print unique values
for (col in binary_columns) {
  cat("\nColumn:", col, "\n")
  print(unique(norm_df[[col]]))
}

unique(norm_df$dv_any_lifetime)
unique(norm_df$vaw_problem_perception)
unique(norm_df$dv_common_perception)


