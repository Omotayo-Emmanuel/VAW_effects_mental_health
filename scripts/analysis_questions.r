# Load Libraries
library(dplyr)
library(tidyr)
library(tidyverse)
chooseCRANmirror()
install.packages("effsize")
library(effsize)
install.packages("effectsize")
library(effectsize)
install.packages("lavaan")
library(lavaan)
install.packages("mediation")
install.packages("colorspace")
install.packages("data.table")
install.packages("Rcpp")
library(mediation)
library(broom)       # to get tidy(), glance(), augment() results
install.packages("jtools")
library(jtools)      # optional, nice summaries with interpretation

# Load the cleaned dataset
df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\data_processed\\Normalized_DV_ Dataset.csv")

# View the dataset
View(df)
str(df)
unique(df$DV_)
# Analysis Question 1
# What percentage of women have experienced or know someone who has experienced 
# any form of domestic violence (rC30), Does this percentage vary significantly by 
# age group, marital status, or education level (AgeCat, BR_rA01, BR_rA02)?

# Calculation the overall prvalenc of DV exposure
mean(df$DV_Exposure, na.rm = TRUE) * 100 #giving us the percentage of women exposd to DV

# Categorical/NA-containing (best for prop.table):
prop.table(table(df$DV_Exposure, useNA = "ifany")) * 100  # Full breakdown

# Comparing how often DV_Exposure occurs across different groups.
# This is for age group, marital status, and education level.
df %>%
    group_by(age_group) %>% #Groups your data by age group
    summarise(
        # Calculates the average DV_Exposure for each age group (ignoring missing values)
        prevalence = mean(DV_Exposure, na.rm = TRUE) * 100, # Multiplies it by 100 to express it as a percentage (i.e., prevalence)
        count = n() # Counts how many people are in each age group
    )

df %>%
  group_by(marital_status) %>% #
  summarise(
    prevalence = mean(DV_Exposure, na.rm = TRUE) * 100,
    count = n()
  )
df %>%
  group_by(education_level) %>%
  summarise(
    prevalence =  mean(DV_Exposure, na.rm = TRUE) * 100,
    count = n()
  )

# Test for significant differences in DV_Exposure across groups
chisq.test(table(df$DV_Exposure, df$age_group))
chisq.test(table(df$DV_Exposure, df$marital_status))
chisq.test(table(df$DV_Exposure, df$education_level))

# Analysis Question 2
# Is there a significant relationship between experiencing domestic violence 
# (DV exposure) and feeling unsafe (Safety Score)?

# Quick look at the variables
table(df$DV_Exposure)      # frequency of DV exposure
summary(df$Unsafe_Score, na.rm = TRUE)   # distribution of safety score
# Creating a small dataset for the needed variables and remove rows with NA
df_aly_2 <- df %>%
  select(DV_Exposure, Unsafe_Score) %>%
  filter(!is.na(DV_Exposure), !is.na(Unsafe_Score)) %>%
  mutate(
    # Ensuring grouping is a factor with clear labels
    DV_Exposure = factor(DV_Exposure, levels = c(0, 1), labels = c("No DV", "DV"))
  )

# A count check 
cat("N (complete cases) =", nrow(df_aly_2), "\n")
print(table(df_aly_2$DV_Exposure))

# Getting their group summaries
group_summary <- df_aly_2 %>%
  group_by(DV_Exposure) %>%
  summarise(
    mean_unsafe = mean(Unsafe_Score, na.rm = TRUE), # Mean safety score for each group
    sd_safety = sd(Unsafe_Score, na.rm = TRUE), # Standard deviation(spread) of safety score
    count = n(), # Counts how many people are in each group
    .groups = 'drop' # Avoids unnecessary grouping in the output
  )

cat("\n--- Group summary (Unsafe_Score by DV_Exposure) ---\n")
print(group_summary)

# Extrating the means for quick difference display
mean_no <- group_summary$mean_unsafe[group_summary$DV_Exposure == "No DV"]
mean_dv <- group_summary$mean_unsafe[group_summary$DV_Exposure == "DV"]
cat(sprintf("\nMean Unsafe_Score: DV = %.3f, No DV = %.3f (diff = %.3f)\n",
            mean_dv, mean_no, mean_dv - mean_no))

# Running a variance check (F-test) to check if the spread (variance) of Unsafe_Score is similar between the two groups.
# this check helps decide whether you should use a Welch t‑test (more robust to unequal variance).
var_test <- var.test(Unsafe_Score ~ DV_Exposure, data = df_aly_2)
cat("\n--- Variance test (F) ---\n")
print(var_test)

# Since the variances are not equal, we will use the Welch t-test for comparing 

# Hypothesis tests
# Welch's t-test (robust to unequal variances; preferred default)
t_welch   <- t.test(Unsafe_Score ~ DV_Exposure, data = df_aly_2, var.equal = FALSE)

# Non-parametric robustness check
# Wilcoxon test confirms the difference holds even when not relying on normality assumptions
w_test <- wilcox.test(Unsafe_Score ~ DV_Exposure, data = df_aly_2, exact = FALSE)

cat("\n--- Welch t-test (default) ---\n");   print(t_welch)
cat("\n--- Wilcoxon rank-sum (robustness) ---\n");   print(w_test)

# Hedges g effect size
# Hedges g is a measure of effect size that corrects for small sample bias
# Run Hedges' g (similar to Cohen's d but corrected for bias in small samples)
g_effect <- cohen.d(Unsafe_Score ~ DV_Exposure, 
                    data = df_aly_2, 
                    hedges.correction = TRUE)

# Print result
cat("\n--- Effect size: Hedges' g (DV vs No DV) ---\n")
print(g_effect)


# Compact one-line takeaway (Welch t-test)
cat(sprintf(
  "\nTakeaway: Mean Unsafe_Score is %.3f for DV vs %.3f for No DV (diff = %.3f).\nWelch t-test: t = %.2f, df = %.1f, p = %.3g; Hedges' g = %.2f.\n",
  mean_dv, mean_no, mean_dv - mean_no,
  unname(t_welch$statistic), unname(t_welch$parameter), t_welch$p.value,
  unname(g_effect$estimate)
))

# Question 3
#Does the type of domestic violence (physical, verbal, denial of needs, 
# denial of communication, sexual harassment) differentially impact feelings of safety 
# and well-being scores?
# Correlation check between Unsafe and WellBeing 
cor_test <- cor.test(df$Unsafe_Score, df$WellBeing_Score, na.rm = TRUE)
print(cor_test)

# Since it is correlated witha value of -0.586 i.e., Moderately strong 
# We'll be proceeding with MANOVA
# Fitting the MANOVA model
# DV_Type = independent variable
# Unsafe_Score and WellBeing_Score = dependent variables

# Had to create a variable column for DV_Type
# reshape to long
df_long <- df %>%
  pivot_longer(
    cols = c(dv_physical, dv_verbal, dv_economic, dv_other, dv_sexual),
    names_to = "DV_Type",
    values_to = "Exposure"
  ) %>%
  filter(Exposure == 1) %>%   # keep only types actually experienced
  select(-Exposure)

# Make DV_Type a factor with nice labels
df_long$DV_Type <- factor(df_long$DV_Type,
                          levels = c("dv_physical","dv_verbal","dv_economic",
                                     "dv_other","dv_sexual"),
                          labels = c("Physical","Verbal","Denial_of_Needs",
                                     "Denial_of_Communication","Sexual_Harassment"))
manova_model <- manova(cbind(Unsafe_Score, WellBeing_Score) ~ DV_Type, data = df_long)
# Summary using different test statistics (Wilks, Pillai, Hotelling, Roy) ---
summary(manova_model, test = "Wilks")
summary(manova_model, test = "Pillai")
summary(manova_model, test = "Hotelling-Lawley")
summary(manova_model, test = "Roy")

summary.aov(manova_model)

#  Post-hoc tests (pairwise comparisons) ---
# For Unsafe_Score
pairwise_unsafe <- pairwise.t.test(df_long$Unsafe_Score, df_long$DV_Type, p.adjust.method = "bonferroni")
print(pairwise_unsafe)

# For WellBeing_Score
pairwise_wellbeing <- pairwise.t.test(df_long$WellBeing_Score, df_long$DV_Type, p.adjust.method = "bonferroni")
print(pairwise_wellbeing)

# We have the significant differences in DV_Type on both Unsafe_Score and WellBeing_Score.
# To get the sense of how much variance in Unsafe and WellBeing is explained by DV_Type, we can calculate the effect size using eta-squared
eta_squared(aov(Unsafe_Score ~ DV_Type, data = df_long), partial = TRUE)
eta_squared(aov(WellBeing_Score ~ DV_Type, data = df_long), partial = TRUE)

# Question 4
#Do frequent conflicts at home (C19) mediate the relationship between domestic violence exposure 
#and safety perceptions (C02, C03, C21)?

# Data Exploration of the conflict column
unique(df$conflict_frequency)
# Result 
# [1] NA              "Never"         "Daily"         "Refused"      
# [5] "Once or twice" "Monthly"       "Weekly"        "Don't know"   
ls(df_med)
ls(df)
# Lets Prep the Data
# Convert conflict_frequency to a factor with ordered levels and drop DK/Refused as NA
View(df)
unique(df$DV_Exposure)
df_med <- df %>%
  mutate(
    conflict_frequency = case_when(
      conflict_frequency %in% c("Don't know", "Refused") ~ NA_character_, # Convert DK/Refused to NA
      TRUE ~ as.character(conflict_frequency) # Keep other values as is
    ),

    # Convert to ordered factor with meaningful levels
    # R will otherwise treat the values as plain text and sort them alphabetically 
    # (e.g., “Daily, Monthly, Never, Once or twice, Weekly”), which is not the real frequency order. 
    # Setting the levels explicitly locks in the correct order for summaries, plots, and modeling.

    conflict_frequency = factor(
      conflict_frequency,
      levels = c("Never", "Once or twice", "Monthly", "Weekly", "Daily"),
      ordered = TRUE
  )
) 
# Create a single analysis dataset and drop rows with any NA we care about
df_clean <- df_med %>%
  dplyr::select(
    DV_Exposure,
    conflict_frequency, # The mediator variable
    Unsafe_Score,
    age_group, education_level, marital_status
  ) %>%
  tidyr::drop_na() %>%
  mutate(
    age_group       = as.factor(age_group),
    education_level = as.factor(education_level),
    marital_status  = as.factor(marital_status),
     # numeric mediator column for mediate()
    conflict_frequency_num = as.numeric(conflict_frequency)
  )


 # Using the Mediation package to test mediation
 # Model how DV_Exposure affects conflict_frequency
 med_model <- lm(conflict_frequency_num ~ DV_Exposure + age_group + education_level+  marital_status, data = df_clean)

# Outcome model
# Model how DV exposure and conflict frequency predict safety perception
out_model <- lm(Unsafe_Score ~DV_Exposure + conflict_frequency_num + age_group + education_level + marital_status, data = df_clean)

# Mediation analysis
set.seed(123)
med_out <- mediate(
  model.m = med_model,   # mediator model
  model.y = out_model,   # outcome model
  treat   = "DV_Exposure", 
  mediator= "conflict_frequency_num",
  boot    = TRUE,        # bootstrap for robust inference
  sims    = 2000
)

nobs(med_model)
nobs(out_model)

# Save the mediation output object
save(med_out, file = "analysis_results/mediation_model.RData")
summary(med_out)

#What the numbers told us:
#	 Yes, there is a mediation effect — about 13% of the impact of DV exposure on feeling unsafe is explained by more frequent conflicts at home.
#	 The other 87% is a direct effect of DV exposure on feeling unsafe, unrelated to the conflict measure.
#	 Both the indirect path (DV → Conflicts → Safety) and the direct path (DV → Safety) were statistically significant.

# Question 5
# Does seeking formal or informal help (C13, C17) moderate the effect of domestic violence 
# on women’s safety perceptions and mobility (C24)?

# Data Exploration on the required columns
unique(df$mobility_frequency)
unique(df$will_seek_help_dv)
unique(df$help_source_harass)
unique(df$help_source_dv)
unique(df$will_seek_help_harass)

# DATA PREP
# Classify the help sorce from dv into formal and informal
# Formal help sources: police, health facility, helpline, women’s centres, shelters, NGOs/CSOs.
# Informal help sources: family, friends, community leaders, religious leaders.

df_mod <- df %>%
  mutate(
    # Convert help source to factor with meaningful levels
    help_type_dv = case_when(
      help_source_dv %in% c("Call/go to police", "Go to health facility", "Call helpline",
                            "Access to women's centres", "Seek help from shelter or safehouse for women",
                            "Seeking support from women's groups/NGOs/CSOs") ~ "Formal",
      help_source_dv %in% c("Seek support from family", "Talk with friends for support or guidance",
                            "Seek support from a religious leader", "Approach community leaders for support") ~ "Informal",
      TRUE ~ "Other/NA"
    ),
      mobility_num = case_when(
        mobility_frequency == "Never" ~ 0,
        mobility_frequency == "Once or twice a month" ~ 1,
        mobility_frequency == "Once a week"  ~ 2,
        mobility_frequency == "2-3 times per week" ~ 3,
        mobility_frequency == "Daily" ~ 4,
        TRUE ~ NA_real_
    )
  )

# Check the distribution of help types
table(df_mod$help_type_dv, useNA = "ifany")
# Testing whether the help type interacts with DV_Exposure in predicting saftey perceptions and mobility
# Since we have two outcomes we'll run two separate models
# Logistic regression -> Domestic violence × Help type on safety perceptions.
# Linear regression -> Domestic violence × Help type on mobility frequency.

# Logistic regression: Does help type moderate DV effect on safety?
# Fit a logistic regression to test moderation (interaction) 
df_mod$unsafe_binary <- as.integer(df_mod$Unsafe_Score > 0)
model_safety <- glm(
  unsafe_binary ~ DV_Exposure * help_type_dv,
  data = df_mod,
  family = binomial(link = "logit")
)

# Show coefficients, standard errors, z-tests, p-values, and model fit
summary(model_safety)
# Linear regression: Does help type moderate DV effect on mobility?
model_mobility <- lm(
  mobility_num ~ DV_Exposure * help_type_dv,
  data = df_mod
)

summary(model_mobility)
# 
exp(coef(model_safety))   # Odds ratios
confint(model_safety)     # 95% CI for odds ratios

save(model_safety, file = "analysis_results/model_safety.RData")
# QUESTION 6
#  Do women exposed to domestic violence report higher levels of mental health strain indicators
#  — such as food insecurity (rA12) and feeling unsafe walking alone during the day/night (C02, C03, BR_rrC02_03)
#  — compared to non-survivors?

# DATA Exploration
unique(df$food_insecurity_score)
unique(df$food_insec)
unique(df$Food_Insecurity_Index)
unique(df$DV_Exposure)
unique(df$Unsafe_Score)

# DATA Preparation
# Define groups (DV_exposed vs Not_exposed)
df_comp <- df%>%
  mutate(
    DV_group = ifelse(DV_Exposure == 1, "survivors", "non-survivors")

  )
table(df_comp$DV_group)
# Check distributions of outcomes ---
# Food insecurity index
# Taking a random sample of 5000 without NAs
shapiro.test(sample(na.omit(df$Food_Insecurity_Index), 5000)) # Normality check

# Safety & well-being
shapiro.test(sample(na.omit(df$Unsafe_Score), 5000))
shapiro.test(sample(na.omit(df$WellBeing_Score), 5000))


# ---- Group Summaries ----
summary_stats <- df_comp %>%
  group_by(DV_group) %>%
  summarise(
    mean_food_insec = mean(Food_Insecurity_Index, na.rm = TRUE),
    median_food_insec = median(Food_Insecurity_Index, na.rm = TRUE),
    mean_unsafe = mean(Unsafe_Score, na.rm = TRUE),
    median_unsafe = median(Unsafe_Score, na.rm = TRUE),
    n = n()
  )
print(summary_stats)

# ---- Group Comparison ----
# If normal -> t-test
t_food <- t.test(Food_Insecurity_Index ~ DV_group, data = df_comp)
t_unsafe <- t.test(Unsafe_Score ~ DV_group, data = df_comp)

# If not normal -> Mann-Whitney U test (Wilcoxon rank-sum)
w_food <- wilcox.test(Food_Insecurity_Index ~ DV_group, data = df_comp)
w_unsafe <- wilcox.test(Unsafe_Score ~ DV_group, data = df_comp)

# Print results
cat("\n--- Food Insecurity ---\n")
print(t_food)
print(w_food)

cat("\n--- Unsafe Score ---\n")
print(t_unsafe)
print(w_unsafe)

# QUESTION 7
# Are women with disabilities at higher risk of experiencing domestic violence 
# and lower well-being scores compared to those without disabilities?

# Since disablity status is binary, we'll  carry on with the analysis
# to test assosciation between disability status and DV exposure, well-being scores
# Data Exploration
table(df$disability_status, df$DV_Exposure, useNA = "ifany")

#Cross-tabulate disability status with DV exposure
# Cross-tabulation
tab <- table(df$disability_status, df$DV_Exposure, useNA = "ifany")
tab
names(df)  # see if both are there
nrow(df)   # should be the same for all variables
# Chi-square test (test of independence)
chisq_test <- chisq.test(tab)
chisq_test

# Optional: Logistic regression (for effect size / odds ratio)
model <- glm(DV_Exposure ~ disability_status, data = df, family = binomial)
summary(model)

# Odds ratio + CI
exp(cbind(OR = coef(model), confint(model)))

# Group means
aggregate(WellBeing_Score ~ disability_status, data = df, mean, na.rm = TRUE)
# On average, the non‑disability group scores about 0.097 points higher on your WellBeing_Score scale.
# Checking normality for  confirmation
wilcox_test <- wilcox.test(WellBeing_Score ~ disability_status, data = df)
wilcox_test

# Save outputs for Visualization 

# Q1: DV prevalence across age, marital status, education
q1_age <- df %>%
  group_by(age_group) %>%
  summarise(prevalence = mean(DV_Exposure, na.rm = TRUE) * 100,
            count = n())

q1_marital <- df %>%
  group_by(marital_status) %>%
  summarise(prevalence = mean(DV_Exposure, na.rm = TRUE) * 100,
            count = n())

q1_edu <- df %>%
  group_by(education_level) %>%
  summarise(prevalence = mean(DV_Exposure, na.rm = TRUE) * 100,
            count = n())

write.csv(q1_age, "Q1_DV_by_Age.csv", row.names = FALSE)
write.csv(q1_marital, "Q1_DV_by_Marital.csv", row.names = FALSE)
write.csv(q1_edu, "Q1_DV_by_Education.csv", row.names = FALSE)

# Q2: DV exposure vs safety (Unsafe_Score)
df_aly_2 <- df %>%
  dplyr::select(DV_Exposure, Unsafe_Score) %>%
  dplyr::filter(!is.na(DV_Exposure), !is.na(Unsafe_Score)) %>%
  mutate(DV_Exposure = factor(DV_Exposure, levels = c(0, 1), labels = c("No DV", "DV")))

group_summary <- df_aly_2 %>%
  group_by(DV_Exposure) %>%
  summarise(mean_unsafe = mean(Unsafe_Score, na.rm = TRUE),
            sd_safety = sd(Unsafe_Score, na.rm = TRUE),
            count = n())
write.csv(group_summary, "Q2_DV_vs_Safety.csv", row.names = FALSE)

# Q3: DV Type vs Safety & Wellbeing
df_long <- df %>%
  pivot_longer(cols = c(dv_physical, dv_verbal, dv_economic, dv_other, dv_sexual),
               names_to = "DV_Type", values_to = "Exposure") %>%
  dplyr::filter(Exposure == 1) %>%
  dplyr::select(-Exposure)

df_long$DV_Type <- factor(df_long$DV_Type,
                          levels = c("dv_physical","dv_verbal","dv_economic","dv_other","dv_sexual"),
                          labels = c("Physical","Verbal","Denial_of_Needs","Denial_of_Communication","Sexual_Harassment"))

write.csv(df_long, "Q3_DVType_Safety_WellBeing.csv", row.names = FALSE)

# Q4: Mediation dataset (DV -> Conflicts -> Safety)
df_clean <- df %>%
  mutate(conflict_frequency = case_when(
           conflict_frequency %in% c("Don't know", "Refused") ~ NA_character_,
           TRUE ~ as.character(conflict_frequency)),
         conflict_frequency = factor(conflict_frequency,
           levels = c("Never","Once or twice","Monthly","Weekly","Daily"), ordered = TRUE)) %>%
  dplyr::select(DV_Exposure, conflict_frequency, Unsafe_Score,
         age_group, education_level, marital_status) %>%
  tidyr::drop_na() %>%
  mutate(conflict_frequency_num = as.numeric(conflict_frequency))
write.csv(df_clean, "Q4_Mediation_Dataset.csv", row.names = FALSE)

# Q5: Moderation (Help type × DV exposure)
df_mod <- df %>%
  mutate(help_type_dv = case_when(
           help_source_dv %in% c("Call/go to police","Go to health facility","Call helpline",
                                 "Access to women's centres","Seek help from shelter or safehouse for women",
                                 "Seeking support from women's groups/NGOs/CSOs") ~ "Formal",
           help_source_dv %in% c("Seek support from family","Talk with friends for support or guidance",
                                 "Seek support from a religious leader","Approach community leaders for support") ~ "Informal",
           TRUE ~ "Other/NA"),
         mobility_num = case_when(
           mobility_frequency == "Never" ~ 0,
           mobility_frequency == "Once or twice a month" ~ 1,
           mobility_frequency == "Once a week" ~ 2,
           mobility_frequency == "2-3 times per week" ~ 3,
           mobility_frequency == "Daily" ~ 4,
           TRUE ~ NA_real_))
write.csv(df_mod, "Q5_HelpType_Moderation.csv", row.names = FALSE)

# Q6: DV survivors vs non-survivors (Food insecurity & Safety)
df_comp <- df %>%
  mutate(DV_group = ifelse(DV_Exposure == 1, "survivors", "non-survivors")) %>%
  dplyr::select(DV_group, Food_Insecurity_Index, Unsafe_Score, WellBeing_Score)
write.csv(df_comp, "Q6_DV_vs_NonDV_Outcomes.csv", row.names = FALSE)

# Q7: Disability vs DV Exposure & WellBeing
df_disab <- df %>%
  dplyr::select(disability_status, DV_Exposure, WellBeing_Score)
write.csv(df_disab, "Q7_Disability_DV_WellBeing.csv", row.names = FALSE)

cat("All 7 datasets saved as CSV files for visualization.\n")

