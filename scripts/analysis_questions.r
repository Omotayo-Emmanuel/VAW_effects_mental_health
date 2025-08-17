# Load Libraries
library(dplyr)
library(tidyverse)
chooseCRANmirror()
install.packages("effsize")
library(effsize)
install.packages("effectsize")
library(effectsize)
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

# Had to create a vriable column for DV_Type
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