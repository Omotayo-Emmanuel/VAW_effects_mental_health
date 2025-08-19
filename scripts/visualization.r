# visualization.R
# Script to visualize key findings from domestic violence analysis

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("effects")
library(effects) # For plotting model effects

# Load the cleaned dataset
df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\data_processed\\Normalized_DV_Dataset.csv")
# Load analysis results
q4_df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q4_Mediation_Dataset.csv")
q5_df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q5_HelpType_Moderation.csv")
q6_df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q6_DV_vs_NonDV_Outcomes.csv")
q7_df <- read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q7_Disability_DV_WellBeing.csv")
# Set theme for all plots
theme_set(theme_minimal(base_size = 12) + 
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
              axis.title = element_text(color = "black"),
              axis.text = element_text(color = "black"),
              legend.text = element_text(color = "black"),
              legend.title = element_text(color = "black"),
              legend.position = "bottom"
            ))
### QUESTION 1: DV PREVALENCE BY DEMOGRAPHICS ###

# 1.1 Overall DV exposure prevalence
p1 <- ggplot(df, aes(x = factor(DV_Exposure))) +
    geom_bar(aes(y = ..prop.., group = 1), fill = "steelblue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Overall Domestic Violence Exposure",
        x = "Exposure Status",
        y = "Percentage") +
    scale_x_discrete(labels = c("No Exposure", "Exposed"))



# 1.2 DV prevalence by age group
p2 <- df %>%
  group_by(age_group, DV_Exposure) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(DV_Exposure == 1) %>%
  ggplot(aes(x = age_group, y = percentage)) +
  geom_col(fill = "coral") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "DV Exposure by Age Group",
       x = "Age Group",
       y = "Percentage Exposed")


# 1.3 DV prevalence by education level (similar to age group plot)
p3 <- df %>%
  group_by(education_level, DV_Exposure) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(DV_Exposure == 1) %>%
  ggplot(aes(x = education_level, y = percentage)) +
  geom_col(fill = "goldenrod") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "DV Exposure by Education Level",
       x = "Education Level",
       y = "Percentage Exposed")



### QUESTION 2: DV AND SAFETY PERCEPTIONS ###

# 2.1 Boxplot of safety scores by DV exposure
p2_1 <- ggplot(df, aes(x = factor(DV_Exposure), y = Unsafe_Score)) +
  geom_boxplot(fill = c("skyblue", "salmon")) +
  labs(title = "Safety Scores by DV Exposure Status",
       x = "DV Exposure",
       y = "Safety Score") +
  scale_x_discrete(labels = c("No Exposure", "Exposed"))

# 2.2 Density plot comparing distributions
p2_2 <- ggplot(df, aes(x = Unsafe_Score, fill = factor(DV_Exposure))) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of Safety Scores",
       x = "Safety Score",
       y = "Density",
       fill = "DV Exposure") +
  scale_fill_manual(values = c("skyblue", "salmon"),
                    labels = c("No Exposure", "Exposed"))

### QUESTION 3: DV TYPES AND OUTCOMES ###

# Prepare long format data for DV types
df_long <- df %>%
  pivot_longer(cols = c(dv_physical, dv_verbal, dv_economic, dv_other, dv_sexual),
               names_to = "DV_Type",
               values_to = "Exposure") %>%
  filter(Exposure == 1)

# 3.1 Unsafe scores by DV type
p3_1 <- ggplot(df_long, aes(x = DV_Type, y = Unsafe_Score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Safety Scores by Type of Domestic Violence",
       x = "DV Type",
       y = "Safety Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.2 Well-being scores by DV type
p3_2 <- ggplot(df_long, aes(x = DV_Type, y = WellBeing_Score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Well-being Scores by Type of Domestic Violence",
       x = "DV Type",
       y = "Well-being Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### QUESTION 4: MEDIATION ANALYSIS VISUALIZATION ###

# 4.1 Conflict frequency by DV exposure
p4_1 <- ggplot(q4_df, aes(x = conflict_frequency, fill = factor(DV_Exposure))) +
  geom_bar(position = "dodge") +
  labs(title = "Conflict Frequency by DV Exposure",
       x = "Conflict Frequency",
       y = "Count",
       fill = "DV Exposure") +
  scale_fill_manual(values = c("skyblue", "salmon"),
                    labels = c("No Exposure", "Exposed")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.2 Mediation model visualization
plot(med_out) # This creates plots showing the mediation pathways

### QUESTION 5: HELP-SEEKING MODERATION ###

# 5.1 Help-seeking types
q5_p1 <- q5_df %>%
  filter(!is.na(help_type_dv), help_type_dv != "Other/NA") %>%
  ggplot(aes(x = help_type_dv, fill = factor(DV_Exposure))) +
  geom_bar(position = "dodge") +
  labs(title = "Help-Seeking Behavior by DV Exposure",
       x = "Type of Help Sought",
       y = "Count",
       fill = "DV Exposure") +
  scale_fill_manual(values = c("skyblue", "salmon"),
                    labels = c("No Exposure", "Exposed"))

# 5.2 Interaction plot for safety perceptions
# Using the effects package to visualize interaction
eff <- Effect(c("DV_Exposure", "help_type_dv"), model_safety)
png("Q5_2_interaction_safety_by_dv_help.png", width = 800, height = 600, res = 120)
plot(eff, 
     main = "Interaction Between DV Exposure and Help Type",
     xlab = "DV Exposure",
     ylab = "Probability of Feeling Unsafe",
     lines = list(col = c("blue", "red")),
     confint = list(style = "bars"))
dev.off()
### QUESTION 6: MENTAL HEALTH STRAIN INDICATORS ###

# 6.1 Food Insecurity by DV Exposure
p6_1 <- ggplot(q6_df, aes(x = DV_group, y = Food_Insecurity_Index, fill = DV_group)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Food Insecurity by DV Exposure Status",
       x = "Group",
       y = "Food Insecurity Index",
       fill = "Group") +
  scale_fill_manual(values = c("non-survivors" = "skyblue", "survivors" = "salmon")) +
  theme(legend.position = "none") + # Remove legend since x-axis already labels groups
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, color = "black") # Add mean marker

# 6.2 Feeling Unsafe by DV Exposure
p6_2 <- ggplot(q6_df, aes(x = DV_group, y = Unsafe_Score, fill = DV_group)) +
  geom_violin(alpha = 0.6, trim = FALSE) + # Shows distribution shape
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) + # Adds boxplot inside
  labs(title = "Safety Perceptions by DV Exposure Status",
       x = "Group",
       y = "Unsafe Score",
       fill = "Group") +
  scale_fill_manual(values = c("non-survivors" = "lightgreen", "survivors" = "orange")) +
  theme(legend.position = "none")

# 6.3 Combined effect visualization (compact)
p6_3 <- q6_df %>%
  dplyr::select(DV_group, Food_Insecurity_Index, Unsafe_Score) %>%
  pivot_longer(cols = -DV_group, names_to = "Measure", values_to = "Score") %>%
  ggplot(aes(x = DV_group, y = Score, fill = Measure)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Mental Health Strain Indicators by DV Exposure",
       x = "Group",
       y = "Score",
       fill = "Measure") +
  scale_fill_manual(values = c("Food_Insecurity_Index" = "steelblue", 
                              "Unsafe_Score" = "goldenrod"),
                    labels = c("Food Insecurity", "Feeling Unsafe")) +
  theme(legend.position = "bottom")


### QUESTION 7: DISABILITY STATUS AND DV EXPOSURE ###

# 7.1 DV Exposure by Disability Status (Stacked Bar Chart)
p7_1 <- ggplot(q7_df, aes(x = factor(disability_status), fill = factor(DV_Exposure))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "DV Exposure by Disability Status",
       x = "Disability Status",
       y = "Percentage",
       fill = "DV Exposure") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                    labels = c("Not Exposed", "Exposed")) +
  scale_x_discrete(labels = c("No Disability", "With Disability"))

# 7.2 Well-being Scores by Disability Status (Boxplot + Jitter)
p7_2 <- ggplot(q7_df, aes(x = factor(disability_status), y = WellBeing_Score)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkgreen") + # Shows individual points
  labs(title = "Well-being Scores by Disability Status",
       x = "Disability Status",
       y = "Well-being Score") +
  scale_x_discrete(labels = c("No Disability", "With Disability")) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") # Add mean marker

# 7.3 Combined Risk Visualization (Point Range Plot)
p7_3 <- q7_df %>%
  group_by(disability_status) %>%
  summarise(
    dv_rate = mean(DV_Exposure, na.rm = TRUE),
    wellbeing_mean = mean(WellBeing_Score, na.rm = TRUE),
    wellbeing_se = sd(WellBeing_Score, na.rm = TRUE)/sqrt(n())) %>%
  ggplot(aes(x = factor(disability_status))) +
  geom_pointrange(aes(y = dv_rate*100, ymin = dv_rate*100, ymax = dv_rate*100),
                  color = "salmon", size = 1) +
  geom_pointrange(aes(y = wellbeing_mean, 
                      ymin = wellbeing_mean - 1.96*wellbeing_se,
                      ymax = wellbeing_mean + 1.96*wellbeing_se),
                  color = "darkgreen", size = 1) +
  scale_y_continuous(
    name = "Well-being Score",
    sec.axis = sec_axis(~./100, name = "DV Exposure Rate")) +
  labs(title = "Disability Status: DV Risk and Well-being",
       x = "Disability Status") +
  scale_x_discrete(labels = c("No Disability", "With Disability")) +
  theme(axis.title.y.right = element_text(color = "salmon"),
        axis.title.y.left = element_text(color = "darkgreen"))

  # Save the plot
ggsave("plots/Q1_1_overall_exposure.png", p1, width = 8, height = 6)
ggsave("plots/Q1_2_by_age.png", p2, width = 8, height = 6)
ggsave("plots/Q1_3_by_education.png", p3, width = 8, height = 6)
ggsave("plots/Q2_1_safety_scores_by_DV_exposure.png", p2_1, width = 8, height = 6)
ggsave("plots/Q2_2_distribution_of_safety_scores.png", p2_2, width = 8, height = 6)
ggsave("plots/Q3_1_safety_by_dv_type.png", p3_1, width = 8, height = 6, dpi = 300)
ggsave("plots/Q3_2_wellbeing_by_dv_type.png", p3_2, width = 8, height = 6, dpi = 300)
ggsave("plots/Q4_conflict_mediation.png",p4_1, width = 8, height = 6)
ggsave("plots/Q5_1_helpseeking_by_dv.png", q5_p1, width = 8, height = 6, dpi = 300)
ggsave("plots/6_1_Food_Insecurity_by_DV.png", p6_1, width = 7, height = 5, dpi = 300)
ggsave("plots/6_2_Unsafe_Score_by_DV.png", p6_2, width = 7, height = 5, dpi = 300)
ggsave("plots/6_3_Mental_Health_Strain_by_DV.png", p6_3, width = 7, height = 5, dpi = 300)
ggsave("plots/7_1_DV_by_Disability.png", p7_1, width = 7, height = 5, dpi = 300)
ggsave("plots/7_2_Wellbeing_by_Disability.png", p7_2, width = 7, height = 5, dpi = 300)
ggsave("plots/7_3_Disability_Risk_Combined.png", p7_3, width = 7, height = 5, dpi = 300)
