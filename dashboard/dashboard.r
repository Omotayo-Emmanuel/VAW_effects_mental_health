#
# Domestic Violence Analysis Dashboard
# Shiny App UI and Server Code
#

# Load necessary libraries
library(shiny)        # Core Shiny functionality for app framework
library(shinydashboard) # For dashboard layout (optional but recommended for structure)
library(tidyverse)    # For data manipulation (dplyr) and plotting (ggplot2)
library(ggplot2)      # For creating visualizations
library(scales)       # For formatting plot axes (e.g., percent labels)
library(effects)      # For calculating and plotting model effects (Q5 interaction)
library(mediation)    # For mediation analysis plots (Q4)

# Define UI for application - the user interface layout
ui <- dashboardPage(
  skin = "blue", # Dashboard theme color
  
  # Application title
  dashboardHeader(title = "DV Analysis Dashboard"),
  
  # Sidebar with navigation menu for different analytical questions
  dashboardSidebar(
    sidebarMenu(
      # Menu Items for each analytical question
      menuItem("Overview & Prevalence (Q1)", tabName = "q1", icon = icon("chart-bar")),
      menuItem("DV & Safety (Q2)", tabName = "q2", icon = icon("shield-alt")),
      menuItem("DV Types & Outcomes (Q3)", tabName = "q3", icon = icon("project-diagram")),
      menuItem("Conflict Mediation (Q4)", tabName = "q4", icon = icon("comments")),
      menuItem("Help-Seeking Moderation (Q5)", tabName = "q5", icon = icon("hands-helping")),
      menuItem("Mental Health Strain (Q6)", tabName = "q6", icon = icon("brain")),
      menuItem("Disability & Risk (Q7)", tabName = "q7", icon = icon("wheelchair"))
    )
  ),
  
  # Main panel body where content for each tab is displayed
  dashboardBody(
    tabItems(
            # First tab content: Overview & Prevalence (Q1)
      tabItem(tabName = "q1",
              h2("Domestic Violence Exposure Prevalence and Demographics"),
              fluidRow(
                # Overall Prevalence Plot
                box(plotOutput("plot1_overall"), width = 6, title = "Overall Exposure", status = "primary"),
                # Prevalence by Age Group Plot
                box(plotOutput("plot1_age"), width = 6, title = "Exposure by Age Group", status = "primary")
              ),
              fluidRow(
                # Prevalence by Education Plot
                box(plotOutput("plot1_education"), width = 6, title = "Exposure by Education", status = "primary"),
                # Prevalence by Marital Status Plot (NEW!)
                box(plotOutput("plot1_marital"), width = 6, title = "Exposure by Marital Status", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box - Now spans the full width below the charts
                box(
                  h4("Key Findings (Q1):"),
                  tags$ul(
                    tags$li("57.98% of women reported exposure to or knowledge of DV."),
                    tags$li("Prevalence decreases with age: Highest in 18-29 (61.5%), lowest in 60+ (44.0%)."),
                    tags$li("Education shows a complex relationship: Lowest in 'Technical/vocational' (46.3%), highest in 'Less than primary' (70.5%)."),
                    tags$li("Marital Status: Separated/Divorced individuals show the highest prevalence (62.3%)."),
                    tags$li("Chi-squared tests confirmed significant variation by age, marital status, and education (all p < .05).")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      ),
      # Second tab content: DV & Safety (Q2)
      tabItem(tabName = "q2",
              h2("Relationship between Domestic Violence Exposure and Feelings of Safety"),
              fluidRow(
                # Boxplot of Safety Scores by DV Exposure
                box(plotOutput("plot2_boxplot"), width = 6, title = "Unsafety Scores by Exposure", status = "primary"),
                # Density plot of Safety Scores
                box(plotOutput("plot2_density"), width = 6, title = "Distribution of UnSafety Scores", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box
                box(
                  h4("Key Findings (Q2):"),
                  tags$ul(
                    tags$li("A strong, significant relationship exists between Domestic Violence exposure and feeling unsafe."),
                    tags$li("Individuals exposed to domestic violence (Mean Safety Score = 0.450) report feeling significantly less safe than those not exposed (Mean Safety Score = 0.326) (diff = 0.124)."),
                    tags$li("Welch's t-test: confirmed highly significant difference, p < .001."),
                    tags$li("Wilcoxon rank‑sum test : non‑parametric check, also significant (p < 0.001)"),
                    tags$li("Effect size (Hedges' g) = -0.61, indicating a medium to large effect. , meaning the difference is meaningful in practical terms as well as statistically.")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      ),
      
      # Third tab content: DV Types & Outcomes (Q3)
      tabItem(tabName = "q3",
              h2("Impact of Different Types of Domestic Violence"),
              fluidRow(
                # Safety Scores by DV Type Plot
                box(plotOutput("plot3_safety"), width = 6, title = "Unsafety by DV Type", status = "primary"),
                # Well-being Scores by DV Type Plot
                box(plotOutput("plot3_wellbeing"), width = 6, title = "Well-being by DV Type", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box
                box(
                  h4("Key Findings (Q3):"),
                  tags$ul(
                    tags$li("Correlation check: Unsafe_Score and WellBeing_Score were moderately negatively correlated (r ≈ −0.576, p < 0.001)."),
                    tags$li("MANOVA showed significant overall effect of DV type on both safety and well-being (p < .001)."),
                    tags$li("Post-hoc tests revealed differential impacts:"),
                    tags$ul(
                      tags$li("Safety: Physical and Sexual harassment types were associated with highest unsafe scores."),
                      tags$li("Well-being: Verbal and physical showed strong negative impacts.")
                    ),
                    tags$li("Effect sizes (Eta²) were small but significant (~0.008)."),
                    tags$li(" the type of abuse shapes how unsafe women feel and how low their well‑being is, with some forms linked to worse outcomes than others.")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      ),
      
      # Fourth tab content: Conflict Mediation (Q4)
      tabItem(tabName = "q4",
              h2("Mediating Role of Frequent Conflicts at Home"),
              fluidRow(
                # Conflict Frequency by DV Exposure Plot
                box(plotOutput("plot4_conflict"), width = 6, title = "Conflict Frequency by Exposure", status = "primary"),
                # Mediation Analysis Plot (from mediation package)
                box(plotOutput("plot4_mediation"), width = 6, title = "Mediation Pathways", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box
                box(
                  h4("Key Findings (Q4):"),
                  tags$ul(
                    tags$li("Frequent conflicts at home partially mediate the DV exposure -> safety perception relationship."),
                    tags$li(". Exposure to domestic violence increases conflict frequency, which in turn increases feelings of unsafety"),
                    tags$li("ACME (Indirect effect): 0.0157, p < .001 - conflict frequency adds a small but statistically significant contribution to unsafety perceptions." ),
                    tags$li("ADE (Direct effect): 0.1044, p < .001 -  even after accounting for conflicts, DV exposure still strongly predicts higher unsafety scores."),
                    tags$li("Total effect: 0.1201, p < .001"),
                    tags$li("Proportion mediated: ~13.0%")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      ),
      
      # Fifth tab content: Help-Seeking Moderation (Q5)
      tabItem(tabName = "q5",
              h2("Moderating Effect of Help-Seeking Behavior"),
              fluidRow(
                # Help-Seeking Behavior Plot
                box(plotOutput("plot5_helpseeking"), width = 6, title = "Help-Seeking by Exposure", status = "primary"),
                # Interaction Effect Plot
                box(plotOutput("plot5_interaction"), width = 6, title = "Interaction Effect", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box
                box(
                  h4("Key Findings (Q5):"),
                  tags$ul(
                    tags$li("Help-seeking behavior did not significantly moderate the relationship between DV exposure and:"),
                    tags$ul(
                      tags$li("Safety perceptions (binary outcome)"),
                      tags$li("Mobility frequency")
                    ),
                    tags$li("the interaction terms were not significant (p = 0.280 for safety; p = 0.872 for mobility)."),
                    tags$li("Meaning Interaction terms in both models were non-significant (p > 0.2)."),
                    tags$li("Formal help sources were less common than informal sources."),
                    tags$li("lines are almost perfectly parallel. This means that the effect of DV Exposure on the probability
                    of feeling unsafe is consistently strong and negative, and this effect does not change depending on the type of help sought.")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      ),
      
      # Sixth tab content: Mental Health Strain (Q6)
      tabItem(tabName = "q6",
              h2("Mental Health Strain Indicators Among Survivors"),
              fluidRow(
                # Food Insecurity by DV Exposure Plot
                box(plotOutput("plot6_food"), width = 4, title = "Food Insecurity", status = "primary"),
                # Safety Perceptions by DV Exposure Plot
                box(plotOutput("plot6_safety"), width = 4, title = "Safety Perceptions", status = "primary"),
                # Combined Indicators Plot
                box(plotOutput("plot6_combined"), width = 4, title = "Combined Indicators", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box
                box(
                  h4("Key Findings (Q6):"),
                  tags$ul(
                    tags$li("DV survivors reported significantly higher levels of mental health strain:"),
                    tags$ul(
                      tags$li("Food Insecurity: 0.413 vs 0.250 (non-survivors), p < .001"),
                      tags$li("Feeling Unsafe: 0.450 vs 0.326 (non-survivors), p < .001")
                    ),
                    tags$li("Effect sizes were medium to large for both measures."),
                    tags$li("DV exposure is strongly associated with increased food insecurity and reduced feelings of safety 
                    both key indicators of psychological strain.")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      ),
      
      # Seventh tab content: Disability & Risk (Q7)
      tabItem(tabName = "q7",
              h2("Disability Status as a Risk Factor"),
              fluidRow(
                # DV Exposure by Disability Status Plot
                box(plotOutput("plot7_dv"), width = 4, title = "DV Exposure by Disability", status = "primary"),
                # Well-being by Disability Status Plot
                box(plotOutput("plot7_wellbeing"), width = 4, title = "Well-being by Disability", status = "primary"),
                # Combined Risk Visualization Plot
                box(plotOutput("plot7_combined"), width = 4, title = "Combined Risk Profile", status = "primary")
              ),
              fluidRow(
                # Key Takeaways Box
                box(
                  h4("Key Findings (Q7):"),
                  tags$ul(
                    tags$li("Women with disabilities face disproportionate risks:"),
                    tags$ul(
                      tags$li("Higher DV exposure: 61.4% vs 57.1% (no disability)"),
                      tags$li("Lower well-being scores: 0.546 vs 0.643 (no disability)")
                    ),
                    tags$li("Statistical tests: Chi-square (p < 0.001) and logistic regression (OR = 1.20) confirm a significant association."),
                    tags$li("Odds ratio for DV exposure: 1.20 [1.11, 1.30] for disabled women"),
                    tags$li("Well-being: Average score for women with disabilities was 0.55, compared to 0.64 for non-disabled women (p < 0.001, Wilcoxon test).")
                  ), width = 12, title = "Analysis Summary", background = "light-blue"
                )
              )
      )
    ) # End of tabItems
  ) # End of dashboardBody
) # End of dashboardPage UI

# Define server logic required to draw plots and process data
server <- function(input, output) {
  
  # Load the main dataset
  # NOTE: Replace the path with your actual file path to Normalized_DV_Dataset.csv
  df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\data_processed\\Normalized_DV_ Dataset.csv") # Update this path
  })
  # Load analysis results for Q1
  q1_edu_df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q1_DV_by_Education.csv") # Update this path
  })
  q1_age_df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q1_DV_by_Age.csv") # Update this path
  })
    q1_marital_df <- reactive({
        read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q1_DV_by_Marital.csv") # Update this path
    })
  # Load analysis results for Q4
  # NOTE: Replace the path with your actual file path to Q4_Mediation_Dataset.csv
  q4_df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q4_Mediation_Dataset.csv") # Update this path
  })
  
  # Load analysis results for Q5
  # NOTE: Replace the path with your actual file path to Q5_HelpType_Moderation.csv
  q5_df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q5_HelpType_Moderation.csv") # Update this path
  })
  
  # Load analysis results for Q6
  # NOTE: Replace the path with your actual file path to Q6_DV_vs_NonDV_Outcomes.csv
  q6_df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q6_DV_vs_NonDV_Outcomes.csv") # Update this path
  })
  
  # Load analysis results for Q7
  # NOTE: Replace the path with your actual file path to Q7_Disability_DV_WellBeing.csv
  q7_df <- reactive({
    read.csv("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\Q7_Disability_DV_WellBeing.csv") # Update this path
  })
  
  # Set a consistent theme for all plots
  plot_theme <- theme_minimal(base_size = 12) + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom")
  
  # Q1.1: Overall DV exposure prevalence plot
  output$plot1_overall <- renderPlot({
    data <- df()
    ggplot(data, aes(x = factor(DV_Exposure))) +
      geom_bar(aes(y = ..prop.., group = 1), fill = "steelblue") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Overall Domestic Violence Exposure",
           x = "Exposure Status", y = "Percentage") +
      scale_x_discrete(labels = c("No Exposure", "Exposed")) +
      plot_theme
  })
  
    # Q1.2: DV prevalence by age group plot (Using Q1_DV_by_Age.csv)
    output$plot1_age <- renderPlot({
    data <- q1_age_df() # Load the pre-summarised data
    ggplot(data, aes(x = age_group, y = prevalence/100)) +
        geom_col(fill = "coral") +
        geom_text(aes(label = paste0(round(prevalence, 1), "%")), vjust = -0.5, size = 4) +  # Add value labels
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.65))  +
        labs(title = "DV Exposure by Age Group",
            x = "Age Group", y = "Percentage Exposed",
            caption = paste("Total N =", sum(data$count))) + # Add total sample size as caption
        plot_theme
    })
    
   #Q1.3: DV prevalence by education level plot (Using Q1_DV_by_Education.csv)
output$plot1_education <- renderPlot({
  data <- q1_edu_df() # Load the pre-summarised data
  
  ggplot(data, aes(x = education_level, y = prevalence/100)) +
    geom_col(fill = "goldenrod") +
    geom_text(aes(label = paste0(round(prevalence, 1), "%")), vjust = -0.5, size = 4, angle = 45) + # Add value labels
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.75)) +
    labs(title = "DV Exposure by Education Level",
         x = "Education Level", y = "Percentage Exposed",
         caption = paste("Total N =", sum(data$count))) + # Add total sample size as caption
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
    plot_theme
})

    # Q1.4: DV prevalence by marital status (Using Q1_DV_by_Marital.csv)
output$plot1_marital <- renderPlot({
  data <- q1_marital_df() # Load the pre-summarised data
  ggplot(data, aes(x = marital_status, y = prevalence/100)) +
    geom_col(fill = "purple") +
    geom_text(aes(label = paste0(round(prevalence, 1), "%")), vjust = -0.5, size = 4) + # Add value labels
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.65)) +
    labs(title = "DV Exposure by Marital Status",
         x = "Marital Status", y = "Percentage Exposed",
         caption = paste("Total N =", sum(data$count))) + # Add total sample size as caption
    plot_theme
})
  
  # Q2.1: Boxplot of safety scores by DV exposure
  output$plot2_boxplot <- renderPlot({
    data <- df()
    ggplot(data, aes(x = factor(DV_Exposure), y = Unsafe_Score)) +
      geom_boxplot(fill = c("skyblue", "salmon")) +
      labs(title = "Unsafety Scores by DV Exposure Status",
           x = "DV Exposure", y = "Unsafety Score") +
      scale_x_discrete(labels = c("No Exposure", "Exposed")) +
      plot_theme
  })
  
  # Q2.2: Density plot comparing distributions
  output$plot2_density <- renderPlot({
    data <- df()
    ggplot(data, aes(x = Unsafe_Score, fill = factor(DV_Exposure))) +
      geom_density(alpha = 0.6) +
      labs(title = "Distribution of Unsafety Scores",
           x = "unsafety Score", y = "Density", fill = "DV Exposure") +
      scale_fill_manual(values = c("skyblue", "salmon"),
                        labels = c("No Exposure", "Exposed")) +
      plot_theme
  })
  
  # Q3.1: Unsafe scores by DV type
  output$plot3_safety <- renderPlot({
    data <- df()
    df_long <- data %>%
      pivot_longer(cols = c(dv_physical, dv_verbal, dv_economic, dv_other, dv_sexual),
                   names_to = "DV_Type", values_to = "Exposure") %>%
      filter(Exposure == 1)
    
    ggplot(df_long, aes(x = DV_Type, y = Unsafe_Score)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = "Unsafety Scores by Type of Domestic Violence",
           x = "DV Type", y = "Unsafety Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      plot_theme
  })
  
  # Q3.2: Well-being scores by DV type
  output$plot3_wellbeing <- renderPlot({
    data <- df()
    df_long <- data %>%
      pivot_longer(cols = c(dv_physical, dv_verbal, dv_economic, dv_other, dv_sexual),
                   names_to = "DV_Type", values_to = "Exposure") %>%
      filter(Exposure == 1)
    
    ggplot(df_long, aes(x = DV_Type, y = WellBeing_Score)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Well-being Scores by Type of Domestic Violence",
           x = "DV Type", y = "Well-being Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      plot_theme
  })
  
  # Q4.1: Conflict frequency by DV exposure
  output$plot4_conflict <- renderPlot({
    data <- q4_df()
    ggplot(data, aes(x = conflict_frequency, fill = factor(DV_Exposure))) +
      geom_bar(position = "dodge") +
      labs(title = "Conflict Frequency by DV Exposure",
           x = "Conflict Frequency", y = "Count", fill = "DV Exposure") +
      scale_fill_manual(values = c("skyblue", "salmon"),
                        labels = c("No Exposure", "Exposed")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      plot_theme
  })
  
  # Q4.2: Mediation model visualization
  # NOTE: This requires the mediation model object (med_out) to be saved and loaded
  # For now, we'll create a placeholder or load if available
  output$plot4_mediation <- renderPlot({
    # Load the mediation model object
    load("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\mediation_model.RData")
    plot(med_out)
  
  })
  
  # Q5.1: Help-seeking types
  output$plot5_helpseeking <- renderPlot({
    data <- q5_df()
    data %>%
      filter(!is.na(help_type_dv), help_type_dv != "Other/NA") %>%
      ggplot(aes(x = help_type_dv, fill = factor(DV_Exposure))) +
      geom_bar(position = "dodge") +
      labs(title = "Help-Seeking Behavior by DV Exposure",
           x = "Type of Help Sought", y = "Count", fill = "DV Exposure") +
      scale_fill_manual(values = c("skyblue", "salmon"),
                        labels = c("No Exposure", "Exposed")) +
      plot_theme
  })
  
  # Q5.2: Interaction plot for safety perceptions
  # NOTE: This requires the model_safety object to be saved and loaded
  output$plot5_interaction <- renderPlot({
    # If you have saved the model object, load it here
    load("C:\\Users\\1040G7\\Documents\\INTERNSHIP\\NITDA\\Data_science_begineers\\DS_beginners_project\\analysis_results\\model_safety.RData")
    eff <- Effect(c("DV_Exposure", "help_type_dv"), model_safety)
    plot(eff, main = "Interaction Between DV Exposure and Help Type",
          xlab = "DV Exposure", ylab = "Probability of Feeling Unsafe",
          lines = list(col = c("blue", "red")), confint = list(style = "bars"))
  })
  
  # Q6.1: Food Insecurity by DV Exposure
  output$plot6_food <- renderPlot({
    data <- q6_df()
    ggplot(data, aes(x = DV_group, y = Food_Insecurity_Index, fill = DV_group)) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "Food Insecurity by DV Exposure Status",
           x = "Group", y = "Food Insecurity Index") +
      scale_fill_manual(values = c("non-survivors" = "skyblue", "survivors" = "salmon")) +
      theme(legend.position = "none") +
      stat_summary(fun = mean, geom = "point", shape = 15, size = 3, color = "black") +
      plot_theme
  })
  
  # Q6.2: Feeling Unsafe by DV Exposure
  output$plot6_safety <- renderPlot({
    data <- q6_df()
    ggplot(data, aes(x = DV_group, y = Unsafe_Score, fill = DV_group)) +
      geom_violin(alpha = 0.6, trim = FALSE) +
      geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) +
      labs(title = "Safety Perceptions by DV Exposure Status",
           x = "Group", y = "Unsafe Score") +
      scale_fill_manual(values = c("non-survivors" = "lightgreen", "survivors" = "orange")) +
      theme(legend.position = "none") +
      plot_theme
  })
  
  # Q6.3: Combined effect visualization
  output$plot6_combined <- renderPlot({
    data <- q6_df()
    data %>%
      dplyr::select(DV_group, Food_Insecurity_Index, Unsafe_Score) %>%
      pivot_longer(cols = -DV_group, names_to = "Measure", values_to = "Score") %>%
      ggplot(aes(x = DV_group, y = Score, fill = Measure)) +
      geom_boxplot(position = position_dodge(0.8)) +
      labs(title = "Mental Health Strain Indicators by DV Exposure",
           x = "Group", y = "Score", fill = "Measure") +
      scale_fill_manual(values = c("Food_Insecurity_Index" = "steelblue", 
                                  "Unsafe_Score" = "goldenrod"),
                        labels = c("Food Insecurity", "Feeling Unsafe")) +
      theme(legend.position = "bottom") +
      plot_theme
  })
  
  # Q7.1: DV Exposure by Disability Status
  output$plot7_dv <- renderPlot({
    data <- q7_df()
    ggplot(data, aes(x = factor(disability_status), fill = factor(DV_Exposure))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "DV Exposure by Disability Status",
           x = "Disability Status", y = "Percentage", fill = "DV Exposure") +
      scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"),
                        labels = c("Not Exposed", "Exposed")) +
      scale_x_discrete(labels = c("No Disability", "With Disability")) +
      plot_theme
  })
  
  # Q7.2: Well-being Scores by Disability Status
  output$plot7_wellbeing <- renderPlot({
    data <- q7_df()
    ggplot(data, aes(x = factor(disability_status), y = WellBeing_Score)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3, color = "darkgreen") +
      labs(title = "Well-being Scores by Disability Status",
           x = "Disability Status", y = "Well-being Score") +
      scale_x_discrete(labels = c("No Disability", "With Disability")) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      plot_theme
  })
  
  # Q7.3: Combined Risk Visualization
  output$plot7_combined <- renderPlot({
    data <- q7_df()
    summary_data <- data %>%
      group_by(disability_status) %>%
      summarise(
        dv_rate = mean(DV_Exposure, na.rm = TRUE),
        wellbeing_mean = mean(WellBeing_Score, na.rm = TRUE),
        wellbeing_se = sd(WellBeing_Score, na.rm = TRUE)/sqrt(n()))
    
    ggplot(summary_data, aes(x = factor(disability_status))) +
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
            axis.title.y.left = element_text(color = "darkgreen")) +
      plot_theme
  })
}

# Run the application 
shinyApp(ui = ui, server = server)