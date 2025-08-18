library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)

# --------------------------------------------------
# UI Component
# --------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "DV & Mental Health Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("DV Exposure Analysis", tabName = "dv_analysis", icon = icon("shield-alt")),
      menuItem("Mental Health Impact", tabName = "mental_health", icon = icon("brain")),
      menuItem("Safety Perceptions", tabName = "safety", icon = icon("home")),
      menuItem("Demographic Patterns", tabName = "demographics", icon = icon("users")),
      menuItem("Data Explorer", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12, status = "primary",
                    h2("Domestic Violence and Mental Health Project"),
                    p("This dashboard presents findings on the relationship between domestic violence exposure and women's mental health outcomes."),
                    br(),
                    valueBoxOutput("sample_size"),
                    valueBoxOutput("dv_prevalence"),
                    valueBoxOutput("mental_health_impact")
                )
              ),
              fluidRow(
                box(width = 12, title = "Key Findings Summary",
                    DTOutput("key_findings_table"))
              )
      ),
      
      # DV Analysis Tab
      tabItem(tabName = "dv_analysis",
              fluidRow(
                box(width = 6, title = "DV Type Prevalence",
                    plotlyOutput("dv_type_prevalence_plot")),
                box(width = 6, title = "DV Exposure by Age",
                    plotlyOutput("dv_age_distribution"))
              ),
              fluidRow(
                box(width = 12, title = "DV Type Correlations",
                    plotlyOutput("dv_correlation_heatmap"))
              )
      ),
      
      # Mental Health Tab
      tabItem(tabName = "mental_health",
              fluidRow(
                box(width = 6, title = "Distress Scores by DV Type",
                    plotlyOutput("distress_dv_plot")),
                box(width = 6, title = "Mental Health Change",
                    plotlyOutput("mental_health_change_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Regression Results",
                    DTOutput("mental_health_model_table"))
              )
      ),
      
      # Safety Perceptions Tab
      tabItem(tabName = "safety",
              fluidRow(
                box(width = 6, title = "Safety Scores by DV Type",
                    plotlyOutput("safety_dv_plot")),
                box(width = 6, title = "Unsafe at Home Prevalence",
                    plotlyOutput("unsafe_home_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Safety Logistic Regression",
                    plotlyOutput("safety_forest_plot"))
              )
      ),
      
      # Demographics Tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(width = 4, title = "Age Group",
                    plotlyOutput("age_distribution")),
                box(width = 4, title = "Education Level",
                    plotlyOutput("education_distribution")),
                box(width = 4, title = "Locality",
                    plotlyOutput("urban_rural_distribution"))
              ),
              fluidRow(
                box(width = 12, title = "Intersectional Analysis",
                    plotlyOutput("intersectional_plot"))
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Filter Data",
                    selectInput("var_select", "Select Variables:", 
                               choices = names(df), multiple = TRUE),
                    sliderInput("age_filter", "Age Group:",
                               min = 18, max = 80, value = c(18, 80)))
              ),
              fluidRow(
                box(width = 12, title = "Data Table",
                    DTOutput("filtered_data_table"))
              )
      )
    )
  )
)

# --------------------------------------------------
# Server Component
# --------------------------------------------------
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    df %>%
      filter(age >= input$age_filter[1] & age <= input$age_filter[2]) %>%
      select(input$var_select)
  })
  
  # Value boxes
  output$sample_size <- renderValueBox({
    valueBox(nrow(df), "Total Sample Size", icon = icon("users"), color = "blue")
  })
  
  output$dv_prevalence <- renderValueBox({
    prev <- mean(df$dv_any_lifetime == "Yes", na.rm = TRUE) * 100
    valueBox(paste0(round(prev, 1), "%"), "Lifetime DV Prevalence", 
            icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$mental_health_impact <- renderValueBox({
    diff <- mean(df$distress_index[df$dv_any_lifetime == "Yes"], na.rm = TRUE) - 
            mean(df$distress_index[df$dv_any_lifetime == "No"], na.rm = TRUE)
    valueBox(round(diff, 2), "Avg. Distress Score Difference", 
            icon = icon("brain"), color = "green")
  })
  
  # Plots (replace with your actual plots)
  output$dv_type_prevalence_plot <- renderPlotly({
    # Your ggplot here
    p <- ggplot(dv_prevalence_long, aes(x = dv_type, y = prevalence, fill = dv_type)) +
      geom_col() +
      labs(title = "Prevalence of Different DV Types")
    ggplotly(p)
  })
  
  output$distress_dv_plot <- renderPlotly({
    # Your ggplot here
    p <- ggplot(df, aes(x = dv_type, y = distress_index, fill = dv_type)) +
      geom_boxplot() +
      labs(title = "Distress Scores by DV Type")
    ggplotly(p)
  })
  
  # Add all other plot outputs following the same pattern
  
  # Data table
  output$filtered_data_table <- renderDT({
    datatable(filtered_data(), options = list(scrollX = TRUE))
  })
  
  # Key findings table
  output$key_findings_table <- renderDT({
    datatable(key_findings_df, rownames = FALSE)
  })
}

# --------------------------------------------------
# Run the application
# --------------------------------------------------
shinyApp(ui = ui, server = server)