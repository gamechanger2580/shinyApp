#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
## Load required packages
# Load required libraries

# Load required packages
# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load the dataset
dataset <- read.csv("Book1.csv")

# Define UI for the application
ui <- fluidPage(
  navbarPage("Disease Group Insights",
             # Page 1: Overview & Trends
             tabPanel("Overview & Trends",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("sex", "Select Sex:", choices = unique(dataset$sex)),
                          selectInput("disease_group", "Select Disease Group:", choices = unique(dataset$disease_group))
                        ),
                        mainPanel(
                          plotOutput("line_trend"),
                          plotOutput("asr_bar")
                        )
                      )
             ),
             
             # Page 2: Demographic Analysis
             tabPanel("Demographic Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year", "Select Year:", choices = unique(dataset$data_year)),
                          selectInput("sex_demo", "Select Sex:", choices = unique(dataset$sex))
                        ),
                        mainPanel(
                          plotOutput("boxplot_asr"),
                          plotOutput("bar_comparison"),
                          dataTableOutput("top_conditions")
                        )
                      )
             ),
             
             # Page 3: Health Burden Analysis
             tabPanel("Health Burden Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("group", "Select Disease Group:", choices = unique(dataset$disease_group))
                        ),
                        mainPanel(
                          plotOutput("time_series")
                        )
                      )
             )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Progress example added in line_trend plot rendering
  output$line_trend <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating Yearly Trends Plot...", value = 0)
    
    # Simulate long processing task
    progress$inc(0.5)
    Sys.sleep(1)
    progress$inc(0.5)
    
    ggplot(filtered_data(), aes(x = data_year)) +
      geom_line(aes(y = yll, color = "YLL", group = 1)) +
      geom_line(aes(y = yld, color = "YLD", group = 1)) +
      geom_line(aes(y = daly, color = "DALY", group = 1)) +
      labs(title = "Yearly Trends of YLL, YLD, and DALY", x = "Year", y = "Values") +
      theme_minimal() +
      scale_color_manual(values = c("YLL" = "blue", "YLD" = "green", "DALY" = "red"))
  })
  
  # Filtered dataset based on input selections
  filtered_data <- reactive({
    dataset %>%
      filter(sex == input$sex, disease_group == input$disease_group)
  })
  
  # Bar plot for ASR by Sex and Disease Group
  output$asr_bar <- renderPlot({
    ggplot(dataset %>% filter(data_year == max(dataset$data_year)), aes(x = disease_group, y = daly_asr, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "DALY ASR by Sex and Disease Group", x = "Disease Group", y = "DALY ASR") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Summary table of YLL, YLD, DALY
  output$summary_table <- renderDataTable({
    dataset %>%
      group_by(disease_group) %>%
      summarise(
        Total_YLL = sum(yll, na.rm = TRUE),
        Total_YLD = sum(yld, na.rm = TRUE),
        Total_DALY = sum(daly, na.rm = TRUE)
      ) %>%
      as.data.frame() %>%
      datatable()
  })
  
  # Boxplot for ASR by Sex in Demographic Analysis
  output$boxplot_asr <- renderPlot({
    ggplot(dataset %>% filter(data_year == input$year), aes(x = sex, y = daly_asr)) +
      geom_boxplot(aes(fill = sex)) +
      labs(title = "DALY ASR by Sex", x = "Sex", y = "DALY ASR") +
      theme_minimal()
  })
  
  # Disease Group Comparison in Demographic Analysis
  output$bar_comparison <- renderPlot({
    ggplot(dataset %>% filter(data_year == input$year, sex == input$sex_demo), 
           aes(x = disease_group, y = daly, fill = disease_group)) +
      geom_bar(stat = "identity") +
      labs(title = "DALY Comparison by Disease Group", x = "Disease Group", y = "DALY") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top 5 Conditions Table
  output$top_conditions <- renderDataTable({
    dataset %>%
      filter(data_year == input$year, sex == input$sex_demo) %>%
      arrange(desc(daly)) %>%
      head(5) %>%
      select(disease_group, yll, yld, daly) %>%
      datatable()
  })
  
  # Time Series Comparison on Page 3
  output$time_series <- renderPlot({
    ggplot(dataset %>% filter(disease_group == input$group), aes(x = data_year)) +
      geom_line(aes(y = yll_asr, color = "YLL ASR", group = 1)) +
      geom_line(aes(y = yld_asr, color = "YLD ASR", group = 1)) +
      geom_line(aes(y = daly_asr, color = "DALY ASR", group = 1)) +
      labs(title = paste("Time Series of YLL, YLD, and DALY for", input$group),
           x = "Year", y = "ASR Values") +
      theme_minimal() +
      scale_color_manual(values = c("YLL ASR" = "blue", "YLD ASR" = "green", "DALY ASR" = "red"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

