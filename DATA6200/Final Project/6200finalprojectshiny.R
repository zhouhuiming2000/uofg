library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

# Load the data
data = read_delim('/Users/vanris/Documents/UG -DATA6200/finalproject/5012-NGS-END_2020-CSV/ngs2020_pumf.csv', delim = ';')

certlevp_map = c('1' = 'college', '2' = 'bachelor', '3' = 'masterdoc', '9' = 'no')
reg_inst_map = c('1' = 'atlantic', '2' = 'quebec', '3' = 'ontario', '4' = 'western')
reg_resp_map = c('1' = 'atlantic', '2' = 'quebec', '3' = 'ontario', '4' = 'western', '9' = 'no')
pgmcipap_map = c('1' = 'education', '2' = 'arts', '4' = 'socialscience', '5' = 'business',
                 '6' = 'lifescience', '7' = 'mcs', '8' = 'architecture', '9' = 'health', '10' = 'other', '99' = 'no')
lfstatp_map = c('1' = 'employed', '2' = 'unemployed', '3' = 'nolabourforce', '9' = 'no')
lma_010_map = c('1' = 'student', '2' = 'notstudent', '9' = 'no')
lfcindp_map = c('1' = 'goodsproducing', '2' = 'trade', '3' = 'finance', '4' = 'sciencetechnology', '5' = 'education',
                '6' = 'other', '7' = 'health', '96' = 'skip', '99' = 'no')
lma6_13a_map = c('1' = 'verysatisfied', '2' = 'satisfied', '3' = 'neutral', '4' = 'dissatisfied', 
                 '5' = 'verydissatisfied', '6' = 'validskip', '9' = 'no')
LFWFTPTP = c('1' = 'fulltime', '2' = 'parttime', '6' = 'skip', '9' = 'no')
JOBINCP_map = c('1' = 'less than 30,000', '2' = '30,000 ~ 49,999', '3' = '50,000 ~ 69,999', 
                '4' = '70,000 ~ 89,999', '5' = 'above 90,000', '96' = 'valid skip', '99' = 'no')
salary_levels <- c('less than 30,000', '30,000 ~ 49,999', '50,000 ~ 69,999', '70,000 ~ 89,999', 'above 90,000','no','valid skip')

data$CERTLEVP = certlevp_map[data$CERTLEVP]
data$REG_INST = reg_inst_map[data$REG_INST]
data$REG_RESP = reg_resp_map[data$REG_RESP]
data$PGMCIPAP = pgmcipap_map[data$PGMCIPAP]
data$LFSTATP = lfstatp_map[data$LFSTATP]
data$LMA_010 = lma_010_map[data$LMA_010]
data$LFCINDP = lfcindp_map[data$LFCINDP]
data$LFWFTPTP = LFWFTPTP[data$LFWFTPTP]
data$LMA6_13A = lma6_13a_map[data$LMA6_13A]
data$JOBINCP = JOBINCP_map[data$JOBINCP]
data$JOBINCP = factor(data$JOBINCP, levels = salary_levels)

names(data) = tolower(names(data))
data_lower = data[, names(data) %in% c('certlevp', 'reg_inst', 'reg_resp', 'pgmcipap', 'lfstatp',
                                       'lma_010', 'lfcindp', 'lfwftptp', 'lma6_13a', 'jobincp')]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Employment Insights", tabName = "employment_insights", icon = icon("line-chart")),
      menuItem("Salary & Satisfaction", tabName = "salary_satisfaction", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(12,
             tabItems(
               tabItem(tabName = "employment_insights",
                       fluidRow(
                         column(3,
                                selectInput("filter_employment_insights", "Filter by", choices = c( "Job Status" = "lfstatp", 
                                                                                                    "Resident Region" = "reg_resp",
                                                                                                    "Education Region" = "reg_inst",
                                                                                                    "Education Program" = "pgmcipap", 
                                                                                                    "Education Level" = "certlevp",
                                                                                                    "Status" = "lma_010"
                                )),
                                uiOutput("filter_employment_insights_ui")
                         ),
                         column(9,
                                box(plotOutput("employment_rate_plot"), width = 12),
                                box(plotlyOutput("employment_rate_pie"), width = 12),
                                box(plotOutput("employment_distribution_plot"), width = 12),
                                box(plotlyOutput("employment_distribution_pie"), width = 12)
                         )
                       )
               ),
               tabItem(tabName = "salary_satisfaction",
                       fluidRow(
                         column(3,
                                selectInput("filter_salary_satisfaction", "Filter by", choices = c( "Job Status" = "lfstatp", 
                                                                                                    "Resident Region" = "reg_resp",
                                                                                                    "Education Region" = "reg_inst",
                                                                                                    "Education Program" = "pgmcipap", 
                                                                                                    "Education Level" = "certlevp",
                                                                                                    "Status" = "lma_010",
                                                                                                    "Job Type" = "lfwftptp",
                                                                                                    "Employment" = "lfcindp",
                                                                                                    "Job Satisfaction" = "lma6_13a")),
                                uiOutput("filter_salary_satisfaction_ui")
                         ),
                         column(9,
                                box(plotOutput("average_salaries_plot"), width = 12),
                                box(plotlyOutput("average_salaries_pie"), width = 12),
                                box(plotOutput("average_salaries_stats"), width = 12),
                                box(plotOutput("job_satisfaction_plot"), width = 12),
                                box(plotlyOutput("job_satisfaction_pie"), width = 12)
                         )
                       )
               )
             )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$filter_employment_insights_ui <- renderUI({
    checkboxGroupInput("filter_employment_insights_value", "Choose values", choices = unique(data_lower[[input$filter_employment_insights]]), selected = unique(data_lower[[input$filter_employment_insights]]))
  })
  
  filtered_employment_insights_data <- reactive({
    req(input$filter_employment_insights_value)
    data_lower %>% filter(!!sym(input$filter_employment_insights) %in% input$filter_employment_insights_value)
  })
  
  output$filter_salary_satisfaction_ui <- renderUI({
    checkboxGroupInput("filter_salary_satisfaction_value", "Choose values", choices = unique(data_lower[[input$filter_salary_satisfaction]]), selected = unique(data_lower[[input$filter_salary_satisfaction]]))
  })
  
  filtered_salary_satisfaction_data <- reactive({
    req(input$filter_salary_satisfaction_value)
    data_lower %>% filter(!!sym(input$filter_salary_satisfaction) %in% input$filter_salary_satisfaction_value)
  })
  
  output$employment_rate_plot <- renderPlot({
    req(filtered_employment_insights_data)
    filtered_data <- filtered_employment_insights_data()
    ggplot(filtered_data, aes(x = lfstatp, fill = lfstatp)) + geom_bar() + labs(x = "Employment Status", y = "Count") + theme_minimal()
  })
  
  output$employment_distribution_plot <- renderPlot({
    req(filtered_employment_insights_data)
    filtered_data <- filtered_employment_insights_data()
    ggplot(filtered_data, aes(x = lfcindp, fill = lfcindp)) + geom_bar() + labs(x = "Industry", y = "Count") + theme_minimal()
  })
  
  output$average_salaries_plot <- renderPlot({
    req(filtered_salary_satisfaction_data)
    filtered_data <- filtered_salary_satisfaction_data()
    ggplot(filtered_data, aes(x = jobincp, fill = jobincp)) + geom_bar() + labs(x = "Salary Range", y = "Count") + theme_minimal()
  })
  
  output$average_salaries_pie <- renderPlotly({
    req(filtered_salary_satisfaction_data)
    filtered_data <- filtered_salary_satisfaction_data()
    average_salaries_count <- filtered_data %>% count(jobincp)
    plot_ly(average_salaries_count, labels = ~jobincp, values = ~n, type = 'pie') %>%
      layout(title = 'Salary Distribution')
  })
  
  output$average_salaries_stats <- renderPlot({
    req(filtered_salary_satisfaction_data)
    filtered_data <- filtered_salary_satisfaction_data()
    ggplot(filtered_data, aes(x = jobincp, y = lma6_13a, fill = jobincp)) + geom_boxplot() + labs(x = "Salary Range", y = "Job Satisfaction") + theme_minimal()
  })
  
  output$job_satisfaction_plot <- renderPlot({
    req(filtered_salary_satisfaction_data)
    filtered_data <- filtered_salary_satisfaction_data()
    ggplot(filtered_data, aes(x = lma6_13a, fill = lma6_13a)) + geom_bar() + labs(x = "Job Satisfaction", y = "Count") + theme_minimal()
  })
  
  output$employment_rate_pie <- renderPlotly({
    req(filtered_employment_insights_data)
    filtered_data <- filtered_employment_insights_data()
    employment_rate_count <- filtered_data %>% count(lfstatp)
    plot_ly(employment_rate_count, labels = ~lfstatp, values = ~n, type = 'pie') %>%
      layout(title = 'Employment Rate')
  })
  
  output$employment_distribution_pie <- renderPlotly({
    req(filtered_employment_insights_data)
    filtered_data <- filtered_employment_insights_data()
    employment_distribution_count <- filtered_data %>% count(lfcindp)
    plot_ly(employment_distribution_count, labels = ~lfcindp, values = ~n, type = 'pie') %>%
      layout(title = 'Employment Distribution')
  })
  
  output$job_satisfaction_pie <- renderPlotly({
    req(filtered_salary_satisfaction_data)
    filtered_data <- filtered_salary_satisfaction_data()
    job_satisfaction_count <- filtered_data %>% count(lma6_13a)
    plot_ly(job_satisfaction_count, labels = ~lma6_13a, values = ~n, type = 'pie') %>%
      layout(title = 'Job Satisfaction')
  })
}

shinyApp(ui, server)
