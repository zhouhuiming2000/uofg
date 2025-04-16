pumfid = random id
certlevp = educate level
reg_inst = region of education 
reg_resp = resident region
pgmcipap_map = 2020 program
lma010= reference week job or student?
lfcindp = sector for job Coded to the North American Industry Classification System (NAICS) 2022. reference week
lfwftptp= fulltime or parttime
annual_salary = JOBINCP
job_satisfy = lma6_13a

install.packages("plotly")

library(readr)

data = read_delim('/Users/vanris/Documents/UG -DATA6200/finalproject/5012-NGS-END_2020-CSV/ngs2020_pumf.csv',delim = ';')
head(data)

certlevp_map = c('1'='college', '2'= 'bachelor', '3'= 'masterdoc', '9'= 'no')
reg_inst_map = c('1'='atlantic', '2'= 'quebec', '3'= 'ontario', '4'='western')
reg_resp_map = c('1'='atlantic', '2'= 'quebec', '3'= 'ontario', '4'='western', '9'= 'no')
pgmcipap_map = c('1'= 'education', '2'= 'arts', '4'= 'socialscience', '5'= 'business',
                 '6'= 'lifescience', '7'= 'mcs', '8'= 'architecture', '9'= 'health', '10'= 'other', '99'= 'no')
lfstatp_map = c('1'= 'employed', '2'= 'unemployed', '3'= 'nolabourforce', '9'= 'no')
lma_010_map = c('1'= 'student', '2'= 'notstudent', '9'= 'no')
lfcindp_map = c('1'= 'goodsproducing', '2'= 'trade', '3'= 'finance', '4'= 'sciencetechnology', '5'='education',
                '6'= 'other', '7'= 'health', '96'= 'skip', '99'= 'no')

lma6_13a_map = c('1' = 'verysatisfied', '2' = 'satisfied', '3' = 'neutral', '4' = 'dissatisfied', 
                 '5' = 'verydissatisfied', '6' = 'validskip', '9' = 'no')
# LFCOCCP_map

LFWFTPTP = c('1'= 'fulltime', '2'= 'parttime', '6'='skip','9'='no')

JOBINCP_map = c('1' = 'less than 30,000', '2' = '30,000 ~ 49,999', '3' = '50,000 ~ 69,999', 
                '4' = '70,000 ~ 89,999', '5' = 'above 90,000', '96' = 'valid skip', '99' = 'no')

data$CERTLEVP = certlevp_map[data$CERTLEVP]
data$REG_INST = reg_resp_map[data$REG_INST]
data$REG_RESP = reg_resp_map[data$REG_RESP]
data$PGMCIPAP = pgmcipap_map[data$PGMCIPAP]
data$LFSTATP = lfstatp_map[data$LFSTATP]
data$LMA_010 = lma_010_map[data$LMA_010]
data$LFCINDP = lfcindp_map[data$LFCINDP]
data$LFWFTPTP = LFWFTPTP[data$LFWFTPTP]
data$LMA6_13A = lma6_13a_map[data$LMA6_13A]
data$JOBINCP = JOBINCP_map[data$JOBINCP]


names(data) = tolower(names(data))
data_lower = data[, names(data) %in% c('certlevp','reg_inst','reg_resp','pgmcipap','lfstatp',
                                       'lma_010','lfcindp','lfwftptp','lma6_13a','jobincp')]

head(data_lower)

















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
      menuItem("Employment Rate", tabName = "employment_rate", icon = icon("line-chart")),
      menuItem("Employment Distribution", tabName = "employment_distribution", icon = icon("pie-chart")),
      menuItem("Average Salaries", tabName = "average_salaries", icon = icon("bar-chart")),
      menuItem("Job Satisfaction", tabName = "job_satisfaction", icon = icon("smile-o"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(12,
             tabItems(
               tabItem(tabName = "employment_rate",
                       fluidRow(
                         column(3,
                                selectInput("filter_employment_rate", "Filter by", choices = c( "Job Status" = "lfstatp", 
                                                                                                "Resident Region" = "reg_resp",
                                                                                                "Education Region" = "reg_inst",
                                                                                                "Education Program" = "pgmcipap", 
                                                                                                "Education Level" = "certlevp",
                                                                                                "Status" = "lma_010"
                                )),
                                uiOutput("filter_employment_rate_ui")
                         ),
                         column(9,
                                box(plotOutput("employment_rate_plot"), width = 12),
                                box(plotlyOutput("employment_rate_pie"), width = 12),
                                box(plotOutput("global_histogram"), width = 12)
                         )
                       )
               ),
               tabItem(tabName = "employment_distribution",
                       fluidRow(
                         column(3,
                                selectInput("filter_employment_distribution", "Filter by", choices = c( "Job Status" = "lfstatp", 
                                                                                                        "Resident Region" = "reg_resp",
                                                                                                        "Education Region" = "reg_inst",
                                                                                                        "Education Program" = "pgmcipap", 
                                                                                                        "Education Level" = "certlevp",
                                                                                                        "Status" = "lma_010",
                                                                                                        "Job Type" = "lfwftptp",
                                                                                                        "Salaries" = "jobincp",
                                                                                                        "Job Satisfaction" = "lma6_13a")),
                                uiOutput("filter_employment_distribution_ui")
                         ),
                         column(9,
                                box(plotOutput("employment_distribution_plot"), width = 12),
                                box(plotlyOutput("employment_distribution_pie"), width = 12),
                                #box(textOutput("employment_distribution_stats"), width = 12)
                         )
                       )
               ),
               tabItem(tabName = "average_salaries",
                       fluidRow(
                         column(3,
                                selectInput("filter_average_salaries", "Filter by", choices = c( "Job Status" = "lfstatp", 
                                                                                                 "Resident Region" = "reg_resp",
                                                                                                 "Education Region" = "reg_inst",
                                                                                                 "Education Program" = "pgmcipap", 
                                                                                                 "Education Level" = "certlevp",
                                                                                                 "Status" = "lma_010",
                                                                                                 "Job Type" = "lfwftptp",
                                                                                                 "Employment" = "lfcindp",
                                                                                                 "Job Satisfaction" = "lma6_13a")),
                                uiOutput("filter_average_salaries_ui")
                         ),
                         column(9,
                                box(plotOutput("average_salaries_plot"), width = 12),
                                box(plotlyOutput("average_salaries_pie"), width = 12),
                                box(plotOutput("average_salaries_stats"), width = 12)
                         )
                       )
               ),
               tabItem(tabName = "job_satisfaction",
                       fluidRow(
                         column(3,
                                selectInput("filter_job_satisfaction", "Filter by", choices = c( "Job Status" = "lfstatp", 
                                                                                                 "Resident Region" = "reg_resp",
                                                                                                 "Education Region" = "reg_inst",
                                                                                                 "Education Program" = "pgmcipap", 
                                                                                                 "Education Level" = "certlevp",
                                                                                                 "Status" = "lma_010",
                                                                                                 "Job Type" = "lfwftptp",
                                                                                                 "Salaries" = "jobincp",
                                                                                                 "Employment" = "lfcindp"
                                )),
                                uiOutput("filter_job_satisfaction_ui")
                         ),
                         column(9,
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
  output$filter_employment_rate_ui <- renderUI({
    checkboxGroupInput("filter_employment_rate_value", "Choose values", choices = unique(data_lower[[input$filter_employment_rate]]), selected = unique(data_lower[[input$filter_employment_rate]]))
  })
  
  filtered_employment_rate_data <- reactive({
    req(input$filter_employment_rate_value)
    data_lower %>% filter(!!sym(input$filter_employment_rate) %in% input$filter_employment_rate_value)
  })
  
  output$filter_employment_distribution_ui <- renderUI({
    checkboxGroupInput("filter_employment_distribution_value", "Choose values", choices = unique(data_lower[[input$filter_employment_distribution]]), selected = unique(data_lower[[input$filter_employment_distribution]]))
  })
  
  filtered_employment_distribution_data <- reactive({
    req(input$filter_employment_distribution_value)
    data_lower %>% filter(!!sym(input$filter_employment_distribution) %in% input$filter_employment_distribution_value)
  })
  
  output$filter_average_salaries_ui <- renderUI({
    checkboxGroupInput("filter_average_salaries_value", "Choose values", choices = unique(data_lower[[input$filter_average_salaries]]), selected = unique(data_lower[[input$filter_average_salaries]]))
  })
  
  filtered_average_salaries_data <- reactive({
    req(input$filter_average_salaries_value)
    data_lower %>% filter(!!sym(input$filter_average_salaries) %in% input$filter_average_salaries_value)
  })
  
  output$filter_job_satisfaction_ui <- renderUI({
    checkboxGroupInput("filter_job_satisfaction_value", "Choose values", choices = unique(data_lower[[input$filter_job_satisfaction]]), selected = unique(data_lower[[input$filter_job_satisfaction]]))
  })
  
  filtered_job_satisfaction_data <- reactive({
    req(input$filter_job_satisfaction_value)
    data_lower %>% filter(!!sym(input$filter_job_satisfaction) %in% input$filter_job_satisfaction_value)
  })
  
  output$employment_rate_plot <- renderPlot({
    req(filtered_employment_rate_data)
    filtered_data <- filtered_employment_rate_data()
    ggplot(filtered_data, aes(x = lfstatp, fill = lfstatp)) + geom_bar() + labs(x = "Employment Status", y = "Count") + theme_minimal()
  })
  
  output$employment_distribution_plot <- renderPlot({
    req(filtered_employment_distribution_data)
    filtered_data <- filtered_employment_distribution_data()
    ggplot(filtered_data, aes(x = lfcindp, fill = lfcindp)) + geom_bar() + labs(x = "Industry", y = "Count") + theme_minimal()
  })
  
  # output$employment_distribution_stats <- renderText({
  #   req(filtered_employment_distribution_data)
  #   filtered_data <- filtered_employment_distribution_data()
  #   employment_in_same_field <- filtered_data %>% filter(pgmcipap == lfcindp) %>% nrow()
  #   total_employment <- filtered_data %>% nrow()
  #   paste("Number of people working in the same field as their program: ", employment_in_same_field, "/", total_employment)
  # })
  
  output$average_salaries_plot <- renderPlot({
    req(filtered_average_salaries_data)
    filtered_data <- filtered_average_salaries_data()
    ggplot(filtered_data, aes(x = jobincp, fill = jobincp)) + geom_bar() + labs(x = "Salary Range", y = "Count") + theme_minimal()
  })
  
  output$average_salaries_pie <- renderPlotly({
    req(filtered_average_salaries_data)
    filtered_data <- filtered_average_salaries_data()
    average_salaries_count <- filtered_data %>% count(jobincp)
    plot_ly(average_salaries_count, labels = ~jobincp, values = ~n, type = 'pie') %>%
      layout(title = 'Average Salaries Distribution', showlegend = TRUE)
  })
  
  output$average_salaries_stats <- renderPlot({
    req(filtered_average_salaries_data)
    filtered_data <- filtered_average_salaries_data() %>% 
      filter(!is.na(jobincp))
    
    # Define the order of salary groups
    salary_levels <- c('less than 30,000', '30,000 ~ 49,999', '50,000 ~ 69,999', '70,000 ~ 89,999', 'above 90,000','valid skip','no')
    filtered_data$jobincp <- factor(filtered_data$jobincp, levels = salary_levels)
    
    # Calculate statistics
    salary_counts <- table(filtered_data$jobincp)
    
    # Find median salary group
    cumulative_counts <- cumsum(salary_counts)
    total_counts <- sum(salary_counts)
    median_index <- which(cumulative_counts >= total_counts / 2)[1]
    median_salary_group <- names(salary_counts)[median_index]
    
    ggplot(filtered_data, aes(x = jobincp, fill = jobincp)) + 
      geom_bar() + 
      labs(x = "Salary Range", y = "Count") + 
      theme_minimal() +
      geom_vline(aes(xintercept = as.numeric(factor(median_salary_group, levels = salary_levels))), color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = as.numeric(factor(median_salary_group, levels = salary_levels)), y = max(salary_counts), label = paste("Median:", median_salary_group), color = "red", vjust = -1)
  })
  
  
  
  output$job_satisfaction_plot <- renderPlot({
    req(filtered_job_satisfaction_data)
    filtered_data <- filtered_job_satisfaction_data()
    ggplot(filtered_data, aes(x = lma6_13a, fill = lma6_13a)) + geom_bar() + labs(x = "Job Satisfaction", y = "Count") + theme_minimal()
  })
  
  output$employment_rate_pie <- renderPlotly({
    req(filtered_employment_rate_data)
    filtered_data <- filtered_employment_rate_data()
    employment_rate_count <- filtered_data %>% count(lfstatp)
    plot_ly(employment_rate_count, labels = ~lfstatp, values = ~n, type = 'pie') %>%
      layout(title = 'Employment Rate Distribution', showlegend = TRUE)
  })
  
  output$employment_distribution_pie <- renderPlotly({
    req(filtered_employment_distribution_data)
    filtered_data <- filtered_employment_distribution_data()
    employment_distribution_count <- filtered_data %>% count(lfcindp)
    plot_ly(employment_distribution_count, labels = ~lfcindp, values = ~n, type = 'pie') %>%
      layout(title = 'Employment Distribution by Industry', showlegend = TRUE)
  })
  
  output$job_satisfaction_pie <- renderPlotly({
    req(filtered_job_satisfaction_data)
    filtered_data <- filtered_job_satisfaction_data()
    job_satisfaction_count <- filtered_data %>% count(lma6_13a)
    plot_ly(job_satisfaction_count, labels = ~lma6_13a, values = ~n, type = 'pie') %>% 
      layout(title = 'Job Satisfaction', showlegend = TRUE)
  })
  
  output$global_histogram <- renderPlot({
    ggplot(data_lower, aes(x = pgmcipap, fill = pgmcipap)) + geom_bar() + labs(x = "Program", y = "Count") + theme_minimal()
  })
}

shinyApp(ui, server)












































library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

# Load the data_2020
data_2020 = read_delim('/Users/vanris/Documents/UG -DATA6200/finalproject/5012-NGS-END_2020-CSV/ngs2020_pumf.csv', delim = ';')
data_2015 = read_csv('/Users/vanris/Documents/UG -DATA6200/finalproject/5012-NGS-END_2015-CSV/NGS-81M0011-E-2018_F1.csv')

# data_2020 cleaning for NGS2020

# certlevp: Education_level
# reg_inst: Education_region
# reg_resp: Resident_region
# pgmcipap: Education_program
# lfstatp: Job_status
# lfcindp: Employment_area
# lma6_13a: Job_satisfaction
# LFWFTPTP: Job_type
# JOBINCP: Salaries
certlevp_map = c('1' = 'College/CEGEP', '2' = 'Bachelor', '3' = 'Master/PhD', '9' = 'NA')
reg_inst_map = c('1' = 'Atlantic', '2' = 'Quebec', '3' = 'Ontario', '4' = 'Western')
reg_resp_map = c('1' = 'Atlantic', '2' = 'Quebec', '3' = 'Ontario', '4' = 'Western', '9' = 'NA')
pgmcipap_map = c('1' = 'Education', '2' = 'Arts', '4' = 'Socialscience', '5' = 'Business', '6' = 'Lifescience', '7' = 'MCS', '8' = 'Architecture', '9' = 'Health',
                 '10' = 'Other', '99' = 'NA')
lfstatp_map = c('1' = 'Employed', '2' = 'Unemployed', '3' = 'No Labour Force', '9' = 'NA')
lfcindp_map = c('1' = 'Goods Producing', '2' = 'Trade', '3' = 'Finance', '4' = 'Science', '5' = 'Education', '6' = 'Other', '7' = 'Health', '96' = 'Skip', '99' = 'NA')
lma6_13a_map = c('1' = 'Very satisfied', '2' = 'Satisfied', '3' = 'Neutral', '4' = 'Dissatisfied', '5' = 'Very dissatisfied', '6' = 'Valid Skip', '9' = 'NA')
LFWFTPTP = c('1' = 'Full-time', '2' = 'Part-time', '6' = 'Skip','9' = 'NA')
JOBINCP_map = c('1' = 'less than 30,000', '2' = '30,000 ~ 49,999', '3' = '50,000 ~ 69,999', '4' = '70,000 ~ 89,999', '5' = 'above 90,000', '96' = 'Valid Skip', '99' = 'no')
salary_levels <- c('less than 30,000', '30,000 ~ 49,999', '50,000 ~ 69,999', '70,000 ~ 89,999', 'above 90,000','no','valid skip')

data_2020$CERTLEVP = certlevp_map[data_2020$CERTLEVP]
data_2020$REG_INST = reg_inst_map[data_2020$REG_INST]
data_2020$REG_RESP = reg_resp_map[data_2020$REG_RESP]
data_2020$PGMCIPAP = pgmcipap_map[data_2020$PGMCIPAP]
data_2020$LFSTATP = lfstatp_map[data_2020$LFSTATP]
data_2020$LFCINDP = lfcindp_map[data_2020$LFCINDP]
data_2020$LFWFTPTP = LFWFTPTP[data_2020$LFWFTPTP]
data_2020$LMA6_13A = lma6_13a_map[data_2020$LMA6_13A]
data_2020$JOBINCP = JOBINCP_map[data_2020$JOBINCP]
data_2020$JOBINCP = factor(data_2020$JOBINCP, levels = salary_levels)

names(data_2020) = tolower(names(data_2020))
data_2020 = data_2020[, names(data_2020) %in% c('certlevp', 'reg_inst', 'reg_resp', 'pgmcipap', 'lfstatp', 'lfcindp', 'lfwftptp', 'lma6_13a', 'jobincp')]
col_names = c('Education_level', 'Education_region', 'Resident_region', 'Education_program', 'Job_status', 'Employment_area', 'Job_type', 'Job_satisfaction',
              'Salaries')
colnames(data_2020) <- col_names

# Data cleaning for NSG2015

# certlevp: Education_level
# reg_inst: Education_region
# reg_resp: Resident_region
# pgmcipap: Education_program
# lfstat: Job_status
# lfcindp: Employment_area
# LFW_420A: Job_satisfaction
# AFT_050: Job_type
# JOBINCP: Salaries
certlevp_map = c('1' = 'College/CEGEP', '2' = 'Bachelor', '3' = 'Master/PhD', '9' = 'NA')
reg_inst_map = c('1' = 'Atlantic', '2' = 'Quebec', '3' = 'Ontario', '4' = 'Western')
reg_resp_map = c('1' = 'Atlantic', '2' = 'Quebec', '3' = 'Ontario', '4' = 'Western', '9' = 'NA')
pgmcipap_map = c('1' = 'Education', '2' = 'Arts', '4' = 'Socialscience', '5' = 'Business', '6' = 'Lifescience', '7' = 'MCS', '8' = 'Architecture', '9' = 'Health',
                 '10' = 'Other', '99' = 'NA')
lfstat_map = c('1' = 'Employed', '2' = 'Unemployed', '3' = 'No Labour Force', '9' = 'NA')
lfcindp_map = c('1' = 'Goods Producing', '2' = 'Trade', '3' = 'Finance', '4' = 'Science', '5' = 'Education', '6' = 'Other', '7' = 'Health', '96' = 'Skip', '99' = 'NA')
lfw_420a = c('1' = 'Very satisfied', '2' = 'Satisfied', '3' = 'Neutral', '4' = 'Dissatisfied', '5' = 'Very dissatisfied', '6' = 'Valid Skip', '9' = 'NA')
aft_050 = c('1' = 'Full-time', '2' = 'Part-time', '6' = 'Skip','9' = 'NA')
JOBINCP_map = c('1' = 'less than 10,000', '1' = '10,000 ~ 19,999', '1' = '20,000 ~ 29,999', '2' = '30,000 ~ 39,999', '2' = '40,000 ~ 49,999', '3' = '50,000 ~ 59,999',
                '3' = '60,000 ~ 69,999', '4' = '70,000 ~ 79,999', '4' = '80,000 ~ 89,999', '5' = 'above 90,000', '96' = 'Valid Skip', '99' = 'no')
salary_levels <- c('less than 10,000', '10,000 ~ 19,999', '20,000 ~ 29,999', '30,000 ~ 39,999', '40,000 ~ 49,999', '50,000 ~ 59,999', '60,000 ~ 69,999', '70,000 ~ 79,999',
                   '80,000 ~ 89,999', 'above 90,000','no','valid skip')

data_2015$CERTLEVP = certlevp_map[data_2015$CERTLEVP]
data_2015$REG_INST = reg_inst_map[data_2015$REG_INST]
data_2015$REG_RESP = reg_resp_map[data_2015$REG_RESP]
data_2015$PGMCIPAP = pgmcipap_map[data_2015$PGMCIPAP]
data_2015$LFSTAT = lfstat_map[data_2015$LFSTAT]
data_2015$LFCINDP = lfcindp_map[data_2015$LFCINDP]
data_2015$LFW_420A = lfw_420a[data_2015$LFW_420A]
data_2015$AFT_050 = aft_050[data_2015$AFT_050]
data_2015$JOBINCP = JOBINCP_map[data_2015$JOBINCP]
data_2015$JOBINCP = factor(data_2015$JOBINCP, levels = salary_levels)

names(data_2015) = tolower(names(data_2015))
data_2015 = data_2015[, names(data_2015) %in% c('certlevp', 'reg_inst', 'reg_resp', 'pgmcipap', 'lfstat', 'lfcindp', 'lfw_420a', 'aft_050', 'jobincp')]
col_names = c('Job_satisfaction', 'Job_type', 'Resident_region', 'Education_level', 'Education_program', 'Job_status', 'Employment_area',
              'Salaries', 'Education_region')
colnames(data_2015) <- col_names

# Pair up the columns of two dataframes
common_columns <- intersect(names(data_2020), names(data_2015))
data_2020 <- data_2020[, common_columns]
data_2015 <- data_2015[, common_columns]

# Combine two dataframes
data_2020$year <- 2020
data_2015$year <- 2015
combined_data <- rbind(data_2020, data_2015)

###################################################################################################

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Graduation Employement Analysis", titleWidth = 330),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("line-chart")),
      menuItem("Industry", tabName = "Industry", icon = icon("industry")),
      menuItem("Salary", tabName = "Salary", icon = icon("money-bill")),
      menuItem("Location", tabName = "Location", icon = icon("location-dot"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Overview",
              
              # Dropdown Menu for Year Selection
              fluidRow(
                column(4,
                       selectInput("overview_year_choice", "Select Year", choices = c(2015, 2020), selected = 2020)
                )
              ),
              
              # fluidRow of 3 valueBoxOutput
              fluidRow(
                valueBoxOutput('num_graduates', width = 4),
                valueBoxOutput("most_salary", width = 4),
                valueBoxOutput("total_employe_rate", width = 4)
              ),
              
              fluidRow(
                column(6,
                       box(plotlyOutput("overall_pie"), width = 12)
                ),
                column(6,
                       box(plotlyOutput("global_histogram"), width = 12)
                )
              )
      ),
      tabItem(tabName = "Industry",
              
              # Dropdown Menu for Year Selection
              fluidRow(
                column(4,
                       selectInput("industry_year_choice", "Select Year", choices = c(2015, 2020), selected = 2020)
                )
              ),
              
              # fluidRow two industry plots
              fluidRow(
                column(6,
                       box(plotlyOutput("industry_barplot"), width = 12)
                ),
                column(6,
                       box(plotlyOutput("industry_pie"), width = 12)
                )
              )
      ),
      tabItem(tabName = "Salary",
              
              # Dropdown Menu for Year Selection
              fluidRow(
                column(4,
                       selectInput("salary_year_choice", "Select Year", choices = c(2015, 2020), selected = 2020)
                )
              ),
              
              # fluidRow two salary plots
              fluidRow(
                column(6,
                       box(plotlyOutput("salary_histogram"), width = 12)
                ),
                column(6,
                       box(plotlyOutput("salary_scatter"), width = 12)
                )
              )
      ),
      tabItem(tabName = "Location"
              
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  ####################
  # Overview UI Page #
  ####################
  
  # Filter data
  overview_filtered_data <- reactive({
    req(input$overview_year_choice)
    combined_data |> filter(year == input$overview_year_choice)
  })
  
  # ：Number of Graduates
  output$num_graduates <- renderValueBox({
    valueBox(nrow(overview_filtered_data()), "Number of Graduates", icon = icon("users"), color = "aqua")
  })
  
  # Most of total Salary
  output$most_salary <- renderValueBox({
    most_common_salary <- names(which.max(table(overview_filtered_data()$Salaries)))
    valueBox(most_common_salary, "Most of Annual Salary(CAD)", icon = icon("dollar-sign"), color = "green")
  })
  
  # Employment Rate in Total
  output$total_employe_rate <- renderValueBox({
    employment_rate <- round((sum(overview_filtered_data()$Job_status == "Employed", na.rm = TRUE) / nrow(overview_filtered_data())) * 100, 2)
    valueBox(
      paste(employment_rate, "%"),
      "Total Employment Rate",
      icon = icon("check"),
      color = "light-blue"
    )
  })
  
  # Employment Rate pie chart
  output$overall_pie <- renderPlotly({
    employment_rate_count <- overview_filtered_data() |> filter(!is.na(Job_status)) |> count(Job_status)
    plot_ly(
      employment_rate_count,
      labels = ~Job_status,
      values = ~n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) |>
      layout(
        title = paste(input$overview_year_choice, 'Employment Distribution'),
        showlegend = TRUE
      )
  })
  
  # Global Histogram
  output$global_histogram <- renderPlotly({
    p <- ggplot(overview_filtered_data(), aes(x = Education_program, fill = Education_program)) + 
      geom_bar() +
      labs(y = "Count", x = 'Program') +
      theme_minimal() +
      theme(axis.text.x = element_blank())
    ggplotly(p, tooltip = "y")
  })
  
  ####################
  # Industry UI Page #
  ####################
  
  # Filter data
  industry_filtered_data <- reactive({
    req(input$industry_year_choice)
    combined_data |> filter(year == input$industry_year_choice)
  })
  
  # Industry Bar plot
  output$industry_barplot <- renderPlotly({
    req(industry_filtered_data())
    
    # create stack data
    salary_stack_data <- industry_filtered_data() |>
      count(Employment_area, Salaries)
    
    plot_ly(
      salary_stack_data,
      x = ~Employment_area,
      y = ~n,
      color = ~Salaries, 
      type = 'bar'
    ) |>
      layout(
        barmode = 'stack',       # Change to stack mode
        title = paste("Employment by Industry and Salary Range in", input$industry_year_choice),
        xaxis = list(title = "Industry"),
        yaxis = list(title = "Count"),
        legend = list(title = list(text = "Salary Range (CAD)"))
      )
  })
  
  # Industry Pie Chart
  output$industry_pie <- renderPlotly({
    industry_distribution <- industry_filtered_data() |>
      count(Employment_area)
    plot_ly(industry_distribution, 
            labels = ~Employment_area, 
            values = ~n, 
            type = 'pie') |>
      layout(title = "Industry Distribution")
  })
  
  ##################
  # Salary UI Page #
  ##################
  
  # Filter data
  salary_filtered_data <- reactive({
    req(input$salary_year_choice)
    combined_data |> filter(year == input$salary_year_choice)
  })
  
  # Salary histogram
  output$salary_histogram <- renderPlotly({
    plot_ly(salary_filtered_data(), 
            x = ~Education_level, 
            type = 'histogram') |>
      layout(title = "Salary Distribution",
             xaxis = list(title = "Salary (CAD)"),
             yaxis = list(title = "Count"))
  })
  
  # Salary Scatter plot
  output$salary_scatter <- renderPlotly({
    plot_ly(
      salary_filtered_data(), 
      x = ~Education_level, 
      y = ~Salaries, 
      mode = 'markers', 
      color = ~Employment_area,
      colorbar = list(
        tickmode = "array",  # Customize mode
        tickvals = NULL     # Hide Heatmap text scale
      )
    ) |>
      layout(
        title = "Salary vs. Degree",
        xaxis = list(title = "Degree"),
        yaxis = list(title = "Salary (CAD)")
      )
  })
  
  ####################
  # Location UI Page #
  ####################
  
  
  
  ####################
  #     MDS Page     #
  ####################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
shinyApp(ui, server)