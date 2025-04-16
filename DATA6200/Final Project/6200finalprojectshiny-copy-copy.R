library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(scales)

# Load the data_2020
# Load the data_2020
data_2020 = read_delim('/Users/vanris/Documents/UG -DATA6200/finalproject/5012-NGS-END_2020-CSV/ngs2020_pumf.csv', delim = ';')
data_2015 = read_csv('/Users/vanris/Documents/UG -DATA6200/finalproject/5012-NGS-END_2015-CSV/NGS-81M0011-E-2018_F1.csv')

######################## Data cleaning for NSG2020 ########################

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

######################## Data cleaning for NSG2015 ########################
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
salary_levels <- c('less than 30,000', '30,000 ~ 49,999', '50,000 ~ 69,999', '70,000 ~ 89,999', 'above 90,000','no','valid skip')
JOBINCP_map = c('1' = 'less than 30,000','2' = 'less than 30,000','3' = 'less than 30,000','4' = '30,000 ~ 49,999','5' = '30,000 ~ 49,999',
                '6' = '50,000 ~ 69,999','7' = '50,000 ~ 69,999', '8' = '70,000 ~ 89,999', '9' ='70,000 ~ 89,999', '10' = 'above 90,000','96' = 'Valid Skip', '99' = 'no')

data_2015$CERTLEVP = certlevp_map[data_2015$CERTLEVP]
data_2015$REG_INST = reg_inst_map[data_2015$REG_INST]
data_2015$REG_RESP = reg_resp_map[data_2015$REG_RESP]
data_2015$PGMCIPAP = pgmcipap_map[data_2015$PGMCIPAP]
data_2015$LFSTATP = lfstat_map[data_2015$LFSTAT]
data_2015$LFCINDP = lfcindp_map[data_2015$LFCINDP]
data_2015$LFW_420A = lfw_420a[data_2015$LFW_420A]
data_2015$AFT_050 = aft_050[data_2015$AFT_050]
data_2015$JOBINCP = JOBINCP_map[data_2015$JOBINCP]
data_2015$JOBINCP = factor(data_2015$JOBINCP, levels = salary_levels)

names(data_2015) = tolower(names(data_2015))
data_2015 = data_2015[, names(data_2015) %in% c('certlevp', 'reg_inst', 'reg_resp', 'pgmcipap', 'lfstatp', 'lfcindp', 'lfw_420a', 'aft_050', 'jobincp')]
col_names = c('Job_satisfaction', 'Job_type', 'Resident_region', 'Education_level', 'Education_program', 'Employment_area', 'Salaries',
              'Education_region', 'Job_status')
colnames(data_2015) <- col_names

# Pair up the columns of two dataframes
common_columns <- intersect(names(data_2020), names(data_2015))
data_2020 <- data_2020[, common_columns]
data_2015 <- data_2015[, common_columns]

######################## Combine two dataframes ########################
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
      menuItem("Employment Insights", tabName = "employment_insights", icon = icon("line-chart")),
      menuItem("Salary & Satisfaction", tabName = "salary_satisfaction", icon = icon("bar-chart"))
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
      tabItem(tabName = "employment_insights",
              fluidRow(
                column(3,
                       uiOutput("year_employment_insights_ui"),
                       selectInput("filter_employment_insights", "Filter by", choices = c(
                         "Resident Region" = "Resident_region",
                         "Education Region" = "Education_region",
                         "Education Program" = "Education_program", 
                         "Education Level" = "Education_level"
                       )),
                       uiOutput("filter_employment_insights_ui")
                ),
                column(9,
                       box(plotlyOutput("employment_rate_plot"), width = 12),
                       box(plotlyOutput("employment_distribution_plot"), width = 12),
                       box(plotlyOutput("employment_distribution_pie"), width = 12)
                )
              )
      ),
      tabItem(tabName = "salary_satisfaction",
              fluidRow(
                column(3,
                       # selectInput("year_salary_satisfaction", "Select Year", choices = c("Years" = "year")),
                       uiOutput("year_salary_satisfaction_ui"),
                       selectInput("filter_salary_satisfaction", "Filter by", choices = c( 
                         "Resident Region" = "Resident_region",
                         "Education Region" = "Education_region",
                         "Education Program" = "Education_program", 
                         "Education Level" = "Education_level",
                         "Industry" = "Employment_area"                                         
                       )),
                       uiOutput("filter_salary_satisfaction_ui")
                ),
                column(9,
                       box(plotlyOutput("average_salaries_plot"), width = 12),
                       box(plotlyOutput("average_salaries_pie"), width = 12),
                       box(plotlyOutput("job_satisfaction_plot"), width = 12),
                       box(plotlyOutput("job_satisfaction_pie"), width = 12)
                )
              )
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
    local_data = combined_data
    local_data  |> filter(year == input$overview_year_choice)
  })
  
  # Number of Graduates
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
  # output$overall_pie <- renderPlotly({
  #   employment_rate_count <- overview_filtered_data()  |> filter(!is.na(Job_status))  |> count(Job_status)
  #   plot_ly(
  #     employment_rate_count,
  #     labels = ~Job_status,
  #     values = ~n,
  #     type = 'pie',
  #     textinfo = 'label+percent',
  #     insidetextorientation = 'radial'
  #   )  |>
  #     layout(
  #       title = paste(input$overview_year_choice, 'Employment Distribution'),
  #       showlegend = TRUE
  #     )
  # })
  output$overall_pie <- renderPlotly({
    employment_rate_count <- overview_filtered_data() %>%
      filter(!is.na(Job_status)) %>%
      count(Job_status)
    
    plot_ly(
      employment_rate_count,
      labels = ~Job_status,
      values = ~n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      textfont = list(size = 12)
    ) %>%
      layout(
        title = list(
          text = paste(input$overview_year_choice, 'Employment Distribution'),
          x = 0.5,
          y = 0.95
        ),
        margin = list(t = 100, b = 50, l = 50, r = 50),
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
  
  ###############################
  # Employment Insights UI Page #
  ###############################
  
  # Render the year checkbox group
  output$year_employment_insights_ui <- renderUI({
    req(combined_data)
    checkboxGroupInput("year_employment_insights_value",
                       "Choose years",
                       choices = unique(combined_data$year),
                       selected = unique(combined_data$year))
  })
  
  # Filter data based on selected years
  year_employment_insights_data <- reactive({
    req(input$year_employment_insights_value)
    filtered_data <- combined_data |>
      filter(year %in% input$year_employment_insights_value)
    return(filtered_data)
  })
  
  # Render the filter options dynamically
  output$filter_employment_insights_ui <- renderUI({
    req(input$filter_employment_insights)
    
    # Ensure the correct column is used for filtering
    choices_column <- input$filter_employment_insights
    
    checkboxGroupInput("filter_employment_insights_value", 
                       "Choose values", 
                       choices = unique(year_employment_insights_data()[[choices_column]]), 
                       selected = unique(year_employment_insights_data()[[choices_column]]))
  })
  
  # Filter data based on selected filter options
  filtered_employment_insights_data <- reactive({
    req(input$filter_employment_insights_value)
    
    filtered_data <- year_employment_insights_data() |>
      filter(!!sym(input$filter_employment_insights) %in% input$filter_employment_insights_value)
    
    return(filtered_data)
  })
  
  output$employment_rate_plot <- renderPlotly({
    req(filtered_employment_insights_data())  # Ensure data is available
    filtered_data <- filtered_employment_insights_data()
    
    # Summarize data: Count total people per group and calculate proportions of employment status
    summarized_data <- filtered_data |>
      group_by_at(input$filter_employment_insights) |>
      count(Job_status) |>
      group_by_at(input$filter_employment_insights) |>
      mutate(proportion = n / sum(n))  # Calculate the proportion
    
    # Create the plotly bar plot
    plot_ly(
      data = summarized_data,
      x = ~get(input$filter_employment_insights),
      y = ~n,
      color = ~Job_status,
      type = 'bar',
      text = ~paste0("Proportion: ", scales::percent(proportion, accuracy = 0.1)),
      textposition = 'outside',
      hoverinfo = 'x+y+text+name'
    ) |>
      layout(
        barmode = 'stack',
        xaxis = list(title = input$filter_employment_insights, tickangle = 45),
        yaxis = list(title = 'Total Count'),
        title = 'Employment Rate'
      )
  })
  
  output$employment_distribution_plot <- renderPlotly({
    req(filtered_employment_insights_data())
    filtered_data <- filtered_employment_insights_data()
    
    # Summarize data: Count total people per group
    summarized_data <- filtered_data |>
      group_by(Employment_area, Job_type) |>
      summarise(total_count = n(), .groups = 'drop')
    
    # Create the plotly bar plot
    plot_ly(
      data = summarized_data,
      x = ~Employment_area,
      y = ~total_count,
      color = ~Job_type,
      type = 'bar',
      hoverinfo = 'x+y+name'
    ) |>
      layout(
        barmode = 'stack',
        xaxis = list(title = "Employment Area", tickangle = 45),
        yaxis = list(title = "Total Count"),
        title = 'Employment Distribution'
      )
  })
  
  output$employment_distribution_pie <- renderPlotly({
    req(filtered_employment_insights_data)
    filtered_data <- filtered_employment_insights_data()
    employment_distribution_count <- filtered_data |>count(Employment_area)
    plot_ly(employment_distribution_count, labels = ~Employment_area, values = ~n, type = 'pie')  |>
      layout(title = 'Employment Distribution')
  })
  
  #################################
  # Salary & Satisfaction UI Page #
  #################################
  # Render UI for year selection in Salary and Satisfaction tab
  output$year_salary_satisfaction_ui <- renderUI({
    req(combined_data)
    checkboxGroupInput("year_salary_satisfaction_value", "Choose years", 
                       choices = unique(combined_data$year), 
                       selected = unique(combined_data$year))
  })
  
  # Filter data based on year selection
  year_salary_satisfaction_data <- reactive({
    req(input$year_salary_satisfaction_value)
    filtered_data <- combined_data |>
      filter(year %in% input$year_salary_satisfaction_value,
             Job_status == "Employed")  # Only keep Employed data
    return(filtered_data)
  })
  
  # Render filter options for salary and satisfaction
  output$filter_salary_satisfaction_ui <- renderUI({
    req(input$filter_salary_satisfaction)
    choices_column <- input$filter_salary_satisfaction
    
    checkboxGroupInput("filter_salary_satisfaction_value", 
                       "Choose values", 
                       choices = unique(year_salary_satisfaction_data()[[choices_column]]), 
                       selected = unique(year_salary_satisfaction_data()[[choices_column]]))
  })
  
  # Apply salary and satisfaction filter
  filtered_salary_satisfaction_data <- reactive({
    req(input$filter_salary_satisfaction_value)
    filtered_data <- year_salary_satisfaction_data() |>
      filter(!!sym(input$filter_salary_satisfaction) %in% input$filter_salary_satisfaction_value)
    return(filtered_data)
  })
  
  # Render plot for average salaries
  output$average_salaries_plot <- renderPlotly({
    req(filtered_salary_satisfaction_data())
    filtered_data <- filtered_salary_satisfaction_data() |>
      filter(!is.na(Salaries))
    
    summarized_data <- filtered_data |>
      group_by_at(input$filter_salary_satisfaction) |>
      count(Job_status, Salary = Salaries)
    
    plot_ly(
      data = summarized_data,
      x = ~get(input$filter_salary_satisfaction),
      y = ~n,
      color = ~Salary,
      type = 'bar',
      hoverinfo = 'x+y+name'
    ) |>
      layout(
        xaxis = list(title = input$filter_salary_satisfaction, tickangle = 45),
        yaxis = list(title = "Total Count"),
        title = 'Average Salaries'
      )
  })
  
  # Job Satisfaction Pie Chart
  output$job_satisfaction_pie <- renderPlotly({
    req(filtered_salary_satisfaction_data())
    filtered_data <- filtered_salary_satisfaction_data() |>
      filter(!is.na(Job_satisfaction)) |>
      filter(Job_satisfaction != 'Valid Skip')
    
    job_satisfaction_count <- filtered_data  |>
      count(Job_satisfaction) 
    
    plot_ly(
      job_satisfaction_count,
      labels = ~Job_satisfaction,
      values = ~n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    )  |>
      layout(
        title = paste(input$year_salary_satisfaction_value, 'Job Satisfaction Distribution'),
        showlegend = TRUE
      )
  })
  
  # Job Satisfaction Plot
  output$job_satisfaction_plot <- renderPlotly({
    req(filtered_salary_satisfaction_data())
    filtered_data <- filtered_salary_satisfaction_data() |>
      filter(!is.na(Job_satisfaction)) |>
      filter(Job_satisfaction != 'Valid Skip')
    
    summarized_satisfaction <- filtered_data |>
      group_by_at(input$filter_salary_satisfaction) |>
      count(Job_satisfaction)
    
    plot_ly(
      data = summarized_satisfaction,
      x = ~get(input$filter_salary_satisfaction),
      y = ~n,
      color = ~Job_satisfaction,
      type = 'bar',
      hoverinfo = 'x+y+name'
    ) |>
      layout(
        xaxis = list(title = input$filter_salary_satisfaction, tickangle = 45),
        yaxis = list(title = "Total Count"),
        title = 'Job Satisfaction'
      )
  })
  
  output$average_salaries_pie <- renderPlotly({
    req(filtered_salary_satisfaction_data())
    filtered_data <- filtered_salary_satisfaction_data()
    
    salary_count <- filtered_data  |>
      filter(!is.na(Salaries))  |>
      count(Salary = Salaries)
    
    plot_ly(
      salary_count,
      labels = ~Salary,
      values = ~n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    )  |>
      layout(
        title = paste(input$year_salary_satisfaction_value, 'Salary Distribution'),
        showlegend = TRUE
      )
  })
}

shinyApp(ui, server)