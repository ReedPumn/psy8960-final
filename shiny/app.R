library(shiny)
library(tidyverse)

# Define the application's user interface.
ui <- fluidPage(
  
  # Application title.
  titlePanel("Monthly Pay, Turnover, and Overall Satisfaction in Employees"),
  
  # Define the sidebar so that users can select the type of data to examine.
  sidebarLayout(
    sidebarPanel(
      # I originally intended to use radiobuttons, but the large number of options for the job roles variable encouraged me to use input selecters. And to keep the shiny app consistent, I used input selectors throughout. To keep data simple at first, I defaulted to keep all subsetters off upon opening the app.
      selectInput("criterionselect",
                  "What do you want to examine?",
                  c("MonthlyIncome","Attrition","OverallSatisfaction")),
      
      selectInput("deptselect",
                  "Subset by department?",
                  c("Sales", "Research & Development", "Human Resources", "All Departments"), selected = "All Departments"),
      
      selectInput("fieldselect",
                  "Subset by educational field?",
                  c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other Field", "All Fields"), selected = "All Fields"),
      
      selectInput("genderselect",
                  "Subset by gender?",
                  c("Women", "Men", "Both"), selected = "Both"),
      
      selectInput("jobroleselect",
                  "Subset by job role?",
                  c("Sales Executive", "Research Scientist", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative", "Manager", "Sales Representative", "Research Director", "Human Resources", "All Job Roles"), selected = "All Job Roles")
    ),
    
    # Show a plot of the generated distribution. This will either be a histogram or barplot, defined later in the server.
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  app_tbl <- readRDS("shiny_input.RDS")  
  output$plot <- renderPlot({
    
    # Define working tbl to iteratively modify.
    disp_app_tbl <- app_tbl
    
    # Don't need to change it if set to All Departments; otherwise filter by department selection.
    if (input$deptselect != "All Departments") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(Department == input$deptselect)
    }
    
    # Don't need to change it if set to All Fields; otherwise filter by educational field selection.
    if (input$fieldselect != "All Fields") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(EducationField == input$fieldselect)
    }
    
    # Don't need to change it if set to Both; otherwise filter by gender selection.
    if (input$genderselect != "Both") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(Gender == input$genderselect)
    }
    
    # Don't need to change it if set to All Job Roles; otherwise filter by department selection.
    if (input$jobroleselect != "All Job Roles") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(JobRole == input$jobroleselect)
    }
    
    # These three if statements filter according the the criterion selected.
    if (input$criterionselect == "MonthlyIncome") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(MonthlyIncome == input$criterionselect)
    }
    
    if (input$criterionselect == "Attrition") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(Attrition == input$criterionselect)
    }
    
    if (input$criterionselect == "OverallSatisfaction") {
      app_disp_tbl <- app_disp_tbl %>%
        filter(OverallSatisfaction == input$criterionselect)
    }
  })
  out