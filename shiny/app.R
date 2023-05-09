library(shiny)
library(tidyverse)

# Define the application's user interface.
ui <- fluidPage(
  
  # Application title.
  titlePanel("Monthly Pay, Turnover, and Job Satisfaction in Employees"),
  # I added a subtitle here to give the reader context to the shiny app.
  tags$h4("Please note: If no data are graphed, then no data exist with the given filters. For Attrition, 0 means still working at the organization; 1 means no longer working at the organization."),
  
  # Define the sidebar so that users can select the type of data to examine.
  sidebarLayout(
    sidebarPanel(
      # I originally intended to use radiobuttons, but the large number of options for the job roles variable encouraged me to use input selecters. And to keep the shiny app consistent, I used input selectors throughout. To keep data simple at first, I defaulted to keep all subsetters off upon opening the app.
      selectInput("criterionselect",
                  "What do you want to examine?",
                  c("Monthly Income","Attrition","Job Satisfaction")),
      
      selectInput("deptselect",
                  "Subset by department?",
                  c("Sales", "Research & Development", "Human Resources", "All Departments"), selected = "All Departments"),
      
      selectInput("fieldselect",
                  "Subset by educational field?",
                  c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other", "All Fields"), selected = "All Fields"),
      
      selectInput("genderselect",
                  "Subset by gender?",
                  c("Female", "Male", "Both"), selected = "Both"),
      
      selectInput("jobroleselect",
                  "Subset by job role?",
                  c("Sales Executive", "Research Scientist", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative", "Manager", "Sales Representative", "Research Director", "Human Resources", "All Job Roles"), selected = "All Job Roles")
    ),
    
    # Show a histogram of the generated distribution along with a table of the distributions' means and SDs.
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)
# Now concstruct the server.
server <- function(input, output) {
  app_initial_tbl <- readRDS("./shiny_input.RDS")
  app_modified_tbl <- app_initial_tbl
  output$plot <- renderPlot({
    
    # Don't need to change anything if the input is set to All Departments; otherwise filter by department selection.
    if (input$deptselect != "All Departments") {
      app_modified_tbl <- app_modified_tbl %>%
        filter(Department == input$deptselect)
    }
    
    # Don't need to change it if set to All Fields; otherwise filter by educational field selection.
    if (input$fieldselect != "All Fields") {
      app_modified_tbl <- app_modified_tbl %>%
        filter(EducationField == input$fieldselect)
    }
    
    # Don't need to change it if set to Both; otherwise filter by gender selection.
    if (input$genderselect != "Both") {
      app_modified_tbl <- app_modified_tbl %>%
        filter(Gender == input$genderselect)
    }
    
    # Don't need to change it if set to All Job Roles; otherwise filter by job role selection.
    if (input$jobroleselect != "All Job Roles") {
      app_modified_tbl <- app_modified_tbl %>%
        filter(JobRole == input$jobroleselect)
    }
    
    # These three if statements filter the data according to the criterion selected.
    if (input$criterionselect == "Monthly Income") {
      criteria <- "MonthlyIncome"
    }
    
    if (input$criterionselect == "Attrition") {
      criteria <- "Attrition"
    }
    
    if (input$criterionselect == "Job Satisfaction") {
      criteria <- "JobSatisfaction"
    }
    
    # Plot the graph. The given x-value depends on which of the three criteria were selected in the user interface.
    ggplot(data = app_modified_tbl, aes(x = app_modified_tbl[[criteria]])) +
      geom_histogram() +
      labs(x = criteria, y = "Count")
  })
  
  # Now create the dynamically generated table of means and SDs.
  output$table <- renderTable({
    
    # These four if else statements subset the table's data. If all data are selected, then no subsetting occurs. This is relevant since I set the default values for these variables to include all data upon opening the app.
    if (input$deptselect != "All Departments"){
      departmentclicked <- "Department"
    } else {
      departmentclicked <- NULL
    }
    
    if (input$fieldselect != "All Fields"){
      fieldclicked <- "EducationField"
    } else {
      fieldclicked <- NULL
    }
    
    if (input$genderselect != "Both"){
      genderclicked <- "Gender"
    } else {
      genderclicked <- NULL
    }
    
    if (input$jobroleselect != "All Job Roles"){
      roleclicked <- "JobRole"
    } else {
      roleclicked <- NULL
    }
    
    # By creating a  string of our chosen filters, we can efficiently feed these filters (assuming they aren't NULL) into the table to inform what data need to be filtered.
    filters <- c(departmentclicked, fieldclicked, genderclicked, roleclicked)
    
    # These three if statements filter according the the criterion selected. The code is exactly the same as the code used for the plot.
    if (input$criterionselect == "Monthly Income") {
      criteria <- "MonthlyIncome"
    }
    
    if (input$criterionselect == "Attrition") {
      criteria <- "Attrition"
    }
    
    if (input$criterionselect == "Job Satisfaction") {
      criteria <- "JobSatisfaction"
    }
    
    # Now create the basic tibble that will list the means and SDs of the selected data. The column header names are auto formatted strangely, but the data are correct.
    app_modified_tbl %>%
      group_by(across(all_of(filters))) %>%
      summarize(across(all_of(criteria), list(mean = mean, sd = sd)))
  })
}

# Run the app.
shinyApp(ui = ui, server = server)