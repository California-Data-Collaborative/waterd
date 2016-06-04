library(shiny)
source("common.R")

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tabsets"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput("modelType", "Model type",
                   choices = names(MODEL_LIST)),
      br(),

            sliderInput("nDays", 
                  "Number of days to forecast", 
                  value = 365*1,
                  min = 1, 
                  max = 365)
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Total usage forecast", plotOutput("totalUsage")), 
                  tabPanel("Per customerUsage", verbatimTextOutput("perCustomerUsage"))
      )
    )
  )
))