library(shiny)
source("common.R")

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Moulton Niguel Water District"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput("modelType", "Model type",
                  choices = array(sapply(MODEL_LIST,function(x) {x$name})),
                  selected='Linear regression'),
      br(),

      sliderInput("nDaysHist", 
                  "Number of days of history", 
                  value = 15,
                  min = 1, 
                  max = 365*13),
      sliderInput("nDays", 
                  "Number of days to forecast", 
                  value = 15,
                  min = 1, 
                  max = 365*5),
      numericInput("confidenceLevel", 
                  "Confidence level", 
                  value = 95,
                  min = 1, 
                  max = 99),
      numericInput("maxDailyPull", 
                  "Max daily pull from storage (mg)", 
                  value = 10,
                  min = 0, 
                  max = 100)
      
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Total usage forecast", plotOutput("totalUsage"))
      )
    )
  )
))