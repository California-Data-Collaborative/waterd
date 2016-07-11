library(shiny)
source("common.R")

# Define UI for random distribution application 
shinyUI(
  navbarPage(
    "MNWD",
    theme = "bootstrap.min.css",
    tabPanel(
      "Commercial Water Supply and Demand",
      # style='font-family:"Open Sans","Helvetica Neue",Helvetica,Arial,sans-serif',
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "modelType", "Model type",
            choices = array(sapply(MODEL_LIST,function(x) {x$name})),
            selected='Linear regression'
          ),
          br(),
          
          sliderInput(
            "nDaysHist", 
            "Number of days of history", 
            value = 15,
            min = 1, 
            max = 365*13
          ),
          sliderInput(
            "nDays", 
            "Number of days to forecast", 
            value = 15,
            min = 1, 
            max = 365*5
          ),
          numericInput(
            "confidenceLevel", 
            "Confidence level (%)", 
            value = 95,
            min = 1, 
            max = 99
          ),
          numericInput(
            "maxDailyPull", 
            "Max daily pull from storage (mg)", 
            value = 10,
            min = 0, 
            max = 100
          )
          
        ),
        mainPanel(
          plotOutput("totalUsage")
        )
      )
    ),
    tabPanel(
      "About",
      tags$p("This tool has been created in a partnership between the California Data Collaborative, the Moulton Niguel Water District and DataKind.")),
    footer=tags$div(
      style="width:100%; text-align:center",
      tags$img(src="cdc.png",width=100,style='margin:50px'),
      tags$img(src="1456792766-2552300-222x177-MNWD-LogoStacked-PMS.png",width=100,style='margin:50px'),
      tags$img(src="2708040823-5.png",width=100,style='margin:50px')
    )
  )
)