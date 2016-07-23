library(shiny)
source("common.R")

# Define UI for random distribution application 
shinyUI(
  navbarPage(
    "Water Demand Forecaster",
    theme = "bootstrap.min.css",
    tabPanel(
      "Recycled water supply and demand",
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
            min = 0, 
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
            "Forecast confidence band (%)", 
            value = 95,
            min = 1, 
            max = 99
          ),
          numericInput(
            "maxDailyPull", 
            "Max daily recycled water supply (mg)", 
            value = 10,
            min = 0, 
            max = 100
          ),
          numericInput(
            "maxDailyStorage", 
            "Max daily storage supply (mg)", 
            value = 3,
            min = 0, 
            max = 100
          ),
          numericInput(
            "excessProb", 
            "Demand excess probability limit (%)", 
            value = 50,
            min = 1, 
            max = 99
          )
        ),
        mainPanel(
          plotOutput("totalUsage"),
          uiOutput("dailyExcess")
        )
      )
    ),
    tabPanel(
      "About",
      uiOutput("aboutCopy")
    ),
    footer=tags$div(
      style="width:100%; text-align:center",
      tags$a(
        href="http://californiadatacollaborative.com/",target="_blank",
        tags$img(src="cdc.png",width=100,style='margin:50px')
      ),
      tags$a(
        href="http://www.mnwd.com/",target="_blank",
        tags$img(src="1456792766-2552300-222x177-MNWD-LogoStacked-PMS.png",width=100,style='margin:50px')
      ),
      tags$a(
        href="http://www.datakind.org/",target="_blank",
        tags$img(src="2708040823-5.png",width=100,style='margin:50px')
      ),
      tags$a(
        href="http://www.bloomberg.com/",target="_blank",
        tags$img(src="bloomberg-logo.svg",width=100,style='margin:50px')
      )
    )
  )
)