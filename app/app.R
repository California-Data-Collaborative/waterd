rm(list=ls())
library(shiny)  

print(getwd())

ui = fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #2E9AFE;
        text-align:center;
      }

    "))
  ),
  headerPanel("MNWD Water Usage Forecast"),
  tabsetPanel(         
    tabPanel(title = "Background"
    ),
    navbarMenu(title = "Analysis"
               , tabPanel(title = "Exploration Phase")
               , tabPanel(title = "Predictive Modelling", dateInput("date",NULL))
               , tabPanel(title="Data Visualization")
               ),
    tabPanel(title="conclusion")       
    ),
  tags$img(src="1456792766-2552300-222x177-MNWD-LogoStacked-PMS.png",width=100),
  tags$img(src="2708040823-5.png",width=100)
  )
   

server = function(input,output){
  
}
shinyApp(ui=ui,server=server)
