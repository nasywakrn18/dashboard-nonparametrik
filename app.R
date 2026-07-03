library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = "NON-PARAM SOCIETY"
  ),
  
  dashboardSidebar(),
  
  dashboardBody()
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)