#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$head(
        tags$h1("Dawson Lab"),
        
    ),
    
    tags$h1(tags$small("Index Compatability Checker")),
    
    actionButton("check", "Check Compatibility")
        )

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
