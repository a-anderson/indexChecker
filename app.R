#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
        tags$link( 
            rel="stylesheet",
            type="text/css",
            href="style.css"
        )
    ),
    tags$h1("Dawson Lab"),
    tags$h4("Index Compatibility Checker"),
    
    fluidRow(
        column(3, selectInput(inputId="kit", label="Kit", choices=c("TakaraBio", "Other"))),
        column(2, selectInput(inputId="set", "Set", choices=c("A", "B", "C", "D"))),
        column(3, selectInput(inputId="machine", label="Sequencer", choices=c("NextSeq", "MiSeq"))),
    ),
   
    tableOutput("kit_indices"),
    
    sidebarPanel(
        actionButton("check", "Check Index"),
        hr(),
        
        conditionalPanel(condition = "true", rHandsontableOutput("hot", width = 400))
    ),
    
    mainPanel(
        rHandsontableOutput("collisions", width = 500)
    ),
    
    downloadButton(outputId="download", label="Download")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
