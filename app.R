source('methods.R')
library(shiny)
library(DT)
library(rhandsontable)

# constants
seqMachines <- c("NextSeq", "MiSeq", "HiSeq 2000/2500", 
                 "HiSeq 3000/4000", "MiniSeq","NovaSeq")

indexTable <- read.table("data/DNA HT Dual Index Kit – 96N Set A Forward.csv", sep=",",
                         header=TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "style.css"),
        tags$h1("Dawson Lab"),
    ),
    
    tags$div(
        id="row-groups",
        tags$h3("Index Compatibility Checker")
    ),
    
    
    fluidRow(
        column(4, selectInput(inputId="kit", label="Kit", 
                              choices=c("DNA HT Dual Index Kit", "Other"))),
        column(3, selectInput(inputId="set", "Set", 
                              choices=c("96N Set A", "96N Set B", "96N Set C", 
                                        "96N Set D", "24N"))),
        column(3, selectInput(inputId="machine", label="Sequencer", choices=seqMachines)),
    ),
    
    tags$div(
        id="row-groups",
        fluidRow(
            column(9, rHandsontableOutput("kit_indices", width = 400)),
            column(3, actionButton("add", "Add Indices to List"))
        ),
    ),
    
    tags$div(
        id="row-groups",
        tags$h3(" ")
    ),
    
    tags$div(
        id="row-groups",
        actionButton("check", "Check Index"),
        tags$div(
            tags$h3(" ")
        ),
        fluidRow(
            column(4, conditionalPanel(condition = "true", 
                                       rHandsontableOutput("index_in", width = 300))),
            column(8, rHandsontableOutput("collisions"))
        ),
    )
    
    
    # downloadButton(outputId="download", label="Download")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$kit_indices <- renderRHandsontable(
        rhandsontable(tableConstructor(), width = 800) %>%
            hot_cols(colWidths = 40)
    )
    
    indexIn = reactive({
        if (!is.null(input$index_in)) {
            indexDF = hot_to_r(input$index_in)
        } else {
            indexDF = data.frame(index = rep("",50), stringsAsFactors = FALSE)
        }
        return(indexDF)
    })
    
    output$index_in <- renderRHandsontable(
        rhandsontable(indexIn(), width = 400) %>%
            hot_cols(colWidths = 200)
    )
    
    # triggered by check button
    checkResult = reactive({
        input$check
        isolate({
            result=list()
            result$collisionCount=0
            result$collisionTable=data.frame()
            
            if (!is.null(input$collisions)) {
                # get input data
                inputTable=indexIn()
                
                # run collsion check
                collisionTable = checkIndex(inputTable)
                
                result$collisionCount = nrow(collisionTable)
                
                if (result$collisionCount == 0) {
                    collisionTable = data.frame(result=c("No collisions"))
                }
                result$collisionTable = collisionTable
            }
            return(result)
        })
    })
    
    output$collisions <- renderRHandsontable({
        outputTable = checkResult()
        rhandsontable(outputTable$collisionTable, readOnly=TRUE)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


