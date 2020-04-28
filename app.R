source('methods.R')
library(shiny)
library(rhandsontable)

# constants
seqMachines <- c("NextSeq", "MiSeq", "HiSeq 2000/2500", 
                 "HiSeq 3000/4000", "MiniSeq","NovaSeq")

defaultSets <- c("96N Set A", "96N Set B", "96N Set C", "96N Set D")

indexBlank <- function() {
    blankDF <- data.frame(index = rep("",100), stringsAsFactors = FALSE)
    return(blankDF)
    }

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
    
    
    # Kit, set and machine selection drop-down menus
    fluidRow(
        column(4, selectInput(inputId="kit", label="Kit", 
                              choices=c("DNA HT Dual Index Kit", "Lexogen"))),
        column(4, selectInput(inputId="set", "Set", 
                              choices=defaultSets)),
        column(3, selectInput(inputId="machine", label="Sequencer", choices=seqMachines)),
    ),
    
    # Index location selection table
    tags$div(
        id="row-groups",
        fluidRow(
            column(9, rHandsontableOutput("kit_indices", width = 400)),
            column(3, actionButton("add", "Add Index"))
        ),
    ),
    
    tags$div(
        id="row-groups",
        tags$h3(" ")
    ),
    
    fluidRow(
        column(2, actionButton("check", "Check Index")),
        column(3, actionButton("clear", "Clear All"), offset=2)
    ),
    
    tags$div(
        id="row-groups",
        tags$div(
            tags$h3(" ")
        ),
        # index check and results tables
        fluidRow(
            column(4, conditionalPanel(condition = "true", 
                                       rHandsontableOutput("index_in", width = 300))),
            column(8, rHandsontableOutput("collisions"))
        ),
    )
    
    
    # downloadButton(outputId="download", label="Download")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$kit_indices <- renderRHandsontable(
        rhandsontable(tableConstructor(), width = 800) %>%
            hot_cols(colWidths = 40)
    )
    
    # update set depending on selected kit
    observe({
        updateSelectInput(
            session, "set",
            label = "Set",
            choices = kitSets(input$kit)
        )
        # remove ticks from index selection table
        output$kit_indices <- renderRHandsontable(
            rhandsontable(tableConstructor(), width = 800) %>%
                hot_cols(colWidths = 40)
        )
    })
    
    # remove ticks from index selection table set changes
    observeEvent(
        input$set,
        {
            output$kit_indices <- renderRHandsontable(
                rhandsontable(tableConstructor(), width = 800) %>%
                    hot_cols(colWidths = 40)
            )
        }
    )
    
    # add selected indices in response to "Add Index" button
    observeEvent(
        input$add,
        {
            direction <- machineDirection(input$machine)
            seqDF <- indexSequenceTable(input$kit, input$set, direction) # change here...
            selectionDF <- hot_to_r(input$kit_indices)
            indices <- sequencesToAdd(selectionDF, seqDF)
            numIndices <- length(indices)
            
            indexDF = na.omit(hot_to_r(input$index_in))
            firstEmptyRow = match("", indexDF[,1])
            indexDF[(firstEmptyRow):(firstEmptyRow+numIndices-1), 1] = indices
            
            # update index table with selected indices
            output$index_in <- renderRHandsontable(
                rhandsontable(indexDF, width = 400) %>%
                    hot_cols(colWidths = 200))

        }

    )
    
    indexIn = reactive({
        if (!is.null(input$index_in)) {
            indexDF = hot_to_r(input$index_in)
        } else {
            indexDF = indexBlank()
        }
        return(indexDF)
    })
    
    # generate index table
    output$index_in <- renderRHandsontable(
        rhandsontable(indexIn(), width = 400) %>%
            hot_cols(colWidths = 200)
    )
    
    #generate collisions table
    output$collisions <- renderRHandsontable({
        outputTable = checkResult()
        rhandsontable(outputTable$collisionTable, readOnly=TRUE)
    })
    
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
                    collisionTable = data.frame(Result=c("No collisions"))
                }
                result$collisionTable = collisionTable
            }
            return(result)
        })
    })

    # check button response
    observeEvent(
        input$check,
        {
            output$collisions <- renderRHandsontable({
                outputTable = checkResult()
                rhandsontable(outputTable$collisionTable, readOnly=TRUE)
            }) 
        }
    )
    
    # clear button response
    observeEvent(
        input$clear,
        {
            output$index_in <- renderRHandsontable(
                rhandsontable(indexBlank(), width = 400) %>%
                    hot_cols(colWidths = 200)
            )
            
            output$collisions <- renderRHandsontable({
                rhandsontable(data.frame(Check="Check Indices"), readOnly=TRUE)
            })
            
            output$kit_indices <- renderRHandsontable(
                rhandsontable(tableConstructor(), width = 800) %>%
                    hot_cols(colWidths = 40)
            )
            
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


