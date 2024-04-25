source('methods.R')
library(shiny)
library(rhandsontable)

# constants
seqMachines <- c("NextSeq", "MiSeq", "HiSeq 2000/2500", 
                 "HiSeq 3000/4000", "MiniSeq","NovaSeq")

defaultSets <- c("96N Set A", "96N Set B", "96N Set C", "96N Set D")

# define UI for application 
ui <- fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "style.css"),
        tags$h1("DNA Sequencing"),
    ),
    
    tags$div(
        tags$h3("Index Compatibility Checker")
    ),
    
    
    # kit, set and machine selection drop-down menus
    fluidRow(
        column(4, selectInput(inputId="kit", label="Kit", 
                              choices=c("DNA HT Dual Index Kit", "Lexogen"))),
        column(4, selectInput(inputId="set", "Set", 
                              choices=defaultSets)),
        column(3, selectInput(inputId="machine", label="Sequencer", choices=seqMachines)),
    ),
    
    # index location selection table
    tags$div(
        fluidRow(
            column(8, rHandsontableOutput("kit_indices", width = 400)),
            column(3, actionButton("add", "Add Index"))
        ),
    ),
    
    tags$div(
        tags$h3(" ")
    ),
    
    fluidRow(
        column(9, rHandsontableOutput("collisions")),
    ),
    
    tags$div(
        tags$h3(" ")
    ),
    
    fluidRow(
        column(2, actionButton("check", "Check Index")),
        column(2, actionButton("clear", "Clear All"), offset=1),
        column(2, downloadButton(outputId="download", label="Download"), offset=1)
    ),
    
    tags$div(
        tags$div(
            tags$h3(" ")
        ),
        # index check and results tables
        fluidRow(
            column(12, rHandsontableOutput("index_in")),
        ),
    )
    
)

# define server logic required for index compatibility checker
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
            seqDF <- indexSequenceTable(input$kit, input$set, direction) 
            selectionDF <- hot_to_r(input$kit_indices)
            indices <- sequencesToAdd(selectionDF, seqDF, input$kit, input$set, input$machine)
            numIndices <- nrow(indices)
            
            indexDF = na.omit(hot_to_r(input$index_in), target.colnames="Index")
            firstEmptyRow = match("", indexDF[,"Index"])
            indexDF[(firstEmptyRow):(firstEmptyRow+numIndices-1), ] = indices
            
            # update index table with selected indices
            output$index_in <- renderRHandsontable(
                rhandsontable(indexDF, width = 800)
            )
        }

    )
    
    # function to generate index checker table 
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
        rhandsontable(indexIn(), width = 800)
    )
    
    # generate collisions table
    output$collisions <- renderRHandsontable({
        rhandsontable(data.frame(Check="Check Index"), readOnly=TRUE)
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
                rhandsontable(indexBlank(), width = 800) 
            )
            
            output$collisions <- renderRHandsontable({
                rhandsontable(data.frame(Check="Check Index"), readOnly=TRUE)
            })
            
            output$kit_indices <- renderRHandsontable(
                rhandsontable(tableConstructor(), width = 800) %>%
                    hot_cols(colWidths = 40)
            )
            
        }
    )
    
    # download button response
    output$download <- downloadHandler(
        filename = function() {
            paste("index-check-", Sys.Date(), ".csv", sep="")
        },
        content=function(file) {
            write.csv(indexIn(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)


