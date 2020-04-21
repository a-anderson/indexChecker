library(shiny)
library(DT)
library(rhandsontable)

# Construct table for index selection
tableConstructor <- function() {
    rows = c("A", "B", "C", "D", "E", "F", "G", "H")
    plate = data.frame(matrix(ncol=12, nrow=8), row.names=rows)
    colnames(plate) <- seq(1, 12)
    plate[is.na(plate)] = FALSE
    return(plate)
}

selectIndex <- function(booleanDF) {
    selectionMatrix <- which(booleanDF==TRUE, arr.ind=TRUE, useNames=TRUE)
    return(selectionMatrix)
}

hammingDistance <- function(string1, string2) {
    # Calculate the hamming distance between 2 strings
    
    minLength = min(nchar(string1), nchar(string2))
    
    # split strings into parsable characters
    split1 <- strsplit(string1, "")[[1]]
    split2 <- strsplit(string2, "")[[1]]
    
    distance = sum(!split1[1:minLength] == split2[1:minLength])
    
    return(distance)
}

checkIndex <- function(indexTable) {
    # Compare differences between all indices in a table
    
    outputDF = data.frame(index1=integer(), 
                          index2=integer(), 
                          sequence1=character(), 
                          sequence2=character(),
                          distance=integer(),
                          stringsAsFactors=FALSE)
    
    numIndices = nrow(indexTable)
    
    # loop over all indices
    for (i in 1:(numIndices-1)) {
        index1 = toupper(trimws(indexTable[i,1]))
        
        # ignore blank entry
        if (nchar(index1) == 0 ) next
        
        for (j in (i+1):numIndices) {
            
            index2 = toupper(trimws(indexTable[j,1]))
            
            # ignore blank entry
            if (nchar(index2) == 0 ) next
            
            distance = hammingDistance(index1, index2)
            
            if (distance < 3) {
                collision <- c(index1=i, 
                               index2=j, 
                               sequence1=index1, 
                               sequence2=index2, 
                               distance=distance)
                
                outputDF[nrow(outputDF) + 1, ] = collision
            }
        }
    }
    return(outputDF)
}

# constants
seqMachines <- c("NextSeq", "MiSeq", "HiSeq 2000/2500", 
                 "HiSeq 3000/4000", "MiniSeq","NovaSeq")

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


