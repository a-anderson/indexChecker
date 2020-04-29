library(dplyr)

kitSets <- function(kit) {
  
  if (kit=="DNA HT Dual Index Kit") {
    return(c("96N Set A", "96N Set B", 
             "96N Set C",  "96N Set D"))
  } else if (kit=="Lexogen") {
    return(c("i7 Index Primers (7001-7096)", 
             "i5 Index Primers (5001-5096)", 
             "SRi7 Index Primers (Small RNA)"))
  } else {
    return(NULL)
  }
}

isValidIndex <- function(string) {
  # check that characters are only a t c g or +
  # check that there is only one or zero + in the string
  # if both both conditions satisfied, return TRUE
  validIndex <- grepl("^[ACTGactg]{6,8}\\+?[ACTGactg]{0,8}$", string)
  return(validIndex)
}

tableConstructor <- function() {
  # returns a dataframe to emulate a 96 well plate
  rows = c("A", "B", "C", "D", "E", "F", "G", "H")
  plate = data.frame(matrix(ncol=12, nrow=8), row.names=rows)
  colnames(plate) <- seq(1, 12)
  plate[is.na(plate)] = FALSE
  return(plate)
}

machineDirection <- function(machine) {
  # returns the direction, forward:"Forward" or reverse-complement:"Reverse",
  # in which the selected machine will read an index
  direction <- case_when(
    machine == "NextSeq" ~ "Reverse", 
    machine == "MiSeq" ~ "Forward", 
    machine == "HiSeq 2000/2500" ~ "Forward", 
    machine == "HiSeq 3000/4000" ~ "Reverse", 
    machine == "MiniSeq" ~ "Reverse",
    machine == "NovaSeq" ~ "Forward"
  )
  return(direction)
}

dualIndexTable <- function(set, direction) {
  # reads in HT Dual Index tables in according to set and machine read direction
  fileLocation <- paste("data/DNA HT Dual Index Kit – ", set, " ", direction, ".csv", sep="")
  indexTable <- read.table(fileLocation, sep=",",header=TRUE, row.names=1, 
                           stringsAsFactors=FALSE, check.names=FALSE)
  return(indexTable)
}

lexogenTable <- function(set, direction) {
  # reads in Lexogen index tables according to set and machine read direction
  if (set == "i5 Index Primers (5001-5096)") {
    fileLocation <- paste("data/Lexogen - ", set, " ", direction, ".csv", sep="")
    print(fileLocation)
  } else {
    fileLocation <- paste("data/Lexogen - ", set, ".csv", sep="")
  }
  indexTable <- read.table(fileLocation, sep=",",header=TRUE, row.names=1, 
                           stringsAsFactors=FALSE, check.names=FALSE)
  return(indexTable)
}

indexSequenceTable <- function(kit, set, direction) {
  # return the appropriate index sequence tables
  if (kit == "DNA HT Dual Index Kit") {
    return(dualIndexTable(set, direction))
  } else if (kit == "Lexogen") {
    return(lexogenTable(set, direction))
  } else {
    return(NULL)
  }
}

locationStrings <- function(rowNumber, colNumber) {
  # return a string of row,column locations which can be easily sorted
  
  rowLetters <- c("A", "B", "C", "D", "E", "F", "G", "H")
  
  if (colNumber < 10) {
    column = paste(0, colNumber, sep="")
  } else {
    column = colNumber
  }
  location <- paste(rowLetters[rowNumber], column, sep="")
  return(location)
}

sequencesToAdd <- function(booleanDF, sequenceDF, kit, set, machine) {
  # identify cells that have been checked in the display table
  # and return a dataframe of the selected index sequences and their origin
  selectionMatrix <- which(booleanDF==TRUE, arr.ind=TRUE, useNames=TRUE)
  
  numSamples <- nrow(selectionMatrix)
  addSequences <- data.frame(Index=character(), 
                          Kit=character(), 
                          Set=character(), 
                          Location=character(), 
                          Machine=character(),
                          stringsAsFactors = FALSE)
  
  
  
  for (sample in 1:numSamples) {
    rowNumber = selectionMatrix[sample, 'row']
    colNumber = selectionMatrix[sample,'col']
    
    location <- locationStrings(rowNumber, colNumber)
    
    sequence <- sequenceDF[rowNumber, colNumber]
    addSequences[sample, ] <- c(Index=sequence, 
                              Kit=kit, 
                              Set=set, 
                              Location=location, 
                              Machine=machine)
  }
  return(addSequences[order(addSequences$Location),])
}

indexBlank <- function() {
  # set up a blank index table for app display
  blankDF <- data.frame(Index=rep("",100), 
                        Kit=rep("",100), 
                        Set=rep("",100), 
                        Location=rep("",100), 
                        Machine=rep("",100), 
                        stringsAsFactors = FALSE)
  return(blankDF)
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
  
  outputDF = data.frame(Index1=integer(), 
                        Index2=integer(), 
                        Sequence1=character(), 
                        Sequence2=character(),
                        Distance=integer(),
                        stringsAsFactors=FALSE)
  
  numIndices = nrow(indexTable)
  
  # loop over all indices
  for (i in 1:(numIndices-1)) {
    index1 = toupper(trimws(indexTable[i,1]))
    
    # ignore blank entry
    if (nchar(index1) == 0 ) next
    
    # test that index1 is a valid index
    if (!isValidIndex(index1)) {
      invalidRow <- c(Index1=i, 
                      Index2=0, 
                      Sequence1=index1, 
                      Sequence2="<-- NOT VALID", 
                      Distance=0)
      
      outputDF[nrow(outputDF) + 1, ] = invalidRow
    }
    
    for (j in (i+1):numIndices) {
      
      index2 = toupper(trimws(indexTable[j,1]))
      
      # ignore blank entry
      if (nchar(index2) == 0 ) next
      
      # ignore comparisons between single and dual index
      if ( abs(nchar(index1) - nchar(index2)) > 6) next
      
      distance = hammingDistance(index1, index2)
      
      if (distance < 3) {
        collision <- c(Index1=i, 
                       Index2=j, 
                       Sequence1=index1, 
                       Sequence2=index2, 
                       Distance=distance)
        
        outputDF[nrow(outputDF) + 1, ] = collision
      }
    }
  }
  return(outputDF)
}