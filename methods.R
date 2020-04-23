library(stringr)
library(dplyr)

kitSets <- function(kit) {
  
  if (kit=="DNA HT Dual Index Kit") {
    return(c("96N Set A", "96N Set B", "96N Set C", 
             "96N Set D", "24N"))
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
  # check that there is only one + in the string
  # if both both conditions satisfied, return TRUE
  # use regex
  validIndex <- grepl("^[ACTGactg]{6,8}\\+?[ACTGactg]{0,8}$", string)
  return(validIndex)
}

# Construct table for index selection
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
  # reads index tables in according to set and machine read direction
  fileLocation <- paste("data/DNA HT Dual Index Kit – ", set, " ", direction, ".csv", sep="")
  indexTable <- read.table(fileLocation, sep=",",header=TRUE, row.names=1, 
                           stringsAsFactors=FALSE, check.names=FALSE)
  return(indexTable)
}

sequencesToAdd <- function(booleanDF, sequenceDF) {
  # identify cells that have been checked in the display table
  # and return a vector of the selected index sequences
  selectionMatrix <- which(booleanDF==TRUE, arr.ind=TRUE, useNames=TRUE)
  sequences <- c()
  numSamples <- nrow(selectionMatrix)
  for (sample in 1:numSamples) {
    sequence <- sequenceDF[selectionMatrix[sample, 'row'], selectionMatrix[sample,'col']]
    sequences <- c(sequences, sequence)
  }
  return(sequences)
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
    
    # test that index1 is a valid index
    if (!isValidIndex(index1)) {
      invalidRow <- c(index1=i, 
                      index2=0, 
                      sequence1=index1, 
                      sequence2="<-- NOT VALID", 
                      distance=0)
      
      outputDF[nrow(outputDF) + 1, ] = invalidRow
    }
    
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