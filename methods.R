library(stringr)

checkDna <- function(string) {
  # check that characters are only a t c g or +
  # check that there is only one + in the string
  # if both both conditions satisfied, return TRUE
  # use regex
  
}

# Construct table for index selection
tableConstructor <- function() {
  rows = c("A", "B", "C", "D", "E", "F", "G", "H")
  plate = data.frame(matrix(ncol=12, nrow=8), row.names=rows)
  colnames(plate) <- seq(1, 12)
  plate[is.na(plate)] = FALSE
  return(plate)
}

selectIndexToCheck <- function(booleanDF, sequenceDF) {
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