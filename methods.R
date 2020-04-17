library(stringr)

checkDna <- function(string) {
  # check that characters are only a t c g or +
  # check that there is only one + in the string
  # if both both conditions satisfied, return TRUE
  # use regex
  
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