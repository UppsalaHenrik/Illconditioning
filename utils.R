# I have chosen to separate these functions into a utils file.
# These are smaller and quite general functions.


parseExtFile <- function(extFilePath){
  
  # Parsing the .ext using readLines because of different types of 
  # pesky whitespace. Also remove the first row and separate header
  rawExtFile <- readLines(extFilePath)
  header <- unlist(strsplit(gsub("^\\s+", "", rawExtFile[2]), "\\s+"))
  extFile <- rawExtFile[3:length(rawExtFile)]
  
  # Remove leading whitespace
  extFile <- gsub("^\\s+", "", extFile)
  
  # split the lines up and pack them into a df
  extFileCharDF <- data.frame(do.call("rbind", strsplit(extFile, "\\s+")), 
                          stringsAsFactors = FALSE)
  names(extFileCharDF) <- header
  
  # I need to convert the characters to numerics
  extFileDF <- as.data.frame(sapply(extFileCharDF, as.numeric))
  
  # Return the ext file contents as a dataframe
  return(extFileDF)
}


calcNMCondNum <- function(extFileDF){
  
  # Find the row that contains the eigenvalues
  eigenRowNum <- grep("-1000000002", extFileDF[,1])
  
  # Pick out that row without the iteration column
  eigenRow <- extFileDF[eigenRowNum,2:length(extFileDF)]
  
  # Remove the zeros et voila, you have a vector of eigenvalues
  eigenvals <- eigenRow[which(eigenRow!=0)]
  
  # Calculate and return the condition number
  NMCondNum <- max(eigenvals)/min(eigenvals)
  return(NMCondNum)
}

getInitOFV <- functions(extFileDF){
  
  # Get the last value in the first line of the ext 
  initOfv <- extFileDF[1,length(extFileDF)][1]
  
  return(initOFV)
  
}

