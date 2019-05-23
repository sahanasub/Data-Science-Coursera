complete <- function(directory, id = 1:332){
  myPath <- directory
  myList <- list.files(myPath)
  
  myList.names <- as.numeric(sub(".csv","",myList))
  selected.files <- myList[intersect(id,myList.names)]
  selected.files.names <- as.numeric(sub(".csv","",selected.files))
  
  myData <- lapply(file.path(myPath,selected.files), read.csv)
  getomittedrows <- lapply(myData,na.omit)
  getnob <- sapply(getomittedrows, nrow)
  
  myDataFrame <- data.frame("id" = selected.files.names, "nobs" = getnob)
  return(myDataFrame)
}