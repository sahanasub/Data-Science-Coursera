pollutantmean <- function(directory, pollutant, id = 1:332) {
  myPath <- directory
  myListOfFiles <- list.files(myPath)
  myListOfFiles.names <- as.numeric(sub(".csv","",myListOfFiles))
  selectedFiles <- myListOfFiles[intersect(id,myListOfFiles.names)]
  
  myData <- lapply(file.path(myPath, selectedFiles), read.csv)
  myData <- do.call(rbind.data.frame, myData)
  
  mean(myData[,pollutant], na.rm = TRUE)
  
}