#install.packages("ggplot2")
process_data <- function(dataVector) {
  resultVector <- c()
  for(val in dataVector) {
    charVector <- strsplit(val, "-")
    firstVal <- as.numeric(charVector[[1]][1])
    
    if(length(charVector[[1]]) > 1) {
      combinedVector <- c(firstVal, as.numeric(charVector[[1]][2]))
      resultVector <- c(resultVector, mean(combinedVector))
    } else {
      resultVector <- c(resultVector, firstVal)
    }
    
  }
  return(resultVector)
}



dataSet <- read.csv(file = 'C:\\Users\\musta\\Desktop\\RTestCSV.csv', TRUE)

attendanceVector <- process_data(as.vector(dataSet[,2]))
statesVector <- factor(dataSet[,1])
attendanceTable <- table(attendanceVector, statesVector)
print(attendanceTable)

hist(attendanceVector, main='Attendance Histogram')
boxplot(attendanceVector, main='Attendance Box Plot')
ggplot2::geom_histogram(attendanceVector)




