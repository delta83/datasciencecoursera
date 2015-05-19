corr <- function(directory, threshold = 0) {
  files = list.files(directory)
  result <- vector()
  flag <- 0
  if(sum(files > 0) == 0) {
    return("No files present in the given directory. Please provide the correct directory and run again. Exiting ...")
  } else {
    for(file in files) {
      id <- sub(".csv", "", file)
      count_frame <- complete(directory, id)
      count <- count_frame[1, 2]
      if(count > threshold) {
        data <- read.csv(paste(directory, "/", file, sep=""))
        dataset_nitrate <- data[, "nitrate"][!(is.na(data[, "nitrate"]) | is.na(data[, "sulfate"]))]
        dataset_sulfate <- data[, "sulfate"][!(is.na(data[, "nitrate"]) | is.na(data[, "sulfate"]))]
        correlation <- cor(dataset_nitrate, dataset_sulfate)
        result <- c(result, correlation)
        flag <- 1
      }
    }
    if(flag == 0) {
      print("None of the monitor readings matched the thresold level. Please try again with a lower threshold. Exiting ...")
      return(result)
    } else {
      return(result)
    }
  }
}