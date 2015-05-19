pollutantmean <- function(directory, pollutant, ids = 1:332) {
  if(!(pollutant == "nitrate" | pollutant == "sulfate")) {
    return("Please enter either nitrate or sulphate as pollutant. Exiting ...")
  }
  flag <- 0
  dataset <- vector()
  for(id in ids) {   
    id <- formatC(id, width=3, flag="0")
    full_path <- paste(directory, "/", id, ".csv", sep="")
    file <- try(read.csv(full_path))
    if(class(file) == "try-error") {
      message <- paste("The monitor id", id, "is not present. Moving to the next monitor id provided.")
      print(message)
    } else {
      dataset_raw <- file[, pollutant]
      dataset_clean <- dataset_raw[!is.na(dataset_raw)]
      dataset <- c(dataset,dataset_clean)
      flag <- 1
    }   
  }
  if(flag == 1) {
    mean <- mean(dataset)
    return(mean)
  } else {
    print("None of the monitor ids provided were legitimate. Please provide the right set of monitor ids and run again. Exiting ...")
    return()
  }
}
