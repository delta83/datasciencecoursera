complete <- function(directory, ids) {
  id <- vector()
  nobs <- vector()
  flag <- 0
  for(monitor_id in ids) {
    format_id <- formatC(monitor_id, width=3, flag="0")
    full_path <- paste(directory, "/", format_id, ".csv", sep="")
    file <- try(read.csv(full_path))
    if(class(file) == "try-error") {
      message <- paste("The monitor id", format_id, "is not present. Moving to the next monitor id provided.")
      print(message)
    } else {
      id <- c(id, monitor_id)
      dataset <- file[, "ID"][!(is.na(file[, "nitrate"]) | is.na(file[, "sulfate"]))]
      count <- sum(dataset > 0)
      nobs <- c(nobs, count)
      flag <- 1
    }  
  }
  if(flag == 0) {
    return("None of the monitor ids provided were legitimate. Please provide the right set of monitor ids and run again. Exiting ...")
  } else {
    table <- data.frame(id, nobs)
    return(table)
  }
}