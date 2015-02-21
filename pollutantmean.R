setwd("~/GitHub/Coursera-R-Programming/")
#list.files()
pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  mean.vector <- c()
  all.files <- as.character( list.files(directory) )
  file.paths <- paste(directory, all.files, sep="")
  for(i in id) {
    current.file <- read.csv(file.paths[i], header=T, sep=",")
    head(current.file)
    pollutant
    na.remove <- current.file[!is.na(current.file[, pollutant]), pollutant]
    mean.vector <- c(mean.vector, na.remove)
  }
  result <- mean(mean.vector)
  return(round(result, 3)) 
}


#testing
pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
pollutantmean("specdata", "nitrate", 23)
## [1] 1.281
