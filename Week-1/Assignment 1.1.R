#####Assigntment 1.1

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ### get directory
  files_full <- list.files(directory, full.names = TRUE) 
  
  ### initiate data frame
  df <- data.frame()
  
  ### loop over to create master data frame 
  for (i in id) {
    df <- rbind(df, read.csv(files_full[i]))
  }
  
  
  #### get mean 
  mean(df[, pollutant], na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
round<-pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
round

pollutantmean("specdata", "nitrate")