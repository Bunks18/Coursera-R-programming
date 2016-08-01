#### 3rd assignment

directory<-getwd()

library(dplyr)
library(sqldf)


corr<-function(directory, threshold = 0){
  
  files_full <- list.files(directory, full.names = TRUE) ### path to directory
  
  
  list<-vector(mode='list', length = length(files_full)) ### initialize directory
  
  for (i in seq_along(files_full)){             ### loop to get all the files into a list
    list[[i]]<-read.csv(files_full[[i]])
  }
  
  output<-do.call(rbind, list) ### rbind to create master data set 
  
  complete<-output[complete.cases(output),]
  
  ### group to get number of obs
  obs<-
    complete %>%
    group_by(ID) %>%
    summarize(count = n())
  
  
  #### filter based off threshold
  correlation<-filter(obs, count > threshold)
  
  #### join to get the right monitors  for correlation 
  library(sqldf)
  df3 <- sqldf("SELECT sulfate, nitrate, ID 
               FROM complete
               JOIN correlation USING(ID)")
  
  #### perform correlation calc 
  results<-
    df3 %>%
    group_by(ID) %>%
    summarize(cor(sulfate,nitrate))
  
  ### subset to vector
  avector <- results[, "cor(sulfate, nitrate)"]
  
  ### turn into numeric vector
  z<-as.numeric(c(t(avector)))

}


