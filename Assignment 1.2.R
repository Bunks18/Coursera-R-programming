#### 2nd assigment

complete<- function(directory, id = 1:332) {
  
  ### library
  #library(dplyr)
  ### get directory path
  files_full <- list.files(directory, full.names = TRUE) 
  
  ### initative data frame 
  dat <- data.frame()
  
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  
  ### create new data frame with only complete cases 
  complete<-dat[complete.cases(dat),]
  
  ### group by to get # of observations 
  
  results<-
    complete %>%
    group_by(ID) %>%
    summarize(count = n())
  
  #### create the right output for data frame 
  results<-as.data.frame(results)
  names(results)[1]<-"id"
  names(results)[2]<-'nobs'
  
  ### print results 
  results
}


cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)



cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)



cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))




set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
