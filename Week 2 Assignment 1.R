#Get columns from csv file
#specdata <- read.csv(filelocation)
#Emptydata <- specdata[0,]

#######################################################
rm(list=ls())

pollutantmean <- function(directory, pollutant, id = 1:332) {
  specdata <- data.frame()
  for(i in id) {
    filelocation <- paste(directory, "/", sprintf("%03d",i), ".csv", sep = "")
    print(filelocation)
    specdata <- rbind(specdata, read.csv(filelocation))
    str(specdata)
  }
  #str(specdata)
  #specdata <- na.omit(specdata)
  str(specdata)
  value_vector <- specdata[[pollutant]]
  value_vector <- value_vector[!is.na(value_vector)]
  mean(value_vector)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

#######################################################
rm(list=ls())

complete <- function(directory, id = 1:332) {
  result <- data.frame()
  for(i in id) {
    filelocation <- paste(directory, "/", sprintf("%03d",i), ".csv", sep = "")
    specdata <- read.csv(filelocation)
    specdata <- na.omit(specdata)
    result <- rbind(result, data.frame(id = i, nobs = nrow(specdata)))
  }
  result
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#######################################################
## Use complete() make it slower, just example use it, that why I use
rm(cc, use)

corr <- function(directory, threshold = 0){
  completeddata <- complete(directory)
  completeddata <- completeddata[completeddata$nobs > threshold,]

  result <- c()
  for(i in completeddata$id) {
    filelocation <- paste(directory, "/", sprintf("%03d",i), ".csv", sep = "")
    specdata <- read.csv(filelocation)
    specdata <- na.omit(specdata)
    result <- c(result, cor(specdata$sulfate, specdata$nitrate))
  }
  result
}

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))





xx <- corr("specdata", 150)

head(xx)











xx <- pollutantmean("specdata", "sulfate")
str(xx)
mean(xx[!is.na(xx)])

directory <- "specdata"

filelocation <- paste(directory, "/", sprintf("%03d",1), ".csv", sep = "")
print(filelocation)
print(head(specdata))
str(specdata)
str(Emptydata)
