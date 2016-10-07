library(stringr)


##
#
##
getPollutantMatrics <- function(directory, id=1:332) {
    
    matrices <- matrix(ncol=4, nrow=0)
    
    for (i in id) {
        fullFileDir <- paste(directory, str_pad(i, 3, pad = "0"), sep = "/")
        fullFileDir <- paste(fullFileDir, ".csv", sep="")
        
        matrices <- rbind(matrices, read.csv(fullFileDir))
    }
    
    matrices
}

pollutantMean <- function(directory="/Users/zhiyin/workspace/coursera/datasciencecoursera/programmingR/airPolution/specdata", 
                          pollutant, id=1:332) {
    
    matrices <- getPollutantMatrics(directory, id)
    mean(matrices[, pollutant], na.rm = TRUE)
}

