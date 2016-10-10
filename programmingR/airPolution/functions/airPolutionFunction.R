library(stringr)

getPollutantMatrics <- function(directory="/Users/zhiyin/workspace/coursera/datasciencecoursera/programmingR/airPolution/specdata", id=1:332) {
    
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

complete <- function(directory="/Users/zhiyin/workspace/coursera/datasciencecoursera/programmingR/airPolution/specdata", 
                     id=1:332) {
    
    matrices <- getPollutantMatrics(directory, id)
    complete <- matrices[complete.cases(matrices[,2:3]),]
    
    l <- split(complete, complete$ID)
    l <- lapply(l, nrow)
    
    m <- tapply()
    tapply(mtcars$mpg,mtcars$cyl,mean)
    l
    
    #print(l)
    #print(l2)
    #df <- as.data.frame(l)
   # colnames(df) <- c("id", "nobs")
    #df
}

corr <- function(directory="/Users/zhiyin/workspace/coursera/datasciencecoursera/programmingR/airPolution/specdata", 
                 threashold = 0, id=1:332) {
    
    matrices <- getPollutantMatrics(directory, id)
    complete <- matrices[complete.cases(matrices[,2:3]),]
    l = split(complete, complete$ID)
    
    final <- sapply(l, function(x){
        if (nrow(x) > threashold) {
            cor(x[,'sulfate'], x[, 'nitrate'])
        }
    })

    final[!sapply(final, is.null)]
}
