corr <- function(directory, threshold = 0) {
        path <- paste0(getwd(),"/", directory)
        corr_vect <- NULL
        for (i in 1:332) {
                if (i < 10) {
                        dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), as.is = TRUE, header = TRUE)
                }
                else if (i < 100) {
                        dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), as.is = TRUE, header = TRUE)
                }
                else {
                        dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), as.is = TRUE, header = TRUE)
                }
                data <- dat[complete.cases(dat),]
                if (nrow(data) > threshold) {
                        corr_vect <- c(corr_vect, cor(data[,"sulfate"], data[, "nitrate"]))
                }
        }
        return(corr_vect)
}