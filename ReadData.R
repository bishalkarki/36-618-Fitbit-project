require(readxl)

setwd("../FitBit Data")
myFiles <- list.files(pattern="*.xls")

dat <- list()

for(i in 1:length(myFiles)){
  dat[[i]] <- read_excel(myFiles[i], sheet = 2)
}

timeData <- as.data.frame(do.call(rbind, dat))