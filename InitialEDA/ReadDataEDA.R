require(readxl)

myFiles <- list.files(path = "../../FitBit Data", pattern="*.xls")

dat <- list()

for(i in 1:length(myFiles)){
  current <- paste("../../FitBit Data/", myFiles[i], sep = "")
  dat[[i]] <- read_excel(current, sheet = 2)
}

timeData <- as.data.frame(do.call(rbind, dat))