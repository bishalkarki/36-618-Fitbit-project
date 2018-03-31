clean_fitbit_column <- function(column){
    
    # remove commas
    clean_column <- gsub(",", "", column)
    
    # try to parse column as numeric
    try_parse <- as.numeric(clean_column)
    
    # if no rows can be parsed as numeric, we have a date column
    if (all(is.na(as.numeric(try_parse)))){
        # date column
        return(strptime(column, "%Y-%m-%d"))
    }
    # otherwise treat the column as numeric
    else{
        return(try_parse)
    }
    
}

read_fitbit_data <- function(path, sheet = 2, pattern = "*.xls"){
    
    # load required package
    if (!require('readxl')){
        install.packages("readxl")
        library('readxl')
    }
    
    # get a list of all files at 'path' matching the extension 'pattern'
    myFiles <- list.files(path = path, pattern = pattern)
    
    # create a list that stores individual datasets for each file
    dat <- list()
    for (i in 1:length(myFiles)){
        current <- paste(path, "/", myFiles[i], sep = "")
        dat[[i]] <- read_excel(current, sheet = sheet)
    }
    
    # merge the list into a single data frame
    df <- as.data.frame(do.call(rbind, dat))
    
    # rename columns without spaces or periods
    colnames(df) <- gsub(" ", "", colnames(df))
    
    # clean data frame into correct data types
    df <- as.data.frame(suppressWarnings(sapply(df, clean_fitbit_column)))

    return(df)
    
} 

