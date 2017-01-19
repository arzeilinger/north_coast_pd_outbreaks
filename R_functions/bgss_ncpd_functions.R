#### Functions for analysis of BGSS abundance data

#### Function to extract data from .xlsx sheets
excelExtract <- function(file = "data/Sonoma_trap_counts.xlsx", sheetIndex = 1){
  rawsheet <- read.xlsx(file = file, sheetIndex = sheetIndex, header = FALSE)
  # Create vineyard name from grower and block information
  vysite <- paste(rawsheet[1,2], rawsheet[2,2], sep = ".")
  # Column names are in row 5, extract and convert to a character vector
  sheetNames <- t(rawsheet[5,]) %>% as.vector()
  sheet <- rawsheet[-c(1:5),]
  names(sheet) <- sheetNames
  sheet$vysite <- vysite
  return(sheet)
}
