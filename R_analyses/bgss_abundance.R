#### Analysis of BGSS trap counts for North Coast PD project

rm(list = ls())
# Load packages
my.packages <- c("xlsx", "tidyr", "dplyr", "data.table", "ggplot2", "lubridate")
lapply(my.packages, require, character.only = TRUE)

# Import functions
source("R_functions/bgss_ncpd_functions.R")
source("R_functions/factor2numeric.R")

#### Load and compile trap counts for all vineyard sites
#### In Sonoma county
#### Need to make sure all active sheets are included as a vector named "activeSheetsIndex"
activeSheetsIndex <- 1:16
sonomaRaw <- lapply(activeSheetsIndex, function(x) excelExtract(sheetIndex = x)) # Load sheets as a list
str(sonomaRaw[[1]]) # Check that data make sense

# Combine sheets into a single data.frame; requires that columns are named identically
sonomaData <- sonomaRaw %>% rbindlist(., fill = TRUE) %>% as.data.frame() 
# Replace spaces in column names with "_"
names(sonomaData) <- names(sonomaData) %>% gsub(" ", "_", .)
str(sonomaData)

# Remove columns for #_days_trap_up and NA, not useful and making things difficult because they're labeled the same headers
sonomaData <- sonomaData[,-which(names(sonomaData) == "#_day_trap_up")] %>% dplyr::select(., -contains("NA")) 
# This doesn't remove #_day_trap_up but adds a number to them. That's OK.
# Remove columns that are all NA
allNA <- vector(length = ncol(sonomaData))
for(i in 1:ncol(sonomaData)){
  allNA[i] <- all(is.na(sonomaData[,i]))
}
sonomaData <- sonomaData[,-which(allNA)]
str(sonomaData)

#### Get dates recognized by R
## Make new data.frame with only dates
trapDates <- sonomaData %>% dplyr::select(., contains("Deployed"), starts_with("Date")) 
# Remove weekday from date entries
#i <- 1
for(i in 1:ncol(trapDates)){
  trapDates[,i] <- tstrsplit(trapDates[,i], " - ", fixed = TRUE)[[2]] %>% as.vector() %>% mdy()
}




#### Add trap count columns to dates data
trapCounts <- sonomaData %>% dplyr::select(., contains("Trap_Count"))
# Convert column class to numeric
for(i in 1:ncol(trapCounts)){
  trapCounts[,i] <- factor2numeric(trapCounts[,i])
}
str(trapCounts)
trapData <- cbind(trapDates, trapCounts, sonomaData$vysite)
