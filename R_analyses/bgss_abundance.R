#### Analysis of BGSS trap counts for North Coast PD project

rm(list = ls())
# Load packages
my.packages <- c("xlsx", "tidyr", "dplyr", "data.table", "ggplot2", "lubridate")
lapply(my.packages, require, character.only = TRUE)

#### Load and compile trap counts for all vineyard sites
#### In Sonoma county
sonomaRaw <- lapply(1:16, function(x) excelExtract(sheetIndex = x)) 
str(sonomaRaw[[1]])

sonomaData <- sonomaRaw %>% rbindlist(., fill = TRUE) %>% as.data.frame()
# Replace spaces in column names with "_"
names(sonomaData) <- names(sonomaData) %>% gsub(" ", "_", .)
str(sonomaData)

# Remove columns for #_days_trap_up and NA, not useful and making things difficult because they're labeled the same headers
sonomaData <- sonomaData[,-which(names(sonomaData) == "#_day_trap_up")] %>% dplyr::select(., -contains("NA"))

#### Get dates recognized by R
trapDates <- sonomaData %>% dplyr::select(., contains("Deployed"), starts_with("Date")) 
# Remove columns that are all NA
allNA <- vector(length = ncol(trapDates))
for(i in 1:ncol(trapDates)){
  allNA[i] <- all(is.na(trapDates[,i]))
}
trapDates <- trapDates[,-which(allNA)]

# Remove weekday from date entries
#i <- 1
for(i in 1:ncol(trapDates)){
  trapDates[,i] <- tstrsplit(trapDates[,i], " - ", fixed = TRUE)[[2]] %>% as.vector() %>% mdy()
}

