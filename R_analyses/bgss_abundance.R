#### Analysis of BGSS trap counts for North Coast PD project

rm(list = ls())
# Load packages
my.packages <- c("xlsx", "tidyr", "dplyr", "data.table", "ggplot2", "lubridate", "scales")
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
head(sonomaData)

#### Get dates recognized by R and remove weekday from date entries
#i <- 1
dateColumns <- c(grep("Deployed", names(sonomaData)), grep("Date", names(sonomaData))) # All columns with dates in them
for(i in dateColumns){
  sonomaData[,i] <- tstrsplit(sonomaData[,i], " - ", fixed = TRUE)[[2]] %>% as.vector() %>% mdy()
}
head(sonomaData)

#### Add trap count columns to dates data
# Convert column class to numeric
for(i in grep("Trap_Count", names(sonomaData))){
    sonomaData[,i] <- factor2numeric(sonomaData[,i])
}
head(sonomaData)
str(sonomaData)
## Save cleaned up data set
saveRDS(sonomaData, file = "output/sonoma_bgss_abundance_data.rds")

#### Remove columns for "Trap_up", "#_days_trap_up", and "Spittle_Bug"
trapCounts <- sonomaData %>% dplyr::select(., -contains("Trap_Up"), -contains("trap_up"), -contains("Spittle"), -contains("RHSS"))
# Remove other columns that provide litte information -- "Latitude", "Longitude", and "BGSS_Count"
# Trap coordinates are in "Latitude_(N)" and "Longitude_(W)"
trapCounts <- trapCounts %>% dplyr::select(., -Latitude, -Longitude, -contains("BGSS_Count"))

#### Transform to long format:
# All trap change dates in a single column
# All trap counts in a single column
trapCounts <- trapCounts %>% gather(., key = "date_num", value = "date", contains("Date")) %>% gather(., key = "trap_num", value = "trap_count", contains("Trap_Count"))
# Remove NAs from date and trap_count columns
trapCounts <- trapCounts %>% dplyr::filter(., !is.na(date) & !is.na(trap_count))
head(trapCounts)
str(trapCounts)


trapSummary <- trapCounts %>% group_by(vysite, date_num) %>% summarise(meanDate = mean(date), meanCount = mean(trap_count))

#### Make some figures

# Simple time-series plot with each vineyard site as a line
timeSeriesPlot <- ggplot(data=trapSummary, aes(x=meanDate, y=meanCount)) +
  #geom_line(aes(colour=vysite), size=1.25) +
  geom_point(aes(colour=vysite), size=2.5) +
  #geom_errorbar(aes(ymax=meancfu+secfu, ymin=meancfu-secfu), width=0.2) +
  scale_x_date(name = "Date collected", labels = date_format("%Y-%m-%d")) + 
  scale_y_continuous(name = "Number of BGSS") +
  # ylab("% insects on source plant") + 
  # ylim(c(0,100)) +
  # xlab("Weeks post inoculation") +
  theme_bw(base_size=18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank()) 
timeSeriesPlot
