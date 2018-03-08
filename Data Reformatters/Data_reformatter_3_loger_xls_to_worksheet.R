library(xlsx)
library(readxl)
library(tidyverse)

temp_converter <- function(temp_F) {
  temp_c <- (temp_F - 32) * (5 / 9) 
  return(temp_c)
}

wb = createWorkbook()

#vector of filenames in directory
in_fnames <- list.files("//deqlab1/Vol_Data/socoast/2009/2009 S Coast Submit/DEQ clipped/Files for import/", full.names = TRUE)
#get logger ID from filename and create vector
loggers <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(in_fnames))


for(i in 1:length(in_fnames)) {
  
  datafile <- in_fnames[i]

file <- read_excel(datafile)
  
filewithdates <- file %>%
  rename(DATETIME = "Date/Time", TEMP_r = "Temperature   (*F)")
  

data <- filewithdates %>%
  mutate(TIME = format(as.POSIXct(strptime(DATETIME,"%m/%d/%y %H:%M:%OS")) ,format = "%H:%M")) %>%
  mutate(DATE = as.Date(DATETIME, "%m/%d/%y %H:%M:%OS")) %>%
  select(DATE, TIME, TEMP_r) %>%
  mutate(TEMP_DQL = "") %>%
  mutate(TEMP_r = temp_converter(TEMP_r))


sheet = createSheet(wb, loggers[i])
addDataFrame(data, sheet = sheet, startRow = 5)

print(paste0("Finished File ", i, " of ", length(datafile)))

}

saveWorkbook(wb, "//deqlab1/Vol_Data/socoast/2009/2009 S Coast Submit/DEQ clipped/Files for import/import.xlsx")