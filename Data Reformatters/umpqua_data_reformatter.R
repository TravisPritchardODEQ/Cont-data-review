library(tidyverse)
library(xlsx)
library(readxl)
library(stringi)
library(lubridate)


#########################################################################################
###     This script pulls together data from 2010 and earlier umpqua dataset to       ###
###                              match the cont. data template                        ###
#########################################################################################


# Script Setup ------------------------------------------------------------


#pathway for working documents
filepath <- "//deqlab1/Vol_Data/umpqua/2009/2009 Reference Temperature Files/"

save_path <- "//deqlab1/Vol_Data/umpqua/2009/2009 Reference Temperature Files/4r2009UmpquaRefCnTemp.xlsx"

#load in lookuptable for lasar IDs
load("E:/Documents/Code/umpqua_ref_site_lookup.RData")
#change from factor to character
site_lookup <-  data.frame(lapply(site_lookup, as.character), stringsAsFactors=FALSE)

#create excel workbook
wb = createWorkbook()

#create needed lists for binding
pp_audit_list = list()
field_audit_list = list()


#Gather filenames from the filepath folder
in_fnames <- list.files(filepath, full.names = TRUE)

#get a vector for the summary table
sumfile <- in_fnames[grepl('A Summary of Ref', in_fnames)]
nonsumfile <- in_fnames[!grepl('A Summary of Ref', in_fnames)]

#get vector for audit files
auditfiles <- nonsumfile[grepl('Audit', nonsumfile)]

#get vector for datafiles
datafiles <- in_fnames[!grepl('A Summary of Ref', in_fnames)]
datafiles <- datafiles[!grepl('Audit', datafiles)]


# Site Master Info Table --------------------------------------------------


#load in summary file
sumtable <- read_excel(sumfile, range = "A7:P12") 


#wrangle summary file into smi table
smi <- sumtable %>%
  rename(Logger_ID = "Unit Number",
         Site_ID_yr = "Unit Number__1",
         Station_Description = "Site Name",
         Decimal_Latitude = "Latitude",
         Decimal_Longitude = "Longitude",
         "Deploy_Depth(meters)" = "Depth (m)",
         Download_Date = "Downloaded Date",
         DownloadTime = Time,
         Logger_Date = "Logger Date",
         Logger_Time = "Logger Time") %>%
  mutate(Site_ID = stri_sub(Site_ID_yr,1, -3)) %>%
  left_join(site_lookup, by = "Site_ID") %>%
  mutate(Time_Diff = "", COMMENTS = "", LAT_LONG_SOURCE = "" ) %>%
  select(Logger_ID, LASAR_ID, Logger_ID, Site_ID_yr,Station_Description,
         Decimal_Latitude, Decimal_Longitude, LAT_LONG_SOURCE, 
         "Deploy_Depth(meters)", Download_Date, DownloadTime,
         Logger_Date, Logger_Time, Time_Diff, COMMENTS ) %>%
  rename(Site_ID = Site_ID_yr) %>%
  mutate(Logger_Time = strftime(Logger_Time, format="%H:%M:%S"), 
         DownloadTime = strftime(DownloadTime, format="%H:%M:%S"))

#make as dataframe so that xlsx::addDataFrame can write without row names
smi <- data.frame(smi)

#create logger lookup table
logger_lookup <- smi %>%
  select(Logger_ID, LASAR_ID) %>%
  rename(LOGGER_ID = Logger_ID)

#add smi to workbook
sheet = createSheet(wb, "SiteMasterInfo")
addDataFrame(smi, sheet = sheet, startRow = 6, row.names = FALSE)


# Pre/Post Audits and Field Audit Results ---------------------------------


#For loop to pull together audit info from all the datasheets
for (i in 1:length(auditfiles)){
  file <-  auditfiles[i]
  
  #read from info. Not sure if needed
  audit_info <- read_excel(file, sheet = 3, range = "A1:B16", col_names  = FALSE) 
  
  logger <- audit_info[[5,2]]
  site <- audit_info[[4,2]]
  
  # read audit data from sheet
  audit_data <- read_excel(file, sheet = 4) %>%
    mutate(auditype = "" ) %>%
    filter(LOGGER_ID != "")
  
  #spread the audit types out
  for (m in seq_along(audit_data$CONT_TEMP_ACC_COMMENTS)) {
    audit_data$auditype[m] <- audit_data$CONT_TEMP_ACC_COMMENTS[m]
    audit_data$auditype[m] <- ifelse(!is.na(audit_data$auditype[m]), audit_data$auditype[m], audit_data$auditype[m-1])
    
  }
    
  #get dataframe of only field audits
  field_audits <- audit_data %>%
    filter(grepl("Field", ignore.case = TRUE, auditype)) %>%
    rename(UNITS = "CONT_TEMP_ACC_UNITS",
           DATE_TIME = "CONT_TEMP_ACC_DATE_TIME" ,
           AUDIT_RESULT = "CONT_TEMP_ACC_AUDIT_RESULT",
           AUDIT_EQUIPMENT_ID = "CONT_TEMP_ACC_AUDIT THERMOMETER_ID",
           DIFF = "CONT_TEMP_ACC_DIFFERENCE" ,
           DQL = "CONT_TEMP_ACC_DQL",
           COMMENTS = auditype) %>%
    mutate(PARAMETER = 'TEMP', UNITS = "deg C", AuditType = "in situ" ) %>%
    left_join(logger_lookup, by = "LOGGER_ID") %>%
    mutate(DATE = as.Date(DATE_TIME), TIME = format(ymd_hms(DATE_TIME), "%H:%M:%S" ))  %>%
    select(LOGGER_ID, LASAR_ID, PARAMETER, UNITS,
           AuditType, DATE, TIME, AUDIT_RESULT,
           LOGGER_RESULT, AUDIT_EQUIPMENT_ID, 
           DIFF, DQL, COMMENTS)
    
field_audit_list[[i]] <- field_audits
  
  
  #get dataframe of pre/post audits
  ppaudits <-  audit_data %>%
    filter(!grepl("Field", ignore.case = TRUE, auditype)) %>%
    rename(UNITS = "CONT_TEMP_ACC_UNITS",
           DATE_TIME = "CONT_TEMP_ACC_DATE_TIME" ,
           EXPECTED_RESULT = "CONT_TEMP_ACC_AUDIT_RESULT",
           REFERENCE_ID = "CONT_TEMP_ACC_AUDIT THERMOMETER_ID",
           DIFF = "CONT_TEMP_ACC_DIFFERENCE" ,
           DQL = "CONT_TEMP_ACC_DQL",
           COMMENTS = auditype) %>%
    mutate(PARAMETER = 'TEMP', UNITS = "deg C" ) %>%
    select(LOGGER_ID, PARAMETER, UNITS, DATE_TIME, 
           EXPECTED_RESULT, LOGGER_RESULT, REFERENCE_ID,
           DIFF, DQL, COMMENTS)
  
  #put ppaudits into list for later binding
  pp_audit_list[[i]] <- ppaudits
  
}

#bind field audit data together
field_auditinfo <- bind_rows(field_audit_list)
field_auditinfo <- data.frame(field_auditinfo)
#bind pre/post data together
pp_audit_data <- bind_rows(pp_audit_list)
pp_audit_data <- data.frame(pp_audit_data)


#put audit info into workbook
sheet = createSheet(wb, "FieldAuditResults")
addDataFrame(field_auditinfo, sheet = sheet, row.names = FALSE)

sheet = createSheet(wb, "PrePostResults")
addDataFrame(pp_audit_data, sheet = sheet, row.names = FALSE)


# Logger Data -------------------------------------------------------------


#get data
for (j in 1:length(datafiles)) {
  print(paste0("starting ",datafiles[j],  "- File ", j, " of ", length(datafiles)))
  datfile <-  datafiles[j]
  
  #get loggerID from filename
  logger <- strsplit(basename(datfile), "[_]")[[1]][1]
  
  #read and wrangle data
  loggerdata <- read_excel(datfile, range = "A2:B20000", col_names  = c("DATETIME", "TEMP_r")) %>%
    mutate(DATE = as.Date(DATETIME), TIME = format(ymd_hms(DATETIME), "%H:%M:%S" ), TEMP_DQL = "")  %>%
    select(DATE, TIME, TEMP_r, TEMP_DQL)
  
  loggerdata <- data.frame(loggerdata)
  
  #add data to workbook
  sheet = createSheet(wb, logger)
  addDataFrame(loggerdata, sheet = sheet, startRow = 5, row.names = FALSE)
  
  print(paste0("Finished File ", j, " of ", length(datafiles))) 
  
  }


# Write to Excel file -----------------------------------------------------


saveWorkbook(wb, save_path)
