library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)

################################################################################
### IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT    ###
###       This script converts temp in F to C. If data is in C already,      ###
###            comment out lines 24-27 and uncomment lines 30-33             ###
################################################################################
### IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT    ###
################################################################################


#################################################################################
###   For creating 4R file from sheets found in the 2009 socoast dataset      ###
###   Audit info is pulled from master audit file                             ###
###   Data is contained in hobo csv files                                     ###
#################################################################################

#function to convert F to C
#if file is in C, comment out and uncomment next section
temp_converter <- function(temp_F) {
  temp_c <- (temp_F - 32) * (5 / 9) 
  return(temp_c)
}

#This function does nothing. Use it if data is in C already
# temp_converter <- function(temp_F) {
#   temp_c <- temp_F 
#   return(temp_c)
# }
########################################################################################
#Parent Directory
parentpath <- "//deqlab1/Vol_Data/socoast/2009/2009 S Coast Submit/"

#Directory to find data files
datapath <- "//deqlab1/Vol_Data/socoast/2009/2009 S Coast Submit/txt files/"

#Audit master File
Audit_Master <- "workingcopy2009 Audit Master_hobo.xls"

#filename to write to
excelname <- "4R2009SoCoastSubmit.xlsx"

#read tab info from Audit Master File
audit_tabs <- excel_sheets(paste0(parentpath,Audit_Master))

#Create empty list to use for later binding
field_audit_list = list()
pp_audit_list = list()

#formatting excel import 
field_ctypes <- c("date",
                  "date",
                  "guess",
                  "guess",
                  "skip",
                  "skip",
                  "skip",
                  "text")
pp_ctypes <- c("date",
               "guess",
               "guess",
               "guess")

pp_cnames <- c("TIME",
               "EXPECTED_RESULT",
               "LOGGER_RESULT",
               "DIFF")

data_cnames <- c("index",
                 "DATETIME",
                 "TEMP_r",
                 "X4",
                 "X5",
                 "X7",
                 "X6",
                 "X8")


#Create excel workbook to write data to
wb = createWorkbook()

#Create Site master table
Audit_Smi <- read_excel(paste0(parentpath,Audit_Master),sheet = "Thermo_Site_List", range = "A1:H30")

#Reformat
smi <- Audit_Smi %>%
  rename(
    Logger_ID = THERMO_NUM,
    LASAR_ID = LASAR_No,
    Site_ID = LocalNID,
    Station_Description = SITE_NAME,
    Decimal_Latitude = DLatitude,
    Decimal_Longitude = DLongitude,
    LAT_LONG_SOURCE = DCoord_Source,
    COMMENTS = "Monitoring Obj"
  ) %>%
  mutate(
    "Deploy_Depth(meters)" = "",
    Download_Date = "",
    DownloadTime = "",
    Logger_Date = "",
    Logger_Time = "",
    Time_Diff = ""
  ) %>%
  select(Logger_ID,
         LASAR_ID,
         Site_ID,
         Station_Description,
         Decimal_Latitude,
         Decimal_Longitude,
         LAT_LONG_SOURCE,
         "Deploy_Depth(meters)",
         Download_Date,
         DownloadTime,
         Logger_Date,
         Logger_Time,
         Time_Diff,
         COMMENTS)



#add to excel sheet
sheet = createSheet(wb, "SiteMasterInfo")
addDataFrame(smi, sheet = sheet, startRow = 6)

excel_sheets(paste0(parentpath,Audit_Master))

#Create table to use as lookup for adding LASAR_IDs to field audit table
smi_lookup <- smi %>%
  select(Logger_ID, LASAR_ID) %>%
  rename(LOGGER_ID = Logger_ID)







#Get audit info
for(i in 2:length(audit_tabs)) {
  tab_name <- audit_tabs[i]
  
  #read Portion of file that has field audit results
  field_audit <- read_excel(paste0(parentpath, Audit_Master),
                            sheet = tab_name,
                            range = "A25:H37",
                            col_types = field_ctypes)
  
  #reformat field audits to match template
  field_audit <- field_audit %>%
    filter(!is.na(Date)) %>%
    rename(
      AUDIT_EQUIPMENT_ID = X__1,
      DATE = Date,
      TIME = Time,
      AUDIT_RESULT = Audit,
      LOGGER_RESULT = Logger
    ) %>%
    mutate(
      LOGGER_ID = tab_name,
      LASAR_ID = "",
      PARAMETER = "TEMP",
      UNITS = "deg c",
      AuditType = "in situ",
      DIFF = "",
      DQL = "",
      COMMENTS = ""
    ) %>%
    select(
      LOGGER_ID,
      PARAMETER,
      UNITS,
      AuditType,
      DATE,
      TIME,
      AUDIT_RESULT,
      LOGGER_RESULT,
      AUDIT_EQUIPMENT_ID,
      DIFF,
      DQL,
      COMMENTS
    ) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(DATE = as.Date(DATE))%>%
    mutate(AUDIT_RESULT = temp_converter(AUDIT_RESULT),
           LOGGER_RESULT = temp_converter(LOGGER_RESULT)) %>%
    left_join(smi_lookup, by = "LOGGER_ID") %>%
    select(
      LOGGER_ID,
      LASAR_ID,
      PARAMETER,
      UNITS,
      AuditType,
      DATE,
      TIME,
      AUDIT_RESULT,
      LOGGER_RESULT,
      AUDIT_EQUIPMENT_ID,
      DIFF,
      DQL,
      COMMENTS
    )
  
  
           
  
  #write to list for later binding
  field_audit_list[[i-1]] <- field_audit
  
  #Pre/post audits
  pre_refID_tbl <-  read_excel(paste0(parentpath, Audit_Master),
                            sheet = tab_name,
                            range = "c13:k13",
                            col_types = c("text",
                                          "skip",
                                          "date",
                                          "skip",
                                          "skip",
                                          "skip",
                                          "text",
                                          "skip",
                                          "date"),
                            col_names = c("preLOGGER_ID",
                                          "predate",
                                          "postLOGGER_ID",
                                          "postdate"))
  
  
  pre_refID <- pre_refID_tbl[[1,2]]
  
  pre_data <- read_excel(paste0(parentpath, Audit_Master),
                        sheet = tab_name,
                        range = "A17:D22",
                        col_types = pp_ctypes,
                        col_names = pp_cnames) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(REFERENCE_ID = pre_refID_tbl[[1,1]], 
           PARAMETER = "TEMP",
           UNITS = "deg C",
           DATE_TIME = as.POSIXct(paste(pre_refID_tbl[[1,2]], TIME), format="%Y-%m-%d %H:%M:%S"),
           DQL = "",
           COMMENTS = "",
           LOGGER_ID = tab_name) %>%
    select(LOGGER_ID,
           PARAMETER,
           UNITS,
           DATE_TIME,
           EXPECTED_RESULT,
           LOGGER_RESULT,
           REFERENCE_ID,
           DIFF,
           DQL,
           COMMENTS)
  
  post_data <- read_excel(paste0(parentpath, Audit_Master),
                         sheet = tab_name,
                         range = "g17:j22",
                         col_types = pp_ctypes,
                         col_names = pp_cnames) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(REFERENCE_ID = pre_refID_tbl[[1,3]], 
           PARAMETER = "TEMP",
           UNITS = "deg C",
           DATE_TIME = as.POSIXct(paste(pre_refID_tbl[[1,4]], TIME), format="%Y-%m-%d %H:%M:%S"),
           DQL = "",
           COMMENTS = "",
           LOGGER_ID = tab_name) %>%
    select(LOGGER_ID,
           PARAMETER,
           UNITS,
           DATE_TIME,
           EXPECTED_RESULT,
           LOGGER_RESULT,
           REFERENCE_ID,
           DIFF,
           DQL,
           COMMENTS)
  
  ppdata <- bind_rows(pre_data, post_data)
  
  #convert F to C
  cor_ppdata <- ppdata %>%
    mutate(EXPECTED_RESULT = temp_converter(EXPECTED_RESULT),
           LOGGER_RESULT = temp_converter(LOGGER_RESULT))
  
  pp_audit_list[[i-1]] <- cor_ppdata
  
  
}

#bind field audit data together
field_auditinfo <- bind_rows(field_audit_list)
#bind pre/post data together
pp_audit_data <- bind_rows(pp_audit_list)

sheet = createSheet(wb, "FieldAuditResults")
addDataFrame(field_auditinfo, sheet = sheet)

sheet = createSheet(wb, "PrePostResults")
addDataFrame(pp_audit_data, sheet = sheet)

###################################################################################
###                                  Read data                                  ###
###################################################################################

#vector of filenames in directory
in_fnames <- list.files(datapath, full.names = TRUE)

#choose only csv
datafiles <- in_fnames[grepl('csv', in_fnames)]

#get logger ID from filename and create vector
loggers <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(datafiles))
 



for(i in 1:length(datafiles)) {
  
  datafile <- datafiles [i]
  
  print(paste0("starting File ", i, " of ", length(datafiles)))
  
  loggerdata <- read_csv(datafile, 
                         skip = 2,
                         col_names = FALSE)

  
  temp_data <- loggerdata %>%
    select(X2, X3) %>%
    rename(DATETIME = X2, TEMP_r = X3) %>%
    mutate(DATE = as.Date(DATETIME, "%m/%d/%y %I:%M:%S %p")) %>%
    mutate(TIME = format(as.POSIXct(strptime(DATETIME,"%m/%d/%y %I:%M:%S %p")) ,format = "%H:%M")) %>%
    mutate(TEMP_r = temp_converter(TEMP_r)) %>%
    select(DATE, TIME, TEMP_r) %>%
    mutate(TEMP_DQL = "")
  
    
  
  
  sheet = createSheet(wb, loggers[i])
  addDataFrame(temp_data, sheet = sheet, startRow = 5)
  
  print(paste0("Finished File ", i, " of ", length(datafiles)))
  
  }

saveWorkbook(wb, paste0(parentpath,excelname))



