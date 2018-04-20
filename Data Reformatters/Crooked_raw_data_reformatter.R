library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)





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
# temp_converter <- function(temp_F) {
#   temp_c <- (temp_F - 32) * (5 / 9) 
#   return(temp_c)
# }

#This function does nothing. Use it if data is in C already
temp_converter <- function(temp_F) {
  temp_c <- temp_F
  return(temp_c)
}


########################################################################################
#Parent Directory
parentpath <- "//deqlab1/Vol_Data/crooked/Temperature/ODEQ 2014 Submittal/"

#Directory to find data files
datapath <- "//deqlab1/Vol_Data/crooked/Temperature/ODEQ 2014 Submittal/2014 Raw Field Data/"

#Audit master File
Audit_Master <- "CT_Audit 2014.xlsx"
#stow_audit_master <- "WorkingCopy2009 Audit Master_stowaway.xls"

#filename to write to
excelname <- "4R2014CrookedCnTemp.xlsx"

#read tab info from Audit Master File
audit_tabs <- excel_sheets(paste0(parentpath,Audit_Master))
#stow_audit_tabs <- excel_sheets(paste0(parentpath,stow_audit_master))

#Create empty list to use for later binding
field_audit_list = list()
pp_audit_list = list()
smi_list = list()
#stow_field_audit_list = list()
#Stow_pp_audit_list = list()

#formatting excel import 
field_ctypes <- c("date",
                  "date",
                  "guess",
                  "guess",
                  "skip",
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

smi_col_names <- c("Logger_ID", 
                   "LASAR_ID", 
                   "Site_ID", 
                   "Station_Description", 
                   "Decimal_Latitude", 
                   "Decimal_Longitude",
                   "LAT_LONG_SOURCE",
                   "Deploy_Depth(meters)",
                   'Download_Date',
                   "DownloadTime",
                   "Logger_Date",
                   "Logger_Time",
                   "Time_Diff",
                   "COMMENTS")



#Create excel workbook to write data to
wb = createWorkbook()



excel_sheets(paste0(parentpath,Audit_Master))


# Get audit and SMI info ---------------------------------------------------



for(i in 1:length(audit_tabs)) {
  
  print(paste("starting audits",audit_tabs[i],  "- File ", i, " of ", length(audit_tabs)))
  tab_name <- audit_tabs[i]
  
  
  #get logger info
  audit_logger <- read_excel(paste0(parentpath, Audit_Master),
                                        sheet = tab_name,
                                        range = "A8:D13")
  logger <- audit_logger[[1,4]]
  ref_therm <- audit_logger[[5,3]]
  
  #read Portion of file that has field audit results
  field_audit <- read_excel(paste0(parentpath, Audit_Master),
                            sheet = tab_name,
                            range = "A25:I37",
                            col_types = field_ctypes)
  
  #reformat field audits to match template
  field_audit_temp <- field_audit %>%
    filter(!is.na(Date)) %>%
    rename(
      DATE = Date,
      TIME = Time,
      AUDIT_RESULT = Audit,
      LOGGER_RESULT = Logger,
      COMMENTS = Comments) %>%
    mutate(
      LOGGER_ID = logger,
      LASAR_ID = "",
      PARAMETER = "TEMP",
      UNITS = "deg c",
      AuditType = "in situ",
      DIFF = "",
      DQL = "",
      COMMENTS = "",
      AUDIT_EQUIPMENT_ID = ref_therm,
    ) %>%
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
    ) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(DATE = as.Date(DATE))%>%
    mutate(AUDIT_RESULT = temp_converter(AUDIT_RESULT),
           LOGGER_RESULT = temp_converter(LOGGER_RESULT))
   
  
           
  
  #write to list for later binding
  field_audit_list[[i]] <- field_audit_temp
  
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
  
  post_refID_tbl <-  read_excel(paste0(parentpath, Audit_Master),
                               sheet = tab_name,
                               range = "c42:k42",
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
  
  
  pre_date <- pre_refID_tbl[[1,2]]
  pre_temp_log <- pre_refID_tbl[[1,1]]
  post_date <- post_refID_tbl[[1,2]]
  post_temp_log <- post_refID_tbl[[1,1]]
  
  pre_data_low <- read_excel(paste0(parentpath, Audit_Master),
                        sheet = tab_name,
                        range = "A17:D22",
                        col_types = pp_ctypes,
                        col_names = pp_cnames) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(REFERENCE_ID = pre_temp_log, 
           PARAMETER = "TEMP",
           UNITS = "deg C",
           DATE_TIME = as.POSIXct(paste(pre_date, TIME), format="%Y-%m-%d %H:%M:%S"),
           DQL = "",
           COMMENTS = "",
           LOGGER_ID = logger) %>%
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
  
  pre_data_high <- read_excel(paste0(parentpath, Audit_Master),
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
           LOGGER_ID = logger) %>%
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
  
  pre_data <- bind_rows(pre_data_high, pre_data_low)
  
  post_data_low <- read_excel(paste0(parentpath, Audit_Master),
                             sheet = tab_name,
                             range = "A46:D51",
                             col_types = pp_ctypes,
                             col_names = pp_cnames) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(REFERENCE_ID = post_temp_log, 
           PARAMETER = "TEMP",
           UNITS = "deg C",
           DATE_TIME = as.POSIXct(paste(post_date, TIME), format="%Y-%m-%d %H:%M:%S"),
           DQL = "",
           COMMENTS = "",
           LOGGER_ID = logger) %>%
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
  
  post_data_high <- read_excel(paste0(parentpath, Audit_Master),
                              sheet = tab_name,
                              range = "g46:j51",
                              col_types = pp_ctypes,
                              col_names = pp_cnames) %>%
    mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
    mutate(REFERENCE_ID = post_refID_tbl[[1,3]], 
           PARAMETER = "TEMP",
           UNITS = "deg C",
           DATE_TIME = as.POSIXct(paste(post_refID_tbl[[1,4]], TIME), format="%Y-%m-%d %H:%M:%S"),
           DQL = "",
           COMMENTS = "",
           LOGGER_ID = logger) %>%
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
  
  post_data <- bind_rows(post_data_low, post_data_high)
  ppdata <- bind_rows(pre_data, post_data)
  
  
  #convert F to C
  cor_ppdata <- ppdata %>%
    mutate(EXPECTED_RESULT = temp_converter(EXPECTED_RESULT),
           LOGGER_RESULT = temp_converter(LOGGER_RESULT))
  
  pp_audit_list[[i]] <- cor_ppdata
  
  print(paste("starting SMI",audit_tabs[i],  "- File ", i, " of ", length(audit_tabs)))
  
  Smi_import <- read_excel(paste0(parentpath, Audit_Master),
                           sheet = tab_name,
                           range = "A2:j11")
  
  smi <- data.frame(matrix(ncol = 14, nrow = 1))
  colnames(smi) <-smi_col_names 
  
  smi <- smi  %>%
    mutate(Logger_ID = logger,
           Site_ID = Smi_import[[1,3]],
           Station_Description = Smi_import[[5,7]],
           Decimal_Latitude = Smi_import[[4,4]],
           Decimal_Longitude = Smi_import[[5,4]],
           `Deploy_Depth(meters)` = Smi_import[[8,2]])
  
  smi_list[[i]] <- smi
  
 
  
  
}

#bind field audit data together
field_auditinfo <- bind_rows(field_audit_list)
#bind pre/post data together
pp_audit_data <- bind_rows(pp_audit_list)
#bind smi data together
smi <- bind_rows(smi_list)

addWorksheet(wb,"FieldAuditResults")
writeDataTable(wb, "FieldAuditResults", field_auditinfo)


addWorksheet(wb, "PrePostResults")
writeDataTable(wb, "PrePostResults", pp_audit_data)


#add SMI to excel sheet
addWorksheet(wb, "SiteMasterInfo")
writeDataTable(wb, "SiteMasterInfo", smi, startRow = 6)

logger_lookup <- smi %>%
  select(Site_ID, Logger_ID)

###################################################################################
###                                  Read data                                  ###
###################################################################################

#vector of filenames in directory
in_fnames <- list.files(datapath, full.names = TRUE)

#choose only csv
datafiles <- in_fnames[grepl('csv', in_fnames)]





for(i in 1:length(datafiles)) {
  
  print(paste0("starting ",datafiles[i],  "- File ", i, " of ", length(datafiles)))
 
   datafile <- datafiles[i]
  
  
  #get logger ID from filename and create vector
  Site_name <- tools::file_path_sans_ext(basename(datafile))
  
  
  data <- read_csv(datafile, col_names = FALSE, skip = 2) %>%
    rename(DATE = X1,
           TIME = X2, 
           TEMP_r = X3) %>%
    select(DATE, TIME, TEMP_r) %>%
    mutate(TEMP_DQL = "",
           Site_ID = Site_name) %>%
    left_join(logger_lookup, by = "Site_ID")
  
  logger_dat <- if_else(is.na(data[[1,6]]), data[[1,5]], data[[1,6]])
  
  data <- data %>%
    select(DATE, TIME, TEMP_r, TEMP_DQL)
  

  addWorksheet(wb, logger_dat)
  writeDataTable(wb, logger_dat, data, startRow = 6)
  
  
  
  
  print(paste0("Finished File ", i, " of ", length(datafiles)))
  
  }

saveWorkbook(wb, paste0(parentpath,excelname), overwrite = TRUE)



