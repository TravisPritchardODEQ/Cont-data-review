library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)


#Directory to find files
path <-  "//deqlab1/Vol_Data/socoast/2013Submit/2013 Multiparameter Data Submit/"

#filename to write to
excelname <- "combineddata.xlsx" 


#vector of filenames in directory
in_fnames <- list.files(path, full.names = TRUE)

#Do not include files with "List of" in name
datafiles <- in_fnames[!grepl('List of', in_fnames)]

wb = createWorkbook()

#formatting excel import 
ctypes <-  c("guess",
             "date",
             "guess",
             "guess",
             "guess",
             "guess",
             "skip",
             "guess",
             "guess",
             "guess")

cnames <- c("LASAR_ID",
            "date",
            "time",
            "temp",
            "pH",
            "conductivity",
            "DO",
            "DO_sat",
            "comments")

LoggerIDnames <- c("DATE",
               "TIME",
               "TEMP_r",
               "TEMP_DQL",
               "PH_r",
               "PH_DQL",
               "COND_r",
               "COND_DQL",
               "DO_r",
               "DO_DQL",
               "DOs_r",
               "DOS_DQL")

LoggerIDtypes <- c("date",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess",
                   "guess")

#Create blank auditinfo table
#auditinfo <- data.frame(matrix(ncol = 9, nrow = 0))
#colnames(auditinfo) <- cnames


#Create empty list to use for later binding
auditlist = list()
smilist = list()



#Field audit into For loop
#For each datafile, pull out audit info and put into table

for (i in 1:length(datafiles)) {
  #############################################################################################################
  #Field audit results
  
  fname <- datafiles[i]
  #read portion of excel file with audit info
  table <-
    read_excel(
      fname,
      range = "C46:L52",
      col_types = ctypes,
      col_names = cnames
    )

  
  #read section with Lasar#
  lasar <-
    read_excel( fname,
              sheet = "LIMS_LASAR LOAD SHEET",
              range = "B3:B3",
              col_names = c("ID")
               
    ) 
  
  #Read logger info
  logger <- read_excel( fname,
                        sheet = "LIMS_LASAR LOAD SHEET",
                        range = "F3:F3",
                        col_names = c("logger")
  )
  
  
  table <- table %>%
    mutate(LASAR_ID = lasar$ID, LOGGER_ID = logger$logger) %>%
   mutate(time = strftime(time, format="%H:%M:%S", tz = "GMT")) %>%
    filter(!is.na(date))
  
auditlist[[i]] <- table
  
 ######################################################################################## 
  #Site master info

Site_description <-  read_excel( fname,
                                 sheet = "DS-QA",
                                 range = "D5:D5",
                                 col_names = c("desc"))


smitable <- Site_description %>%
  mutate(LOGGER_ID = logger$logger,
         LASAR_ID = lasar$ID)



smilist[[i]] <- smitable

#################################################################################
#data. Create data frames for each logger
log <- logger$logger[[1]]

datasheet <-
  read_excel(
    fname,
    range = "A6:L10000",
    sheet = "LIMS_LASAR LOAD SHEET",
    col_types = LoggerIDtypes,
    col_names = LoggerIDnames
  )

datasheet <- datasheet %>%
  filter(!is.na(DATE)) %>%
  replace(is.na(.), "") %>%
  mutate(TIME = strftime(TIME, format="%H:%M:%S", tz = "GMT")) %>%
  mutate(DATE = as.Date(DATE))

sheet <- createSheet(wb, log)
addDataFrame(datasheet, sheet = sheet, startRow = 5)

#write.xlsx(datasheet, file = excelname, sheetName = log,  append=TRUE)

 
 
  }



#write tables from lists
auditinfo <- bind_rows(auditlist)
smi <- bind_rows(smilist)


#Formate FieldAuditResults

FieldAuditResults <- auditinfo %>%
  gather(PARAMETER,
         AUDIT_RESULT,
         -LASAR_ID,
         -date,
         -time,
         -comments,
         -LOGGER_ID) %>%
  mutate(
    DATE = date,
    TIME = time,
    COMMENTS = comments,
    UNITS = "",
    AuditType = "",
    LOGGER_RESULT = "",
    AUDIT_EQUIPMENT_ID = "",
    DIFF = "",
    DQL = ""
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
  )


#Rename parameters

FieldAuditResults <- FieldAuditResults %>%
  mutate(PARAMETER = ifelse(PARAMETER == "temp","TEMP",
    ifelse(PARAMETER == "conductivity", "COND", 
    ifelse(PARAMETER == "pH", "PH",
    ifelse(PARAMETER == "DO_sat", "DOs", PARAMETER
                  )))))

#Enter units
FieldAuditResults <- FieldAuditResults %>%
  mutate(UNITS = ifelse(PARAMETER == "TEMP", "deg C",
                 ifelse(PARAMETER == "COND", "uS/cm", 
                 ifelse(PARAMETER == "PH", "SU",
                 ifelse(PARAMETER == "DOs", "%",
                 ifelse(PARAMETER == "DO", "Mg/L",
                        PARAMETER
                                          )))))) %>%
  mutate(AuditType = "in situ")

sheet = createSheet(wb, "FieldAuditResults")
addDataFrame(FieldAuditResults, sheet = sheet)

#format SMI table
SiteMasterInfo <- smi %>%
  rename(Station_Description = desc) %>%
  mutate(Logger_ID = LOGGER_ID,
         Site_ID = "",
         Decimal_Latitude = "",
         Decimal_Longitude = "",
         LAT_LONG_SOURCE = "",
         "Deploy_Depth(meters)" = "",
         Download_Date = "",
         DownloadTime = "",
         Logger_Date = "",
         Logger_Time =  "",
         Time_Diff = "",
         COMMENTS  = ""
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
  

sheet = createSheet(wb, "SiteMasterInfo")
addDataFrame(SiteMasterInfo, sheet = sheet, startRow = 6)


saveWorkbook(wb, paste0(path,excelname))


  # select(LOGGER_ID, LASAR_ID, Site_ID)


# table <-
#   read_excel(
#     paste0(path,"2010 08 Chetco Intertidal.xlsm"),
#     range = "C46:L52",
#     col_types = ctypes,
#     col_names = cnames
#   )
# 
# 
# logger <-
#   read_excel(
#     "//deqlab1/Vol_Data/socoast/2013Submit/2013 Multiparameter Data Submit/2010 08 Chetco Intertidal.xlsm",
#     sheet = "LIMS_LASAR LOAD SHEET",
#     range = "F3:F3",
#     col_names = c("logger")
#   )
# 
# table <- table %>%
#   mutate(site = logger$logger) %>%
#   mutate(time = strftime(time, format="%H:%M:%S")) %>%
#   filter(!is.na(date))
