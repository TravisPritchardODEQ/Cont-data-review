library(readxl)
library(dplyr)


#Directory ot find files
path = "//deqlab1/Vol_Data/socoast/2013Submit/2013 Multiparameter Data Submit/"


#vector of filenames in directory
in_fnames <- list.files(path, full.names = TRUE)

#Do not include files with "List of" in name
datafiles <- in_fnames[!grepl('List of', in_fnames)]


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

cnames <- c("site",
            "date",
            "time",
            "temp",
            "pH",
            "conductivity",
            "DO",
            "DO_sat",
            "comments")

#Create blank auditinfo table
auditinfo <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(auditinfo) <- cnames


#Create empty list to use for later binding
datalist = list()



#Field audit into For loop
#For each datafile, pull out audit info and put into table

for (m in 1:length(datafiles)) {
  
  fname <- datafiles[m]
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
               range = "C5:C5",
               col_names = c("ID")
               
    ) 
  
  
  table <- table %>%
    mutate(site = lasar$ID) %>%
    mutate(time = strftime(time, format="%H:%M:%S")) %>%
    filter(!is.na(date))
  
datalist[[m]] <- table
  
  
  
  
  }

auditinfo <- bind_rows(datalist)

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
