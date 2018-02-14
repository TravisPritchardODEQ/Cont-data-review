library(readxl)
library(dplyr)

path = "//deqlab1/Vol_Data/socoast/2013Submit/2013 Multiparameter Data Submit/"


in_fnames <- list.files(path, full.names = TRUE)
datafiles <- in_fnames[!grepl('List of', in_fnames)]


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

auditinfo <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(auditinfo) <- cnames

datalist = list()



for (m in 1:length(datafiles)) {
  
  fname <- datafiles[m]
  table <-
    read_excel(
      fname,
      range = "C46:L52",
      col_types = ctypes,
      col_names = cnames
    )

  
  logger <-
    read_excel(
      datafiles[m],
      sheet = "LIMS_LASAR LOAD SHEET",
      range = "F3:F3",
      col_names = c("logger")
    )
  
  
  table <- table %>%
    mutate(site = logger$logger) %>%
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
