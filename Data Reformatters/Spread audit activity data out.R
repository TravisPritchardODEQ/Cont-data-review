
read.csv("C:/Users/tpritch/Desktop/Book1.csv")

table <- read.csv("C:/Users/tpritch/Desktop/Book1.csv", colClasses = "character", na.strings = c("", "NA")) 



for (i in seq_along(table$CONT_TEMP_ACC_COMMENTS)) {
  table$auditype[i] <- table$CONT_TEMP_ACC_COMMENTS[i]
  table$auditype[i] <- ifelse(!is.na(table$auditype[i]), table$auditype[i], table$auditype[i-1])
    
}

