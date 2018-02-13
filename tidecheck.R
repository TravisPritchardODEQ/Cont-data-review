library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(lubridate)

setwd("C:/Users/tpritch/Desktop/tides")



#read all tide data here. Be sure to reformat dates to yyyy/mm/dd HH:MM
tidedata <- read.csv("June2016.csv")


#load date from shiny
fname1 <- "0097_40024_10938353_TEMP_20160623_God Wants You Slough 2 (mid)_0_.Rdata"

###################################################################################################################
###################################################################################################################

###  LOCATION OF DATA FILES TO BE PROCESSED (This shouldn't change)
shiny_path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"


#Load site data from shiny folder as dataframe
load(paste0(shiny_path, fname1))
site1 <- tmp_data 

#Sort out some dates

site1$DATETIME <- as.POSIXct(site1$DATETIME, format = "%y%m%d %H:%M")
tidedata$DATETIME <- as.POSIXct(tidedata$DATETIME)

#add tide info
sitetide <- left_join(site1, tidedata, by = "DATETIME") 
 

##################################
### Start of shiny app graphing###
##################################

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush"),
  verbatimTextOutput("zoom")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- sitetide$DATETIME
    plot_ly(
      sitetide,
      x = ~ DATETIME,
      y = ~ r,
      color = ~ water_level,
      colors = "Blues",
      key = ~ key,
      type = 'scatter'
      
    ) %>%
      layout(
        dragmode = "zoom",
        legend = list(
          x = 0,
          y = -0.7,
          orientation = 'h'
        ),
        height = 900
      )
    
  })
  
  
  #  output$click <- renderPrint({
  #    d <- event_data("plotly_click")
  #    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  #  })
  #  
  #  
  #  output$zoom <- renderPrint({
  #    d <- event_data("plotly_relayout")
  #    if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  #  })
  
}

shinyApp(ui, server)


