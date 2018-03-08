library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)

fname1 <- "S:/DataManagement/ContinuousDataRTool/Data in Waiting/data/0106_30147_2251473_TEMP_20110615_Camp Creek at mouth_0_.Rdata"
fname2 <- "//deqlab1/Vol_Data/umpqua/2012/ReferenceTemp12/RFiles/0091_30147_2251473_TEMP_20120617_Camp Creek at mouth_0_.Rdata"



###  LOCATION OF DATA FILES TO BE PROCESSED (This shouldn't change)
shiny_path <- "E:/ShinyDataFolder/SoCoast/2017/data/"



#Load first site as dataframe
load(fname1)
site1 <- tmp_data

#Load second site as dataframe
load(fname2)
site2 <- tmp_data

site1$site <- fname1
site2$site <- fname2

#combine data as 1 frame
combineddata <- bind_rows(site1, site2)



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
    key <- combineddata$DATETIME
        plot_ly(combineddata, x = ~DATETIME, y = ~r, mode = 'lines', color = ~site, symbol = ~charid,  key = ~key, colors = "Dark2") %>%
        layout(dragmode = "zoom",
               legend = list(x = 0, y = -0.7, orientation = 'h'),
               height = 900)
    
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
