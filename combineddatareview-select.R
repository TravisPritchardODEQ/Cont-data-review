library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)

fname1 <- "0095_34056_5194_TEMP_20080605_Hall Creek Lower Schrader Property Line_NA_.Rdata"
fname2 <- "0095_34058_5195_TEMP_20080605_Hall Creek Upper Schrader Property Line_NA_.Rdata"



###  LOCATION OF DATA FILES TO BE PROCESSED (This shouldn't change)
shiny_path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"



#Load first site as dataframe
load(paste0(shiny_path, fname1))
site1 <- tmp_data

#Load second site as dataframe
load(paste0(shiny_path, fname2))
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
        plot_ly(combineddata, x = ~DATETIME, y = ~r, color = ~site, symbol = ~charid,  key = ~key, colors = "Set2") %>%
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
