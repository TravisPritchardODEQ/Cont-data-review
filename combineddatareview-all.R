#Script overlays 2 continuous datasets for compaisons
#Graphs are colored by filename
#Different symbols represent different parameters

library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)


###  LOCATION OF DATA FILES TO BE PROCESSED (This shouldn't change)
shiny_path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"

combined_data <- data.frame()
datafiles <- list.files(shiny_path, pattern = '_.Rdata')


for (i in seq_along(datafiles)) {
  load(paste0(shiny_path, datafiles[[i]]))
  selected_data <- select(tmp_data, DATETIME, charid, r, rDQL)
  selected_data$filename <- datafiles[[i]]
  combined_data <- rbind(combined_data, selected_data) 
}


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
    key <- combined_data$DATETIME
        plot_ly(combined_data, 
                x = ~DATETIME, 
                y = ~r, 
                color = ~filename, 
                symbol = ~charid, 
                key = ~key, 
                colors = "Dark2", 
                type = 'scatter', 
                mode = 'lines') %>%
        
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
