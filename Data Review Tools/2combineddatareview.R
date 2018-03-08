library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)

firstsite_name <- "lower_34056"
secondsite_name <- "upper_34058"
load('//deqlab1/WQM/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/0095_34056_5194_TEMP_20080605_Hall Creek Lower Schrader Property Line_NA_.Rdata')
firstsite <- as_tibble(tmp_data)
load('//deqlab1/WQM/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/0095_34058_5195_TEMP_20080605_Hall Creek Upper Schrader Property Line_NA_.Rdata')
secondsite <- as_tibble(tmp_data)



first <- firstsite %>%
  select(DATETIME, r) %>%
  mutate(site = firstsite_name)


second <- secondsite %>%
  select(DATETIME, r) %>%
  mutate(site = secondsite_name)

combineddata <- bind_rows(first, second)


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
        plot_ly(combineddata, x = ~DATETIME, y = ~r, color = ~site, symbol = ~site,  key = ~key) %>%
        layout(dragmode = "zoom")
    
  })
  
 
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  
  output$zoom <- renderPrint({
    d <- event_data("plotly_relayout")
    if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  })
  
}

shinyApp(ui, server)
