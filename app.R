library(shiny)
library(tidyverse)
library(ggpmisc)
library(ggthemes)
library(ggExtra)
library(ggseas)
library(leaflet)


# Load data ----
data <- read.csv("www/ECCCCombined_2018_test.csv")

mapInfo <- data %>%
  distinct(NAPS, .keep_all = TRUE) %>%
  select(c(NAPS, City, P, Latitude, Longitude))
  


# User interface ----
ui <- fluidPage(
  
  titlePanel("Testing Shiny Applet for the visualization of Air Quality Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("NAPS", 
                  label = "Choose which station's data to display",
                  choices = unique(data$NAPS),
                  selected = "10102"),
      sliderInput(inputId = "slider_time",
                  label = "Select time range to be displayed",  
                  min = as.Date(min(data$Date_time)), max = as.Date(max(data$Date_time)), 
                  value = c(as.Date(min(data$Date_time)), as.Date(max(data$Date_time)))),
      "Browsw the map below to find all NAPS stations contained in the current dataset; click on a pin to see that stations NAPS ID number which you can input above.",
      leafletOutput("mymap")
    ),
    
    mainPanel(
      "Below is a time series plot of NO2, O3, and Ox concentrations. Note that an 8hr moving average is applied to smooth data. **Future versions might have this as an On/Off option.",
      plotOutput("TimeseriesPlot"),
      "Below is a correlation plot of NO2 vs O3 over the time span plotted above. Read below for notes.",
      plotOutput("CompPlot"),
      "*Notes*: (1) The equation of best fit, and its R^2 value are plotted in the top left; (2) The plot contains marginal histograms which show the 1-dimensional distribution of the corresponding axis-variable, and (3) To avoid overplotting the transparancey (alpha) of ind. points has been decreased (darker spots mean a denser number of points) and random noise (read jitter) has been added to each point so they don't overlap, this does not affect the marginal histogram or the line of best fit."
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  
  output$TimeseriesPlot <- renderPlot({
    
    #dat <- subset(data, NAPS == input$NAPS)
    ### Filter by date
    dat <- data[as.Date(data$Date_time) >= input$slider_time[1] & as.Date(data$Date_time) <= input$slider_time[2] , ]
    
  ggplot(data = subset(dat, NAPS == input$NAPS), aes(x = as.POSIXct(Date_time))) + 
    stat_rollapplyr(aes(y = O3, colour = "O3"), width = 8, align = "right") +
    stat_rollapplyr(aes(y = NO2, colour = "NO2"), width = 8, align = "right") +
    stat_rollapplyr(aes(y = Ox, colour = "Ox"), width = 8, align = "right") +
    theme(legend.position="right") +
    scale_x_datetime() +
    theme_classic()
  })
  
  output$CompPlot <- renderPlot({
    
    #dat <- subset(data, NAPS == input$NAPS)
    ### Filter by date
    dat <- data[as.Date(data$Date_time) >= input$slider_time[1] & as.Date(data$Date_time) <= input$slider_time[2] , ]
    
    my.formula <- y ~ x
    
    p <- ggplot(data = subset(dat, NAPS == input$NAPS), aes(x = NO2, y = O3)) + 
      geom_jitter(alpha = .1) +
      geom_smooth(method='lm', formula= y~x, se=F) +
      stat_poly_eq(formula = y~x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      theme_classic()
    ggMarginal(p, type = "density")
    
    })
    
  output$mymap <- renderLeaflet({
      leaflet(data = mapInfo) %>% addTiles() %>%
        addMarkers(~Longitude, ~Latitude, popup = ~paste(City, ", NAPS: ", NAPS), label = ~paste(City, ", NAPS: ", NAPS))
    })
  
    
   
  
}


# Run app ----
shinyApp(ui, server)