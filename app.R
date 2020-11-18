library(shiny)
library(tidyverse)
library(ggpmisc)
library(ggthemes)
library(ggExtra)
library(ggseas)



# Load data ----
data <- read.csv("www/ECCCCombined_2018_test.csv")


# User interface ----
ui <- fluidPage(
  selectInput("NAPS", 
              label = "Choose a station to display",
              choices = unique(data$NAPS),
              selected = "10102"),
  sliderInput(inputId = "slider_time",
              label = "Time",  
              min = as.Date(min(data$Date_time)), max = as.Date(max(data$Date_time)), 
              value = c(as.Date(min(data$Date_time)), as.Date(max(data$Date_time)))),
  plotOutput("TimeseriesPlot"),
  
  # selectInput("Chem1", 
  #             label = "Choose a pollutant to display",
  #             choices = c("O3", "NO2", "Ox"),
  #             selected = "O3"),
  # 
  # selectInput("Chem2", 
  #             label = "Choose a pollutant to display",
  #             choices = c("O3", "NO2", "Ox"),
  #             selected = "NO2"),
  # 

  plotOutput("CompPlot")
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
  
}


# Run app ----
shinyApp(ui, server)