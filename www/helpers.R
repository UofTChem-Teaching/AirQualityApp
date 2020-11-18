









output$TimeseriesPlot <- renderPlot({
  
  #dat <- subset(data, NAPS == input$NAPS)
  ### Filter by date
  dat <- data[as.Date(data$Date_time) >= input$slider_time[1] & as.Date(data$Date_time) <= input$slider_time[2] , ]
  
  ggplot(data = subset(dat, NAPS == input$NAPS & data[as.Date(data$Date_time) >= input$slider_time[1] & as.Date(data$Date_time) <= input$slider_time[2] , ]), aes(x = as.POSIXct(Date_time))) + 
    stat_rollapplyr(aes(y = O3, colour = "O3"), width = 8, align = "right") +
    stat_rollapplyr(aes(y = NO2, colour = "NO2"), width = 8, align = "right") +
    stat_rollapplyr(aes(y = Ox, colour = "Ox"), width = 8, align = "right") +
    theme(legend.position="right") +
    scale_x_datetime() +
    theme_classic()
})