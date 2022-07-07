### Functions called for Air Quality App


# Googlesheets helpers ----


## Saves student num & Assigned data to google sheets
saveData <- function(data, sheet_id ) {
  # The data must be a dataframe rather than a named vector
  data <- data %>%
    as.list() %>%
    data.frame()
  # Add the data as a new row
  sheet_append(sheet_id, data)
}


# imports student google sheets data as data.frame
loadData <- function(sheet_id ) {
  # Read the data
  read_sheet(sheet_id, col_names = TRUE)
}

# Plotting functions ----
# Functions used for time-series and correlation scatter plots 


## Reactive subsetting of NAPS data based on inputs, used for both plots & summary stats
stationDataPrep<- function(data, naps, start_date, end_date){

  df <- data %>%
    filter(NAPS == .env$naps) %>%
    filter(Date >= .env$start_date & Date <= .env$end_date) %>%
    pivot_longer(
      cols = starts_with("H"),
      names_to = c("Hour", "Pollutant"),
      names_sep = "_",
      values_to = "Concentration"
    ) %>%
    pivot_wider(names_from = 'Pollutant',
                values_from = 'Concentration') %>%
    mutate(Date_time = paste0(Date, " ", Hour, ":00")) %>%
    mutate(Date_time = lubridate::parse_date_time(Date_time, "%Y-%m-%d %H:%M") - lubridate::hours(1)) %>%
    mutate(Ox = NO2 + O3,
           NO2_8hr = zoo::rollmean(NO2, k = 7, fill = NA, align = "right"),
           O3_8hr = zoo::rollmean(O3, k = 7, fill = NA, align = "right"),
           Ox_8hr = zoo::rollmean(Ox, k = 7, fill = NA, align = "right")) 
  df
}

## base time-series plot

timeSeriesPlot <- function(data){
  
  fig <- plot_ly(data, x = ~Date_time) %>%
    layout(
      xaxis = list(
        title = "Time",
        showgrid = F,
        rangeselector = list(
          buttons = list(
            list(
              count = 8,
              label = "8 hrs",
              step = "hour",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 day",
              step = "day",
              stepmode = "backward"),
            list(
              count = 7,
              label = "7 days",
              step = "day",
              stepmode = "backward"),
            
            list(step = "all"))),
        
        rangeslider = list(type = "date")),
      
      yaxis = list(title = "Concentration (ppb)",
                   showgrid = F)) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "toggleSpikelines", "zoom2d", "resetScale2d" )) %>%
    layout(margin = list(b = 20, l = 40, r = 40, t = 100, pad = 0, autoexpand = TRUE))
  fig
}

# Base scatter/correlation plot

scatterPlot <- function(data){
  
  p <- ggplot(data = data, 
              aes(x = NO2, y = O3)) +
    geom_smooth(method='lm', formula= y~x, se=F) +
    stat_poly_eq(formula = y~x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE,
                 label.x.npc = 1) +
    xlab("1hr Concentration NO2 (ppb)") +
    ylab("1hr Concentration O3 (ppb)") 
  p
}


