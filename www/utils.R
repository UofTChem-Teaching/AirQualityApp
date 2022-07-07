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
  
}