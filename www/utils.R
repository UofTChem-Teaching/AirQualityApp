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
  read_sheet(sheet_id, col_names = TRUE) %>%
    mutate(start_date = round_date(start_date, unit = "hour"))
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

# Functions for student data assignments ---- 

# Convert date-time to excel serial 

dateToExcel <- function(date){
  d0 <- as_datetime(0, origin = "1899-12-30 00:00:00", tz = "UTC")
  d1 <- date
  
  d <- as.numeric(d1-d0)
  d
}

# Functions inputs,logins, etc.  ---- 

# Hashes input ID using sha256 
hashID <- function(id){
  paste(sodium::sha256(charToRaw(as.character(id))), collapse = " ")
}



# Check to see if valid UofT ID
# UofT student numbers contain 10 digits
# wiggle room on either end
checkUofTID <- function(id){
   
  id <- as.character(id)
  
  if (!str_detect(id, "^[0-9]+$")) { 
    ID <- FALSE
  # } else if (str_count(id) < 9) {
  #   ID <- FALSE
  # } else if (str_count(id) > 11) {
  #   ID <- FALSE
  } else {
    ID <- TRUE
  }
  ID
}

# Checks if student was already assigned data set
# if not, assignes them one
# outputs a single row data.frame matching the layout of the GoogleSheets
assignedVals <- function(df, id, courseData = courseData, naps_stations = naps_stations, sheet_id = SHEET_ID){
  # df is google sheets with assigned datasets
  # id is student number 
  # courseData is prepapred .csv of toronto NAPS for students loaded at top of App
  # naps_stations is unique naps stations in courseData
  # sheet_id is url to google sheets, at top of the app 
  
  id <- hashID(id)
  
  if(id %in% df$student_number){
    student_vals <- df[df$student_number == id, ] 
  } else {
    
    # Random NAPS station from available
    student_naps <- as.character(sample(naps_stations, 1))
    
    # filter for NAPS station
    data_naps <- as.data.frame(courseData[courseData$NAPS == student_naps, ])
    
    # Pick random start date, giving >= 7 days of data
    n_row <- sample((nrow(data_naps) - 168), 1)
    
    # Starting date as value
    startDate <- data_naps[n_row, "Time"]
    
    # collecting student's assigned values 
    student_vals <- data.frame(
      "student_number" = id,
      "naps_station" = student_naps,
      "start_date" = startDate,
      "error_row" = sample(c(1:168), size = 1),
      "date_created" = Sys.time()
    )
    
    # saving student's newly assigned values to Google Sheets 
    saveData(student_vals, sheet_id = sheet_id)
  }
  
  # returning one row data.frame of student's values 
  as.data.frame(student_vals)
  
}

# Using student's assigned values, subsets and returns dataset for student
studentDataset <- function(student_vals,  courseData = courseData ){

  student_naps <- as.integer(student_vals[1, "naps_station"])
  student_date <- student_vals[1, "start_date"]
  student_error <- student_vals[1, "error_row"]
  
  # filter for NAPS station
  start_row <- which(courseData$NAPS == student_naps & courseData$Time == as.numeric(student_date),
                     arr.ind = TRUE
  )
  
  # Getting 7 day dataset from student's start date
  df <- slice(
    courseData,
    start_row:(start_row + 167)
  )
  # inserting '-999' error into students data
  df[student_error, "O3"] <- -999
  
  df

}
