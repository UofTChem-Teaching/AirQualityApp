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