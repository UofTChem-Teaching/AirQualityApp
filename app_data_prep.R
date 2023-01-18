# 2023-01-17 --- David Ross Hall --- davidross.hall@mail.utoronto.ca
# script to prepapare ECCC data for app.

# Run this entire script to generate the national NAPS
# dataset for the Air Quality App (what's displayed)
# and to generate the Toronto Subset that's passed along to students.


# 0. Source & libraries ----

library(tidyverse)
library(anytime)
source("data_prep_utils.r")

# 1. Declarations of variables/filepaths ----

## 1.1 Location of O3/NO2 and population data ----
O3_data <- "raw-data/O3_2020.csv"
NO2_data <- "raw-data/NO2_2020.csv"
population_data <- "raw-data/NAPSPops.csv"

## 1.2 Enumerating desired NAPS stations for student datasets ----
# the ones listed here are for Toronto.
student_stations <- c("60410", "60430", "60435", "60438", "60440")

## 1.3 Location of outputs; "www" folder is uploaded to the app  ----
map_info_save <- "www/ECCC2020_mapInfo.csv"
wide_Combined <- "www/ECCC2020_wideCombined.csv"
student_data <- "www/Toronto2020_studentData.csv"

# 2. Saving ECCC Data for App ----
## Merging the two NAPS datasets for the App;
## This is the dataset displayed on the app.

## 2.1 Joinined ECCC datasets ----

joinedECCC <- joinECCC(
  O3_data,
  NO2_data
)

## 2.2 Importing and saving map data -----


pops <- read_csv(population_data,
  locale = readr::locale(encoding = "Latin1")
) # Population info for every NAPS City

mapInfo <- mapInfo(
  joinedECCC = joinedECCC,
  popDat = pops
)

write_csv(mapInfo, file = map_info_save)

## 2.3 Pruning and saving ECCC data ----

joinedECCC %>%
  mergedCity() %>%
  select(-c(Latitude, Longitude)) %>%
  write_csv(file = wide_Combined)

# 3. Toronto Data for student assigned datasets  ----
# Ensure that you've run Part 1 before running part 2
# Enumerating desired NAPS stations; the ones listed here are for Toronto.

# student_Naps <- paste(student_stations, collapse ="|")

# Tidying wide Toronto NAPS data
data <- read_csv(wide_Combined) %>%
  mutate(NAPS = str_replace(NAPS, ".*:", "")) %>%
  filter(str_detect(NAPS, paste(student_stations, collapse = "|"))) %>%
  pivot_longer(
    cols = starts_with("H"),
    names_to = c("Hour", "Pollutant"),
    names_sep = "_",
    names_prefix = "H",
    values_to = "Concentration"
  ) %>%
  pivot_wider(
    names_from = "Pollutant",
    values_from = "Concentration"
  )

# Converting time format to match Excels.
data2 <- data %>%
  filter(Date < lubridate::ymd("2020-03-01")) %>%
  mutate(Time = paste0(Date, " ", Hour, ":00")) %>%
  mutate(Time = lubridate::parse_date_time(Time, "%Y-%m-%d %H:%M") - lubridate::hours(1)) %>%
  # relocate(Time, .after = Longitude) %>%
  select(-c(Date, Hour)) %>%
  relocate("Time", .after = "NAPS") %>%
  replace_na(list(
    O3 = -999,
    NO2 = -999
  ))

write_csv(x = data2, file = student_data)
