# 2023-01-17 -- David Ross Hall --- davidross.hall@mail.utoronto.ca
# functions to prepare continuous hourly NAPS data for app

# 0. Libraries

library(tidyverse)
library(anytime)

# 1. import ECCC files ----

# Reads hourly ECCC NAPS .csv file.
# Skips preamable, and cleans up column names (removing bilingual part, P vs. Province, etc.)

readECCC <- function(file) {

  # Reading ECCC hourly data and cleaning up headers
  df <- read_csv(file,
    skip = 7,
    locale = readr::locale(encoding = "Latin1"),
    name_repair = "universal"
  ) %>%
    rename_with(~ gsub("\\..*", "", .x))

  # Datasets before 2019 record province as "P", check to make consistent to "Province"
  if (colnames(df[4]) == "P") {
    df <- rename(df, Province = P)

    df
  }

  example_date <- df[1, 7] # getting first date in column to assess format

  if (str_detect(example_date, "/")) {
    df <- df %>%
      mutate(Date = anydate(Date))
  }

  # Append hour with Pollutant type
  chem <- df$Pollutant[1]
  df <- rename_with(df, ~ paste(., chem, sep = "_"), starts_with("H")) %>%
    select(-Pollutant)

  df
}


# 2. Combine ECCC for NAPS ----

## Combining O3 and NO2 via inner_join, so pollutants from both stations
## Can work with any two hourly datasets, may expand in future any # hourly sets

joinECCC <- function(O3, NO2) {

  # Cleaning eccc files
  O3 <- readECCC(O3)
  NO2 <- readECCC(NO2)

  # Subsetted joined dataset
  df <- O3 %>%
    inner_join(NO2) %>%
    na_if(-999)

  df
}

# 3. Merged City Column ----

# The app dropdown menu & map icons derive data from different files
# this function ensure they're the same format/content

mergedCity <- function(df) {
  df <- df %>%
    unite("City", City:Province, sep = ", ") %>%
    unite("NAPS", City:NAPS, sep = ", NAPS: ")

  df
}

# 3. Merging ECCC with Population Data ------------
# Takes the NAPS population data, and inner merges it w/ combined NAPS lists.
# Returns dataset with City

mapInfo <- function(joinedECCC, popDat) {

  # removing pollutant columns
  joinedECCC <- joinedECCC %>%
    select(-c(starts_with("H"), Date))

  # getting annotations for map icons
  df <- joinedECCC %>%
    inner_join(popDat) %>%
    distinct(NAPS, .keep_all = TRUE) %>%
    mutate(PopSize = case_when(
      Population >= 100000 ~ "large",
      Population < 100000 & Population >= 30000 ~ "medium",
      Population < 30000 & Population >= 1000 ~ "small",
      TRUE ~ as.character("rural")
    ))

  # Simplifying city names for map labels & dropdown menu
  df <- df %>%
    mergedCity()

  df
}

# Convert date-time to excel serial
# Holdover from previous version.

dateToExcel <- function(date) {
  d0 <- as_datetime(0, origin = "1899-12-30 00:00:00", tz = "UTC")
  d1 <- date

  d <- as.numeric(d1 - d0)
  d
}
