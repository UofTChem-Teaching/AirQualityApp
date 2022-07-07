# 2022-01-08 --- David Ross Hall --- davidross.hall@mail.utoronto.ca
# script to prepapare ECCC data for app. 
# largely based on code from JChemEd - AirQualityData repo

# 0. Libraries

library(tidyverse)


df <- read_csv(file = "raw-data/O3_2020.csv",
               skip = 7, 
               )


# 1. import ECCC files ----

# Reads hourly ECCC NAPS .csv file. 
# Skips premable, and cleans up column names (removing blingual part, P vs. Province, etc.)

readECCC <- function(file){
  
  # Reading ECCC hourly data and cleaning up headers
  df <- read_csv(file,
                 skip = 7, 
                 locale = readr::locale(encoding = "Latin1"), 
                 name_repair = "universal") %>%
    rename_with(~ gsub("\\..*", "", .x)) 
  
  # Datasets before 2019 record province as "P", check to make consistent to "Province" 
  if(colnames(df[4]) == "P"){
    
    df <- rename(df, Province = P)
    
    df
  }
  
  example_date <- df[1,7] # getting first date in column to assess format
  
  if(str_detect(example_date, "/")){
    
    df <- df %>%
      mutate(Date = anydate(Date))
    
  }
  
  # Append hour with Pollutant type
  chem <- df$Pollutant[1]
  df <-  rename_with(df, ~paste(., chem, sep = "_"), starts_with("H")) %>%
    select(-Pollutant)
  
  df
}

O3 <- readECCC("raw-data/O3_2019.csv")

# 2. Combine ECCC for NAPS ----

## Combining O3 and NO2 via inner_join, so pollutants from both stations
## Can work with any two hourly datasets, may expand in future any # hourly sets

joinECCC <- function(O3, NO2){
  
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

mergedCity <- function(df){
  
  df <- df %>%
    unite("City", City:Province, sep = ", ") %>%
    unite("NAPS", City:NAPS, sep = ", NAPS: ") 
  
  df
  
}

# 3. Merging ECCC with Population Data ------------
# Takes the NAPS population data, and inner merges it w/ combined NAPS lists. 
# Returns dataset with City

mapInfo <- function(joinedECCC, popDat){
  
  # removing pollutant columns 
  joinedECCC <- joinedECCC %>%
    select(-c(starts_with("H"), Date))
  
  # getting annotations for map icons
  df <- joinedECCC %>%
    inner_join(popDat) %>%
    distinct(NAPS, .keep_all = TRUE) %>%
    mutate(PopSize = case_when(Population >= 100000 ~ "large", 
                               Population < 100000 & Population >= 30000 ~ "medium",
                               Population < 30000 & Population >= 1000 ~ "small",
                               TRUE ~ as.character("rural")))
    
  # Simplifying city names for map labels & dropdown menu
  df <- df %>%
    mergedCity()

  df
  
}



# 4. Saving Datasets

## 4.1 Joinined ECCC datasets ----

joinedECCC <- joinECCC("raw-data/O3_2020.csv", 
                       "raw-data/NO2_2020.csv")

## 4.2 Importing and saving map data -----


pops <- read_csv("raw-data/NAPSPops.csv",
                 locale = readr::locale(encoding = "Latin1")) # Population info for every NAPS City

mapInfo <- mapInfo(joinedECCC = joinedECCC, 
                   popDat = pops)

write_csv(mapInfo, file = "www/ECCC2020_mapInfo.csv")

## 4.3 Pruning and saving ECCC data ----

joinedECCC %>%
  mergedCity() %>%
  select(-c(Latitude, Longitude)) %>%
  write_csv(file = "www/ECCC2020_wideCombined.csv")


# 5. Toronto Data for student assigned datasets  ----












