### 20210106 --- David Ross Hall --- davidross.hall@mail.utoronto.ca
## Script to prepare ECCC data for App. 

# 1. Packages & Dependecies -------------------

library(tidyverse)

# 2. Functions for cleaning data ------------

## 2.1 Removing french half of column names (i.e. after \\) ------------
ECCCcolRename <- function(df, chem){
  
  df <- df %>% rename_all(funs(gsub("\\..*","", make.names(names(df))))) %>%
    rename_at(vars(starts_with("H")), ~paste0(chem,"_", .))
  
}

## 2.2 Combine O3 and NO2 datasets 
# Only worrying about O3 and NO2 for now
combineO3andNO2 <- function(O3, NO2, nrows = Inf){
  
  O3dat <-  read.csv(O3, skip = 7, nrows = nrows) %>%
    ECCCcolRename(., "O3") %>%
    select(-c("Pollutant"))
  
  NO2dat <- read.csv(NO2, skip = 7, nrows = nrows) %>%
    ECCCcolRename(., "NO2") %>%
    select(-c("Latitude", "Longitude", starts_with("P"), "City", "Pollutant")) # use starts_with("P") b/c 2018 data uses "P/T", and 2019 uses "Province/Terrirory" 
  
  df <- O3dat %>%
    inner_join(NO2dat, by = c("NAPS", "Date")) %>%
    na_if(-999)
  
}

# 3. Importing ECCC data and merging ----------------

O3_path = "raw-data/O3_2019.csv"
NO2_path = "raw-data/NO2_2019.csv"


datWide <- combineO3andNO2(O3 = "raw-data/O3_2019.csv", NO2 = "raw-data/NO2_2019.csv")

# 4. Merging ECCC with Population Data ------------

pops <- read.csv("raw-data/NAPSPops.csv") # Population info for every NAPS City

# Adding population to wide data
mapInfo <- datWide %>% 
  inner_join(pops, by = "City") %>%
  distinct(NAPS, .keep_all = TRUE) %>%
  select(c(City, starts_with("P"), NAPS, Latitude, Longitude, Population))  %>%
  mutate(PopSize = if_else(Population >= 100000, "large",
                           if_else(Population < 100000 & Population >= 30000, "medium",
                                   if_else(Population < 30000 & Population >= 1000, "small","rural")))) %>%
  relocate(Population, .before = Latitude) %>%
  relocate(PopSize, .before = Latitude) 

# Simplifying city names for map labels & dropdown menu
mapInfo <- mapInfo %>%
  unite("City", City:P, sep = ", ") %>%
  unite("NAPS", City:NAPS, sep = ", NAPS: ") 

#write.csv(df, file = "www/ECCC2018_wideCombined.csv")
