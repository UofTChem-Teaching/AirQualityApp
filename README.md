# CHM135: Exploring Air Quality Data 

This is the repo for the University of Toronto's CHM 135 *Experiment 1*. It includes:

- code used to generate the shiny web application
- sources of data 
- Instructons and code for tidying data (useful if you want to change the year, current is 2019). 

Code and website written by David Hall and J. D'eon. 

## Generating Datasets

The app displays data from the O~3~ and NO~2~ hourly measurements as recorded in the [NAPS Datasets](https://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/?lang=en). The script `prepDataForAppV2.R` takes in these datasets, and the `NAPSPops.csv` file (copies in `raw-data` folder), and creates the two datasets used for the app. If you want to change the year of the datasets, simply change the inputted ECCC NAPS datasets.


# Regarding local installation
Error message 1: cannot resolve local host in Linux. 
One of the purrr, curl and proxy errors. Check if you have the xxxx port open. Fix to xxxx port. Local access is at http://127.0.0.1:xxxx 

Error message 2: packrat is not installed.
When deploying on Rstudio Connect, packrat is used to reproduce your local environment in the connect server. Search how to install packrat and its dependencies.

Error message 3: ggExtra cannot load, masked from package:base. 
This error is usually caused by the lubridate dependency. Check if packages - intersec,setdiff,union,lubridate,date - are updated to the newest version.