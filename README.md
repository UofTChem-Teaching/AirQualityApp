# Air Quality App 

The Air Quality App is a web application developed as part of the *Air Quality Activity*, a new-lab component developed for the first-year general chemistry course *CHM135 Chemistry: Physical Principles* at the University of Toronto. In this activity, students use Microsoft Excel to analyze a unique subset of atmospheric O~3~ and NO~2~ measurements from the [National Air Pollution Surveillance (NAPS) program](https://www.canada.ca/en/environment-climate-change/services/air-pollution/monitoring-networks-data/national-air-pollution-program.html). Throughout the lab they develop their numeracy, graphicacy, and proficiency with Excel. Moreover, students are equipped with a foundational approach to data analysis they can leverage throughout their studies. The app facilitates this by generating and distributing datasets for students, in addition to exploring the entirety of the NAPS data to expand discussions.

Instructions for running the app, and customizing it, can be found below. 

### Rough App Workflow

The app is written to mirror the Air Quality Activity. As such, the typical workflow for the app would be:

1.  Navigate to app at [uoft-chem.shinyapps.io/Air_Quality_App/](https://uoft-chem.shinyapps.io/Air_Quality_App/); the **Welcome** splash page contains instructions for navigating the app.

2.  Go to **My Data** where students input their student number and receive a download prompt for their assigned dataset. This is students analyze offline in Excel.

    <details>

    <summary>Details on how student datasets are generated</summary>

    The datasets provided to students are produced as follows. Once students input their student number, a random sampling point is chosen from a random Greater Toronto Area (GTA) NAPS station. Then a subset of that dataset corresponding to 168 consecutive hours of measurements from that station and timepoint is produced and made available for download. Resolving issues around missing values, stored as '-999' in the NAPS data, is a critical component of the lab. Therefore, we artificially insert a random '-999' error into each dataset, ensuring every student encounters this problem. The inputted student numbers are stored alongside that number's associated random NAPS station, starting data, inserted random error point, and the time the dataset was first created. This permits several features of the air quality app. Firstly, it ensures that the same dataset will be recreated for a given student number input. Secondly, it ensures that instructors can verify that student submissions correspond to their assigned dataset. Thirdly, it provides the ability to reproduce any student's dataset should the need arise. To this end, we also included a password protected 'Admin' tab to the app, accessible only to TAs provided with the appropriate login credentials. Once unlocked, the 'Admin' tab allows markers to input any student's ID and receive an automated plotting of that student's assigned dataset for comparison to that student's submission. To preserve student privacy, inputted student IDs are one-way hashed via the [sodium package](https://cran.r-project.org/web/packages/sodium/index.html) before storage on a private, password protected, Google Sheets page accessible only by the course instructor.

    </details>

    <br>

3.  After offline analysis, discussion questions will prompt students to the **Explore NAPS** tab where they can quickly survey the entirety of the NAPS dataset using the available inputs on the side-bar.

Additional elements of the app:

4.  The **Summary Stats** page is not presently used in the CHM135 activity, but does provide simple summary statistics for the dataset selected from the sidebar inputs.

5.  The **Notes** and **Admin** tabs provide background info on the app, and rapid reproduction of expected students plot from their dataset, as discussed above.

## Running App

The Air Quality App can be readily customized and deployed for any course.

1.  **Clone the repo**. Apart from R package dependencies, all the content needed to run the Air Quality App is contained in this repository.

2.  **Install package dependencies**; the package dependencies are called in the \`app.R\` file. The primary dependencies are:

    <details>

    <summary>Packages explicilty called by the app</summary>

    -   [`tidyverse`](https://www.tidyverse.org/) which imports the `dplyr`, `ggplot`, `forcats`, `tibble`, `readr`, `stringr`, `tidyr`, and `purrr` packages.
    -   [`lubridate`](https://lubridate.tidyverse.org/)
    -   [`ggExtra`](https://cran.r-project.org/web/packages/ggExtra/vignettes/ggExtra.html)
    -   [`ggpmisc`](https://cran.r-project.org/web/packages/ggpmisc/)
    -   [`anytime`](https://cran.r-project.org/web/packages/anytime/)
    -   [`leaflet`](https://cran.r-project.org/web/packages/leaflet/)
    -   [`DT`](https://cran.r-project.org/web/packages/DT/)
    -   [`zoo`](https://cran.r-project.org/web/packages/zoo/)
    -   [`plotly`](https://cran.r-project.org/web/packages/plotly/)
    -   [`googlesheets4`](https://cran.r-project.org/web/packages/googlesheets4/)
    -   [`shinyauthr`](https://cran.r-project.org/web/packages/shinyauthr/)
    -   [`shinycssloaders`](https://cran.r-project.org/web/packages/shinycssloaders/)
    -   [`shiny`](https://cran.r-project.org/web/packages/shiny/)
    -   See `sessioninfo.txt` for complete session info.

    </details>

    <br>

3.  **Run app locally**; the app can be run locally by opening the `AirQualityApp.Rproj` in RStudio, then opening the `app.R` file, and clicking *Run App* in the top right corner. See [Helpful Links](#helpful-links) for resources on coding in R and Shiny.

    -   Note that the dataset generation functionally (the **My Data** tab) will not work and cause the app to hang (i.e. freeze), as the google sheet is not setup. See [Setting up Google Sheets](#setting-up-google-sheets) for details on resolving this.

4.  Once working, the app can be customized to meet your needs. See [Customizing App](#customizing-app) for instructions on updating NAPS dataset for the app; datasets for students assignments, and controlling the admin tab passwords.

5.  Deploy the app; this is straightforward via [shinyapps.io](https://www.shinyapps.io/); but see [Comments on shinyapps.io](#comments-on-shinyappsio)

    1.  When deploying the app make sure to include the `.secrets` folder created during the [Setting up Google Sheets](#setting-up-google-sheets){style="font-size: 11pt;"} instructions, in addition to the entire `www` folder with the data prepared for the app.

## Setting up Google Sheets 

The **My Data** tab will not work until the app is linked to Google Sheets. This is accomplished via the [`googlesheets4`](https://github.com/tidyverse/googlesheets4) package and the following:

1.  Create a google sheets document
    -   It should have the following headers: `student_number`, `naps_station`, `start_date`, and `error_row`; in that order.
    -   The sheet should be made private; see [Google Docs Help](https://support.google.com/docs/answer/1218656?hl=en&co=GENIE.Platform%3DDesktop)
2.  Follow the [instructions by Jonathan Trattner to connect google sheets with Shiny.](https://www.jdtrat.com/blog/connect-shiny-google/)
    1.  the `SHEET_ID` variable at the top of the `app.R` file should be your sheets unique URL.

## Customizing App

The principle customizing of the app will relate to the datasets displayed in the App and assigned from datasets. Since the app utilizes publicly available hourly measurements of O~3~ and NO~2~ from the NAPS.

### Updating NAPS data and student data

The basis of the app is the datasets of continuous hourly measurements of O~3~ and NO~2~ concentrations as measured by NAPS. The NAPS datasets consist of separate files for all hourly measurements across the entire network for a given pollutant for that year. As of Feb. 2023, the latest available dataset are the 2020 measurements; and are found [here](https://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/2020/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/?lang=en). Some work is needed to prepare the data for the app.

The verbatim files from the NAPS dataset, in addition to a compilation of census data (`NAPSPops.csv`) are stored in the `raw-data` folder. These datasets are combined to create all the datasets required for the app. To prepare data for the app, run the `app_data_prep.R` script, which contains instructions and code to combine NAPS hourly datasets and the aforementioned census data. The script will deposit the generated datasets in the `www` folder which contains the data needed for the app to run. A couple of notes:

-   You can specify which NAPS stations to include in the App via the `student_stations` variable in the `app_data_prep.R` script.
-   Ensure that you update the `app.R` file to reflect your newly created files.

### Controlling Admin Tab passwords

Create a local file called `user_base.r` that contains the following:

    # passwords for air quality app

    # create a user base then hash passwords with sodium 
    # then save to an rds file in app directory 
    library(sodium)
    library(tibble)
    library(purrr)

    user_base <- tibble::tibble(
      user = c("XXX", "YYY", "ZZZ"), 
      password = purrr::map_chr(c("xxx","yyy", "zzz"), 
                                sodium::password_store), 
      permissions = c("standard", "standard", "admin"), # aren't used for anything 
      name = c("User One", "User Two", "User Three")
    )

    saveRDS(user_base, "www/user_base.rds")

Change the `user`, `password`, and any other items to suite your needs. Run the scripts to create the `user_base.rds` file, which contains the encrypted passwords. These are the passwords for the Admin tabs. DO NOT push the `user_based.r` file to Github as this contains the un-encrypted passwords. See the [`shinyauthr` repo](https://github.com/PaulC91/shinyauthr) for additional details.

## Comments on shinyapps.io 

Running a Shiny app requires server side computing. In other words, as students select data to plot, a dedicated server must perform the necessary computations. While these requirements are not egregious, they are still to be considered if you plan on hosting your own instance of the app. While Shiny provides instructions and software for running the app on premise, we opted to host our app on the Shiny server cloud. Predictably, many students 'flashed' the app as the assignment deadline approached. Without adequate server space, this could increase load times, decrease responsiveness, and possibly crash the app. The free 15hrs/month of server time (time it takes to run the app) provided by Shiny is unsuitable for the anticipated server loads if the App is deployed for the typical fall CHM135 sessions which can have more than 2000 students.

We have used the *Standard* ShinyApps package to run the app for courses numbering more than 1600 students, and proper configuration. The *Basic* package is satisfactory for classes less then 250 students. Note that the app needs to be further configured via the ShinyApps dashboard (details [here](https://shiny.rstudio.com/articles/scaling-and-tuning.html)). Special attention should be payed to the number of possible concurrent connections.

## Helpful links

-   Therefore the easiest way run, modify, and deploy the app is from cloning this repo either from

-   [github directly](%5Bhttps://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)](<https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository>)) or, via

-   [RStudio](%5Bhttps://happygitwithr.com/rstudio-git-github.html).](<https://happygitwithr.com/rstudio-git-github.html>).)
