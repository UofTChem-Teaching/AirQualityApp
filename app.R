# 1. Setup -----

## 1.1. Libraries ----
library(tidyverse)
library(lubridate)
library(ggExtra) # for marginal histogram
library(ggpmisc) # to display line of best fit on plot
library(anytime) # easier then lubridate for menu selection
library(leaflet) # interactive map
library(DT) # for data table
library(zoo) # rolling averages
library(plotly) # interactive plots
library(googlesheets4) # talking to google sheets 
library(shinyauthr)# for logins/password protection 
library(shiny)

source("www/utils.R")

## 1.2 ECCC Starting Data ----
# used for plotting and maps
data <- data.table::fread("www/ECCC2020_wideCombined.csv", encoding = "UTF-8")

mapInfo <- data.table::fread("www/ECCC2020_mapInfo.csv", encoding = "UTF-8")

## 1.3 Data for Subsetting ----
# what's broken up and handed out to students for the assignement
courseData <- data.table::fread("www/Toronto2020_courseData.csv")

naps_stations <- unique(courseData$NAPS)

## 1.4 Map Icons   =================================
 ### 1.4.1 Custom icons for population size =================================
  
  leafIcons <- icons(
    iconUrl = ifelse(mapInfo$PopSize == "large", "www/largeIcon.png",
      ifelse(mapInfo$PopSize == "medium", "www/medIcon.png",
        ifelse(mapInfo$PopSize == "small", "www/smallIcon.png", "www/ruralIcon.png")
      )
    ),
    iconWidth = 26, iconHeight = 40,
    iconAnchorX = 13, iconAnchorY = 40
  )
  
 ### 1.4.2 Descriptive text for map icon popups =================================
  
  labs <- lapply(seq(nrow(mapInfo)), function(i) {
    paste0(
      "<p>",
      "<b>", mapInfo[i, "NAPS"], "</b><br>",
      "Population: ", mapInfo[i, "Population"], "<br>",
      "Size: ", mapInfo[i, "PopSize"],
      "</p>"
    )
  })


## 1.6 Declarations and functions for google sheets ----

SHEET_ID <- "https://docs.google.com/spreadsheets/d/1spwFA7AlDyhzTjtClexC7YmxDFWT6MvvdCABXnXmsUY/edit?usp=sharing"

  # for authenticating sheets access on server 
  options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = TRUE,
    # specify auth tokens should be stored in a hidden directory ".secrets", don't push to github....
    gargle_oauth_cache = ".secrets"
  )


  
  
## 1.7 usernames & passwords ----
  
  # user_base <- tibble::tibble(
  #   user = c("user1", "user2"),
  #   password = c("pass1", "pass2"),
  #   permissions = c("admin", "standard"),
  #   name = c("User One", "User Two")
  # )
  
  user_base <- readRDS("user_base.rds")
  
# 2. UI  ---------------------------------

ui <- fluidPage(

  ## 2.1 App title =====================================
  titlePanel("Exploring Air Quality Data"),

  ## 2.2 Sidebar Inputs ====================
  sidebarLayout(
    sidebarPanel(
      leafletOutput("mymap"),
      selectInput("NAPS",
        label = "Choose which station's data to display; use the map above to view location of available stations (Click map icons to see population size).",
        choices = unique(data$NAPS),
        selected = "Sudbury, ON, NAPS: 60610"
      ),
      dateRangeInput("dateRange",
        label = "Select the date range you would like to plot.",
        start = anydate(min(data$Date)),
        min = anydate(min(data$Date)),
        end = anydate(max(data$Date)),
        max = anydate(max(data$Date))
      ),
      radioButtons(inputId = "rollingAvg", 
                   label = "Plot 8hr rolling average?", 
                   choices = c( "No, plot 1hr measurements","Yes")),
      radioButtons(inputId = "excel", 
                   label = "Improve my correlation plot?", 
                   choices = c("No", "Yes"))
    ),

  ## 2.3 Main Panel with outputs ========================
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Welcome",
          includeHTML("www/welcome.html")
        ),
        tabPanel(
          "My Data",
          fluidRow(
            column(
              3,
              textInput("studentNum", "Enter your student number below"),
              actionButton("showStudNum", "Get my Data!"),
              br(),
              br(),
              p("Your student number is shown on your UofT student card; it should be approx 10 numbers in length.", style = "font-family: 'times'; font-si16pt")
            ),
            column(
              6,
              align="center",
              br(),
              uiOutput("downloadButton"),
              br(),
              # downloadButton("download", "Download Your Data!"),
              DT::dataTableOutput("df_table") %>% 
                withSpinner(color = "#002A5C")
            ),
            column(3)
          )
        ),
        tabPanel(
          "Explore NAPS",
          plotlyOutput("TimeseriesPlot") %>% 
            withSpinner(color = "#002A5C"),
          br(),
          p("You can interact with the time series plot above; i.e. narrowing displayed date range. Note however that the date range for both plots is dictated by your inputted dates. In other words, you’ll need to change your inputted dates to update the data displayed on the correlation plot. To save as an image, use the download button in the top-left (time-series) or right-click and save as (correlation)", 
            style = "font-family: 'times'; font-si16pt"),
          plotOutput("CompPlot") %>% 
            withSpinner(color = "#002A5C"),
          p("Both the normal and improved correlation plot display the exact same data. However, the improved version implements a couple of tweaks to improve readability. Firstly, by applying a 'jitter' and increase the transparency of an individual point we can see overlapping points. As well, we've added marginal histograms that show the distribution of the O3 and NO2 data. Lastly, removing the grey background improves readability.", 
            style = "font-family: 'times'; font-si16pt")
        ),
        tabPanel(
          "Summary Stats",
          br(),
          DT::dataTableOutput("SumTable")
        ),
        tabPanel(
          "Notes",
          includeHTML("www/notes.html")
        ),
        tabPanel(
          "Admin",
          # add logout button UI
          div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
          # add login panel UI function
          shinyauthr::loginUI(id = "login"),
          # setup table output to show user info after login
          #tableOutput("user_table")
          uiOutput("studentNum2")
        )
      )
    )
  )
)

# 3. Server -------------------------------------

server <- function(input, output, session) {

  ## 3.1 Reactive data subsetting ================
  stationDat <- reactive({
    validate(
      need(input$dateRange[1] <= input$dateRange[2], "Make sure dates are correct.")
    )

    data <- stationDataPrep(
      data = data,
      naps = input$NAPS,
      start_date = input$dateRange[1],
      end_date = input$dateRange[2]
    )
    data
  })

  ## 3.2 Time series plot =================
  output$TimeseriesPlot <- renderPlotly({
    if (input$rollingAvg == "No, plot 1hr measurements") {

      # # Plot w/ 8hr rolling avg.
      fig <- timeSeriesPlot(data = stationDat()) %>%
        layout(
          title = paste("1 hr readings at ", input$NAPS, "<br>")
        ) %>%
        add_lines(y = ~O3_8hr, name = "O3") %>%
        add_lines(y = ~NO2_8hr, name = "NO2") %>%
        add_lines(y = ~Ox_8hr, name = "Ox")
      fig
    } else {

      # # Plot w/ 8hr rolling avg.
      fig <- timeSeriesPlot(data = stationDat()) %>%
        layout(
          title = paste("Rolling 8hr mean readings at ", input$NAPS, "<br>")
        ) %>%
        add_lines(y = ~O3_8hr, name = "O3_8hr") %>%
        add_lines(y = ~NO2_8hr, name = "NO2_8hr") %>%
        add_lines(y = ~Ox_8hr, name = "Ox_8hr")
      fig
    }
  })

  ## 3.3 O3 vs. NO2 plot =======================
  output$CompPlot <- renderPlot({
    my.formula <- y ~ x

    p <- scatterPlot(data = stationDat()) +
      ggtitle(paste("\nO3 vs NO2 at ", input$NAPS, " \nfrom ", input$dateRange[1], " to ", input$dateRange[2]))

    if (input$excel == "No") {
      p +
        geom_point()
    } else {
      p <- p +
        geom_jitter(alpha = .1) +
        theme_classic()

      ggMarginal(p, type = "density")
    }
  })

  ## 3.4 Leaflet map ===================
  output$mymap <- renderLeaflet({
    leaflet(data = mapInfo) %>%
      addTiles() %>%
      addMarkers(~Longitude,
        ~Latitude,
        popup = lapply(labs, htmltools::HTML),
        label = ~ paste(NAPS),
        icon = leafIcons
      ) 
    
  })

  ## 3.5 Table w/ summary stats ===============
  output$SumTable <- DT::renderDataTable({
    DT::datatable(stationDat() %>%
      pivot_longer(
        cols = c("O3", "NO2", "Ox", "NO2_8hr", "O3_8hr", "Ox_8hr"),
        names_to = "Pollutant",
        values_to = "Concentration"
      ) %>%
      group_by(Pollutant) %>%
      summarise(
        mean = mean(Concentration, na.rm = TRUE),
        sd = sd(Concentration, na.rm = TRUE),
        median = median(Concentration, na.rm = TRUE),
        min = min(Concentration, na.rm = TRUE),
        max = max(Concentration, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, round, digits = 2),
    caption = "Table 1: Summary statistics for O3, NO2, and Ox measurements from your selected NAPS station and time range. Note, all measurements are in ppb."
    )
  })
  ## 3.6 Data assigner -----
  
  # checking if students inputted a valid student ID.
  checkID <- eventReactive(input$showStudNum, {
    # check if any characters other than digits 
    # UofT student ID contain only 10 digits 
    checkUofTID(input$studentNum)
  })
  
  # Loading google sheet w/ recorded student values
  course_data <- eventReactive(input$showStudNum, {
    
    validate(need(checkID(), "Please input your UofT Student number on the left."))
    
    df <- loadData(sheet_id = SHEET_ID) %>%
      mutate(start_date = round_date(start_date, unit = "hour"))
    df
  })
  
  # Getting student values to retrieve their assigned datasets
  # Checks if student was already assigned data set if not, assignes them one
  # outputs a single row data.frame matching GoogleSheets
   student_vals <- eventReactive(input$showStudNum, {

     assigne_vals(df = course_data(), 
                  id = input$studentNum,
                  courseData = courseData, 
                  naps_stations = naps_stations,
                  sheet_id = SHEET_ID)
    
  })
  
  # retrieving student assigned datasets
  student_data <- reactive({
    student_vals <- student_vals()
    
    student_naps <- student_vals[1, "naps_station"]
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
  })
  
  output$df_table <- renderDT({
    student_data <- student_data() %>%
      mutate(Time = dateToExcel(Time))
    student_data
  })
  
  output$download <- downloadHandler(
    filename = "my_7day_data.csv",
    content = function(file) {
      write.csv(student_data(), file, row.names = FALSE)
    }
  )
  
  # renders download button once dataset as has been retrieved
  output$downloadButton <- renderUI({
    if (!is.null(student_data())) {
      downloadButton("download", "Download Your Data!")
    }
  })
  
  # modal dialog to download data
  observeEvent(student_data(), {
    showModal(modalDialog(
      title = "Download",
      "Download your data using the download button.",
      easyClose = TRUE
    ))
  })

  
  ## 3.7 Admin Tab -----
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    df <- loadData(sheet_id = SHEET_ID) %>%
      mutate(start_date = round_date(start_date, unit = "hour"))
    df
  })
  
  output$studentNum2 <- renderUI({
    req(credentials()$user_auth)
    textInput("studentNum2", "Input Student Number")
  })
  
}

# 4. Run the application -----------------
shinyApp(ui = ui, server = server)
