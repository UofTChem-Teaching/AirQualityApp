# 1. Setup -----

## 1.1. Libraries ----
library(tidyverse)
library(lubridate)
library(ggExtra) # for marginal histogram
library(ggpmisc) # to display line of best fit on plot
library(anytime) # easier then lurbidate for menu selectio...
library(leaflet) # interactive map
library(DT) # for data table
library(zoo) # rolling averages
library(plotly) # interactive plots
library(googlesheets4)
library(shinycssloaders)
library(shiny)

source("www/utils.R")

## 1.2 ECCC Starting Data ----

data <- data.table::fread("www/ECCC2020_wideCombined.csv", encoding = "UTF-8")

mapInfo <- data.table::fread("www/ECCC2020_mapInfo.csv", encoding = "UTF-8")

## 1.3 Data for Subsetting ----

## 1.4 Custom icons for population size =================================

leafIcons <- icons(
  iconUrl = ifelse(mapInfo$PopSize == "large", "www/largeIcon.png",
    ifelse(mapInfo$PopSize == "medium", "www/medIcon.png",
      ifelse(mapInfo$PopSize == "small", "www/smallIcon.png", "www/ruralIcon.png")
    )
  ),
  iconWidth = 26, iconHeight = 40,
  iconAnchorX = 13, iconAnchorY = 40
)

## 1.5 Descriptive text for map icon popups =================================

labs <- lapply(seq(nrow(mapInfo)), function(i) {
  paste0(
    "<p>",
    "<b>", mapInfo[i, "NAPS"], "</b><br>",
    "Population: ", mapInfo[i, "Population"], "<br>",
    "Size: ", mapInfo[i, "PopSize"],
    "</p>"
  )
})

# 2. UI  ---------------------------------

ui <- fluidPage(

  ## 2.1 App title =====================================
  titlePanel("CHM135: Exploring Air Quality Data"),

  ## 2.2 Sidebar Inputs ====================
  sidebarLayout(
    sidebarPanel(
      leafletOutput("mymap"),
      selectInput("NAPS",
        label = "Choose which station's data to display; use the map above to view location of available stations.",
        choices = unique(data$NAPS),
        selected = "60435"
      ),
      dateRangeInput("dateRange",
        label = "Select the date range you would like to plot.",
        start = anydate(min(data$Date)),
        min = anydate(min(data$Date)),
        end = anydate(max(data$Date)),
        max = anydate(max(data$Date))
      ),
      radioButtons(inputId = "rollingAvg", label = "Plot 8hr rolling average?", choices = c("Yes", "No, plot 1hr measurements")),
      radioButtons(inputId = "excel", label = "Improve my correlation plot?", choices = c("No", "Yes"))
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
          "Data",
          includeHTML("www/welcome.html")
        ),
        tabPanel(
          "Plot",
          plotlyOutput("TimeseriesPlot") %>% withSpinner(color = "#002A5C"),
          br(),
          p("You can interact with the time series plot above; i.e. narrowing displayed date range. Note however that the date range for both plots is dictated by your inputted dates. In other words, you’ll need to change your inputted dates to update the data displayed on the correlation plot. To save as an image, use the download button in the top-left (time-series) or right-click and save as (correlation)", style = "font-family: 'times'; font-si16pt"),
          plotOutput("CompPlot") %>% withSpinner(color = "#002A5C")
        ),
        tabPanel(
          "Summary Stats",
          br(),
          DT::dataTableOutput("SumTable")
        ),
        tabPanel(
          "Notes",
          includeHTML("www/notes.html")
        )
      )
    )
  )
)

# 3. Server -------------------------------------

server <- function(input, output) {


  # 3.1 Reactive data subsetting ================
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

  # 3.2 Time series plot =================
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

  # 3.3 O3 vs. NO2 plot =======================
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

  # 3.4 Leaflet map ===================
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

  # 3.5 Table w/ summary stats ===============
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
}

# 3.4 Leaflet map ===================

# 3

# 4. Run the application -----------------
shinyApp(ui = ui, server = server)
