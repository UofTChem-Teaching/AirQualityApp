
# 1. Packages and date import ---------------------------------
library(shiny)
library(tidyverse)
library(ggExtra) # for marginal histogram
library(ggpmisc) # to display line of best fit on plot
library(leaflet) # interactive map
library(DT) # for data table
library(zoo) # rolling averages
library(plotly) # interactive plots 

data <- data.table::fread("www/ECCC2019_wideCombined.csv", encoding = 'UTF-8') 

mapInfo <- data.table::fread("www/ECCC2019_mapInfo.csv", encoding = 'UTF-8')

## 1.2 Custom icons for population size ================================= 

leafIcons <- icons(
    iconUrl = ifelse(mapInfo$PopSize == "large", "www/largeIcon.png",
                     ifelse(mapInfo$PopSize == "medium", "www/medIcon.png",
                            ifelse(mapInfo$PopSize == "small", "www/smallIcon.png", "www/ruralIcon.png")
                             )
    ),
    iconWidth = 26, iconHeight = 40, 
    iconAnchorX = 13, iconAnchorY = 40
    )

## 1.3 Descriptive text for map icon popups ================================= 

labs <- lapply(seq(nrow(mapInfo)), function(i) {
    paste0( '<p>',
            '<b>',mapInfo[i, "NAPS"], '</b><br>',
            "Population: ", mapInfo[i, "Population"], '<br>',
            "Size: ", mapInfo[i, "PopSize"], 
            '</p>') 
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
            
            dateRangeInput('dateRange', 
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
            
            tabsetPanel(type = "tabs",
                        tabPanel("Welcome", 
                                 includeHTML("www/welcome.html")),
                        tabPanel("Plot",
                                 plotlyOutput("TimeseriesPlot"),
                                 br(),
                                 p("You can interact with the time series plot above; i.e. narrowing displayed date range. Note however that the date range for both plots is dictated by your inputted dates. In other words, you’ll need to change your inputted dates to update the data displayed on the correlation plot. To save as an image, use the download button in the top-left (time-series) or right-click and save as (correlation)", style = "font-family: 'times'; font-si16pt"),
                                 plotOutput("CompPlot")),
                        tabPanel("Summary Stats", 
                                 br(),
                                 DT::dataTableOutput("SumTable")),
                        tabPanel("Notes", 
                                 includeHTML("www/notes.html"))
            )
        )
    )
)

# 3. Server -------------------------------------

server <- function(input, output) {
    
    
    # 3.1 Reactive data subsetting ================
    stationDat <- reactive({
        
        validate(
            need(input$dateRange[1] <= input$dateRange[2], 'Make sure dates are correct.')
        )
        
        data %>% filter(NAPS == input$NAPS) %>%
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
            filter(Date_time >= input$dateRange[1] & Date_time <= input$dateRange[2]) %>%
            mutate(Ox = NO2 + O3,
                   NO2_8hr = zoo::rollmean(NO2, k = 7, fill = NA, align = "right"),
                   O3_8hr = zoo::rollmean(O3, k = 7, fill = NA, align = "right"),
                   Ox_8hr = zoo::rollmean(Ox, k = 7, fill = NA, align = "right"))
        
    })
    
    # 3.2 Time series plot =================
    
    output$TimeseriesPlot <- renderPlotly({
        
            if(input$rollingAvg == "No, plot 1hr measurements"){
     
                fig <- plot_ly(stationDat(), x = ~Date_time)
                fig <- fig %>% add_lines(y = ~O3, name = "O3")
                fig <- fig %>% add_lines(y = ~NO2, name = "NO2")
                fig <- fig %>% add_lines(y = ~Ox, name = "Ox")
                fig <- fig %>% layout(
                    title = paste("1 hr readings at ", input$NAPS, '<br>' ),
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
                
            } else {
                
                # Plot w/ 8hr rolling avg. 
                fig <- plot_ly(stationDat(), x = ~Date_time)
                fig <- fig %>% add_lines(y = ~O3_8hr, name = "O3_8hr")
                fig <- fig %>% add_lines(y = ~NO2_8hr, name = "NO2_8hr")
                fig <- fig %>% add_lines(y = ~Ox_8hr, name = "Ox_8hr")
                fig <- fig %>% layout(
                    title = paste("Rolling 8hr mean readings at ", input$NAPS),
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
            
    })


    # 3.3 O3 vs. NO2 plot =======================
    
    output$CompPlot <- renderPlot({
        my.formula <- y ~ x
        
        if(input$excel == "No"){
            
            p <- ggplot(data = stationDat(), aes(x = NO2, y = O3)) + 
                geom_point() +
                geom_smooth(method='lm', formula= y~x, se=F) +
                stat_poly_eq(formula = y~x, 
                             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                             parse = TRUE,
                             label.x.npc = 1) +
                xlab("1hr Concentration NO2 (ppb)") +
                ylab("1hr Concentration O3 (ppb)") +
                ggtitle(paste("\nO3 vs NO2 at ",input$NAPS, " \nfrom ", input$dateRange[1], " to ", input$dateRange[2] ))
                
            p
            
        } else {
        
        p <- ggplot(data = stationDat(), aes(x = NO2, y = O3)) + 
            geom_jitter(alpha = .1) +
            geom_smooth(method='lm', formula= y~x, se=F) +
            stat_poly_eq(formula = y~x, 
                         aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                         parse = TRUE,
                         label.x.npc = 1) +
            xlab("1hr Concentration NO2 (ppb)") +
            ylab("1hr Concentration O3 (ppb)") +
            theme_classic() +
            ggtitle(paste("\nO3 vs NO2 at ",input$NAPS, " \nfrom ", input$dateRange[1], " to ", input$dateRange[2] ))
        ggMarginal(p, type = "density")
        
        }
    })
    
    # 3.4 Leaflet map ===================
    
    output$mymap <- renderLeaflet({
        leaflet(data = mapInfo) %>% addTiles() %>%
            addMarkers(~Longitude, 
                       ~Latitude, 
                       popup = lapply(labs, htmltools::HTML), 
                       label = ~paste(NAPS),
                       icon = leafIcons)
    })
    
    # 3.5 Table w/ summary stats ===============
    
    output$SumTable <- DT::renderDataTable({
        
        DT::datatable( stationDat() %>%
                            pivot_longer(cols = c("O3", "NO2", "Ox", "NO2_8hr", "O3_8hr", "Ox_8hr"),
                                         names_to = "Pollutant",
                                         values_to = "Concentration") %>%
                            group_by(Pollutant) %>%
                            summarise(mean = mean(Concentration, na.rm = TRUE), 
                                      sd = sd(Concentration, na.rm = TRUE),
                                      median = median(Concentration, na.rm = TRUE),
                                      min =  min(Concentration, na.rm = TRUE),
                                      max = max(Concentration, na.rm = TRUE)
                            ) %>%
                            mutate_if(is.numeric, round, digits = 2),
                       caption = 'Table 1: Summary statistics for O3, NO2, and Ox measurements from your selected NAPS station and time range. Note, all measurements are in ppb.' 
        )
    })
    
}

# 4. Run the application ----------------- 
shinyApp(ui = ui, server = server)
