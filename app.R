
# 1. Packages and date import ---------------------------------
library(shiny)
library(tidyverse)
library(ggExtra) # for marginal histogram
library(ggpmisc) # to display line of best fit on plot
library(leaflet) # interactive map
library(DT) 
library(ggseas) # for rolling avg. 
library(anytime)
library(zoo)
library(plotly)

data <- data.table::fread("www/ECCC2018_wideCombined.csv")

mapInfo <- data %>%
    distinct(NAPS, .keep_all = TRUE) %>%
    select(c(NAPS, Latitude, Longitude, Population, PopSize)) 

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
# labs <- lapply(seq(nrow(mapInfo)), function(i) {
#     paste0(mapInfo[i, "City"], '<br>',
#            'NAPS', mapInfo[i, "NAPSID"], '<br>',
#            'Population: ', mapInfo[i, "Population"],'<br>',
#            'Size: ', mapInfo[i, "PopSize"])
# })

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
    titlePanel("Exploring CHM 135 Air Quality Data"),
    
    ## 2.2 Sidebar Inputs ==================== 
    sidebarLayout(
        sidebarPanel(
            leafletOutput("mymap"),
            
            selectInput("NAPS", 
                        label = "Choose which station's data to display",
                        choices = unique(data$NAPS),
                        selected = "10102"
                        ),
            
            dateRangeInput('dateRange', 
                           label = "date range input",
                           start = anydate(min(data$Date)),
                           min = anydate(min(data$Date)),
                           end = anydate(max(data$Date)),
                           max = anydate(max(data$Date))
                           ),
            radioButtons(inputId = "rollingAvg", label = "Plot 8hr rolling average?", choices = c("Yes (Basic plotly)", "No (Interactive plot)")),
            radioButtons(inputId = "excel", label = "Improve my correlation plot?", choices = c("No, I like Excel and abacuses", "Yes, it's 2020 ffs"))
        ),

       ## 2.3 Main Panel with outputs ========================
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Plot",
                                 plotlyOutput("TimeseriesPlot"),
                                 "Hey Jess, I was just playing around with the plotly package, which creates plots you can interact with (i.e. zoom into, etc.). The 8hr-avg plot is a standard static output from R, and the 1hr plot is the new interactive version. I can make everything this type of interactive if you'd prefer.",
                                 plotOutput("CompPlot"),
                                 "Hey Jess, I though playing with the optiosn would be revealing. The normal scatter plot (like you'd make in Excel), really hides the actual distribution of the data"
                        ),
                        tabPanel("Summary Stats", DT::dataTableOutput("SumTable")),
                        tabPanel("Notes", "Notes go here. data from Blah Blah Blah, UofT This Blah Blah Blah. Hi mom!")
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
                cols = O3_H01:NO2_H24,
                names_to = c("Pollutant", "Hour"),
                names_sep = "_",
                values_to = "Concentration"
            ) %>%
            pivot_wider(names_from = 'Pollutant',
                        values_from = 'Concentration') %>%
            mutate(Date_time = anytime(paste(Date, str_sub(Hour, -2, -1), ":00"))) %>%
            filter(Date_time >= input$dateRange[1] & Date_time <= input$dateRange[2]) %>%
            mutate(Ox = NO2 + O3,
                   NO2_8hr = zoo::rollmean(O3, k = 7, fill = NA, align = "right"),
                   O3_8hr = zoo::rollmean(NO2, k = 7, fill = NA, align = "right"),
                   Ox_8hr = zoo::rollmean(Ox, k = 7, fill = NA, align = "right"))
        
    })
    
    # 3.2 Time series plot =================
    
    output$TimeseriesPlot <- renderPlotly({
        
            if(input$rollingAvg == "No (Interactive plot)"){
     
                fig <- plot_ly(stationDat(), x = ~Date_time)
                fig <- fig %>% add_lines(y = ~O3, name = "O3")
                fig <- fig %>% add_lines(y = ~NO2, name = "NO2")
                fig <- fig %>% add_lines(y = ~Ox, name = "Ox")
                fig <- fig %>% layout(
                    title = paste("1 hr readings at ", input$NAPS),
                    xaxis = list(
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
                    
                    yaxis = list(title = "Concentration (ppb)"))
                
                fig
                
            } else {
                
                # Plot w/ 8hr rolling avg. 
                fig <- plot_ly(stationDat(), x = ~Date_time)
                fig <- fig %>% add_lines(y = ~O3_8hr, name = "O3_8hr")
                fig <- fig %>% add_lines(y = ~NO2_8hr, name = "NO2_8hr")
                fig <- fig %>% add_lines(y = ~Ox_8hr, name = "Ox_8hr")
                fig <- fig %>% layout(
                    title = paste("8 hr readings at ", input$NAPS),
                    xaxis = list(
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
                    
                    yaxis = list(title = "Concentration (ppb)"))
                
                fig
            }
            
    })


    # 3.3 O3 vs. NO2 plot =======================
    
    output$CompPlot <- renderPlot({
        my.formula <- y ~ x
        
        if(input$excel == "No, I like Excel and abacuses"){
            
            p <- ggplot(data = stationDat(), aes(x = NO2, y = O3)) + 
                geom_point() +
                geom_smooth(method='lm', formula= y~x, se=F) +
                stat_poly_eq(formula = y~x, 
                             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                             parse = TRUE,
                             label.x.npc = 1) +
                xlab("Concentration NO2 (ppb)") +
                ylab("Concentration O3 (ppb)") 
                
            p
            
        } else {
        
        p <- ggplot(data = stationDat(), aes(x = NO2, y = O3)) + 
            geom_jitter(alpha = .1) +
            geom_smooth(method='lm', formula= y~x, se=F) +
            stat_poly_eq(formula = y~x, 
                         aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                         parse = TRUE,
                         label.x.npc = 1) +
            xlab("Concentration NO2 (ppb)") +
            ylab("Concentration O3 (ppb)") +
            theme_classic()
        ggMarginal(p, type = "density")
        
        }
    })
    
    # 3.4 Leaflet map ===================
    
    output$mymap <- renderLeaflet({
        leaflet(data = mapInfo) %>% addTiles() %>%
            addMarkers(~Longitude, 
                       ~Latitude, 
                       popup = lapply(labs, htmltools::HTML), 
                       #popup = ~paste(NAPS),
                       label = ~paste(NAPS),
                       icon = leafIcons)
    })
    
    # 3.5 Table w/ summary stats ===============
    
    output$SumTable <- DT::renderDataTable({
        
        stationDat() %>%
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
            mutate_if(is.numeric, round, digits = 2)
            
         
    })
    
}

# 4. Run the application ----------------- 
shinyApp(ui = ui, server = server)
