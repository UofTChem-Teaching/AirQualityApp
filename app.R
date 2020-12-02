
# 1. Packages and date import ---------------------------------
library(shiny)
library(tidyverse)
library(ggExtra) # for marginal histogram
library(ggpmisc) # to display line of best fit on plot
library(leaflet) # interactive map
library(DT) 
library(ggseas) # for rolling avg. 
library(anytime)

data <- data.table::fread("www/ECCC2018_wideCombined.csv")

mapInfo <- data %>%
    distinct(NAPS, .keep_all = TRUE) %>%
    select(c(NAPS, City, P, Latitude, Longitude))


# 2. UI  ---------------------------------

ui <- fluidPage(

    # 2.1 App title =====================================
    titlePanel("Exploring CHM 135 Air Quality Data"),
    
    # 2.2 Sidebar Inputs ==================== 
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
            radioButtons(inputId = "rollingAvg", label = "Plot 8hr rolling average?", choices = c("Yes", "No")),
            radioButtons(inputId = "excel", label = "Improve my correlation plot?", choices = c("No, I like Excel and abacuses", "Yes, it's 2020 ffs"))
        ),

        # 2.3 Main Panel with outputs ========================
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Plot",
                                 plotOutput("TimeseriesPlot"),
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
            need(input$dateRange[1] <= input$dateRange[2], 'Make sure dates are correct. ')
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
            mutate(Ox = NO2 + O3) %>%
            mutate(Date_time = anytime(paste(Date, str_sub(Hour, -2, -1), ":00"))) %>%
            filter(Date_time >= input$dateRange[1] & Date_time <= input$dateRange[2])

    })
    
    # 3.2 Time series plot =================
    
    output$TimeseriesPlot <- renderPlot({
        
            if(input$rollingAvg == "No"){
                
                # Plot w/ 1hr readings.
                ggplot(data = stationDat(), 
                       aes(x = as.POSIXct(Date_time))) +
                    geom_line(aes(y = O3, colour = "O3")) +
                    geom_line(aes(y = NO2, colour = "NO2")) +
                    geom_line(aes(y = Ox, colour = "Ox")) +
                    scale_x_datetime() +
                    xlab("Date") +
                    ylab("Concentration (ppb)") + 
                    labs(colour = "Pollutant") +
                    theme_classic() +
                    ggtitle(paste("NAPS station ", input$NAPS), 
                            subtitle = "Time series with 1 hr readings") 
                
                
            } else {
                
                # Plot w/ 8hr rolling avg. 
                ggplot(data = stationDat(), 
                       aes(x = as.POSIXct(Date_time))) +
                    stat_rollapplyr(aes(y = O3, colour = "O3"), 
                                    width = 8, align = "right") +
                    stat_rollapplyr(aes(y = NO2, colour = "NO2"), 
                                    width = 8, align = "right") +
                    stat_rollapplyr(aes(y = Ox, colour = "Ox"), 
                                    width = 8, align = "right") +
                    scale_x_datetime() +
                    xlab("Date") +
                    ylab("Concentration (ppb)") +
                    labs(colour = "Pollutant") +
                    theme_classic() +
                    ggtitle(paste("NAPS station ", input$NAPS), 
                            subtitle = "Time series with rolling 8 hr average") 
                
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
            addMarkers(~Longitude, ~Latitude, popup = ~paste(City,", NAPS: ", NAPS), label = ~paste(City, ", NAPS: ", NAPS))
    })
    
    # 3.5 Table w/ summary stats ===============
    
    output$SumTable <- DT::renderDataTable({
        
        stationDat() %>%
            pivot_longer(cols = c("O3", "NO2", "Ox"),
                         names_to = "Pollutant",
                         values_to = "Concentration") %>%
            group_by(Pollutant) %>%
            summarise(mean = mean(Concentration, na.rm = TRUE), 
                      sd = sd(Concentration, na.rm = TRUE),
                      median = median(Concentration, na.rm = TRUE),
                      min_1hr =  min(Concentration, na.rm = TRUE),
                      max_1hr = max(Concentration, na.rm = TRUE)
            ) %>%
            mutate_if(is.numeric, round, digits = 2)
         
    })
    
}

# 4. Run the application ----------------- 
shinyApp(ui = ui, server = server)
