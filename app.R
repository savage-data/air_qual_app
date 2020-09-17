#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")

# Load database
air_quality <- readxl::read_xlsx("Master_Air_Data_1.xlsx")
air_quality$Date <- ymd_hms(air_quality$Date, tz = "UTC")

# Correct pm2.5
air_quality <- air_quality %>% 
    distinct(Sensor_loc, Date, PM2.5_O, Latitude, Longitude, PM2.5_U, 
             Temperature_F, Humidity_P, Pressure_hpa)

# A Q & U conversion factor for winter readings
air_quality_win <- air_quality %>% 
    filter(Date >= as.POSIXct("2019-11-01") & 
               Date <= as.POSIXct("2020-03-01"))

air_quality_win <- air_quality_win %>%  
    mutate(pm_o_corr = ((.778 * PM2.5_O) + 2.65))

air_quality_win <- air_quality_win %>%  
    mutate(pm_u_corr = ((.778 * PM2.5_U) + 2.65))

air_quality_prew <- air_quality %>% 
    filter(Date < as.POSIXct("2019-11-01"))

air_quality_prew$pm_o_corr <- air_quality_prew$PM2.5_O

air_quality_prew$pm_u_corr <- air_quality_prew$PM2.5_U

air_quality_postw <- air_quality %>% 
    filter(Date > as.POSIXct("2020-03-01"))

air_quality_postw$pm_o_corr <- air_quality_postw$PM2.5_O

air_quality_postw$pm_u_corr <- air_quality_postw$PM2.5_U

air_quality_all <- rbind(air_quality_prew, 
                         air_quality_win, 
                         air_quality_postw)

# remove unneccessary variables and data sets
air_quality_all <- air_quality_all %>% 
    dplyr::select(-PM2.5_O, -PM2.5_U)

rm(air_quality_postw)
rm(air_quality_win)
rm(air_quality_prew)
rm(air_quality)

air_quality_all <- air_quality_all %>% 
    arrange(Sensor_loc)

# Remove variables with 15% difference - based on EPA's standards
air_quality_all <-air_quality_all %>% 
    mutate(sensor_per_diff_o = ((pm_o_corr - pm_u_corr)/pm_u_corr)*100)

air_quality_all <-air_quality_all %>% 
    mutate(sensor_per_diff_u = ((pm_u_corr - pm_o_corr)/pm_o_corr)*100)

# filter data for sensors less than 5 or less than 
# 15% diff
air_quality_all <- air_quality_all %>% 
    filter(sensor_per_diff_u < 15 &
               sensor_per_diff_o < 15 | 
               (pm_o_corr < 5 | pm_u_corr < 5))

# Create new column
air_quality_all <- air_quality_all %>% 
    mutate(Recess = if_else(air_quality_all$pm_o_corr >= 35 | air_quality_all$pm_u_corr >= 35, 
                            "danger", 
                            "child"))

# Arrange by Date
air_quality_all <- air_quality_all %>%
    arrange(Date)

# Filter Date 
air_quality_all <- air_quality_all %>% 
    filter(Date > as.POSIXct("2019-09-09") & 
               Date < as.POSIXct("2020-04-01"))

# Recess Icons
recessIcons <-
    awesomeIconList(
        "child" = makeAwesomeIcon(icon = "child", library = "fa", 
                                  markerColor = "blue"),
        "danger" = makeAwesomeIcon(icon = "exclamation", library = "fa", 
                                   markerColor = "darkred")
    )

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
    
    titlePanel("Davis County Recess Tracker"),

    # Dropdown with date choices 
    selectInput("Date","Date and Time:", 
                choices = unique(air_quality_all$Date),
               selectize = T),
               #size = 25,169),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map")
        )
    )


# Define server logic required to draw map
server <- function(input, output) {
    
    # Filter data by user input
    filtered_df <- reactive({air_quality_all %>% 
            filter(Date == ymd_hms(input$Date))})
    
    # Render interactive map
    output$map <- renderLeaflet({
        # Map
        leaflet(filtered_df()) %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addAwesomeMarkers(~Longitude,
                              ~Latitude,
                              icon = ~recessIcons[Recess],
                              label = ~Sensor_loc,
                              labelOptions = labelOptions(noHide = F)) %>% 
            addLegend("topright",
                      title = "Good vs Bad Air Days",
                      colors = c("#990033", "#3399CC"),
                      labels = c("PM2.5 >= 35", "PM2.5 < 35"),
                      opacity = .8)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
