#install library this project use
list_packages <- c('ggplot2','shiny','dplyr','maps','leaflet','ggthemes','shinydashboard','scales','shinyWidgets','plotly',
                   'sf', 'readr', 'rnaturalearth', 'rnaturalearthdata','scales','shinyjs',"httr","jsonlite","png","grid","googleway","shinyWidgets")
new_packages <- list_packages[!list_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library("shinyWidgets")
library("httr")
library("jsonlite")
library("png")
library("grid")
library("shiny")
library("dplyr")
library("maps")
library("leaflet")
library("ggplot2")
library("shiny")
library("ggthemes")
library("shinydashboard")
library("scales")
library("shinyWidgets")
library("plotly")
library("sf")
library("readr")
library("rnaturalearth")
library("scales")
library('shinyjs')
library("googleway")
source('tableau-in-shiny-v1.0.R')

##################
# DATA   PROCESS #
##################
mode_patronage <- read.csv("dataset/patronage_by_transport_mode.csv", stringsAsFactors = FALSE) 
#only remain the data of weekday because our topic is commute to CBD
weekday_mode_patronage <- mode_patronage %>% filter(Day_type == "Normal Weekday") %>% group_by(Day_of_week) %>% summarise(Patronage = sum(Pax_daily))
weekday_mode_patronage$Day_of_week <- factor(weekday_mode_patronage$Day_of_week, 
                                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                        "Friday"),
                                             ordered = TRUE)

#read in a csv file about station location and patronage of each station  
station_data <- read.csv("dataset/station_information.csv", stringsAsFactors = FALSE) 

#get the most crowd station
crowd_station <- station_data %>%
  select(Stop_name, Pax_norm_weekday) %>%  # Select relevant columns
  arrange(desc(Pax_norm_weekday)) %>%      # Sort by pax_norm_weekday in descending order
  slice_head(n = 10) 

#############
# API Calls #
#############
#city name
city = "Melbourne,au"
#the key of weather api
api_key = "b3f5978cef2a9b60c80171ee675b9109"

#get the weather of Melbourne using API
get_weather_by_coordinates <- function(city,api_key) {
  base_url <- "https://api.openweathermap.org/data/2.5/weather"
  response <- GET(url = base_url,query = list(q = city,appid = api_key,units = "metric"))
  
  # check whether GET is successful
  if (http_status(response)$category != "Success") {
    warning("Failed to fetch data")
    return(NULL)
  }
  
  # Parsing a json response
  weather_data <- fromJSON(content(response, as = "text"))
  return(weather_data)
}


#get icon image url
display_weather_icon <- function(icon_code) {
  icon_url <- paste0("http://openweathermap.org/img/w/", icon_code, ".png")
  return(icon_url)
}

# get icon code 
icon_code <- get_weather_by_coordinates(city,api_key)$weather$icon[1]  
icon_url <- display_weather_icon(icon_code)

#check whether returned
print(get_weather_by_coordinates(city,api_key))
weather_information <- get_weather_by_coordinates(city,api_key)

#the key of Google map api
map_api = "AIzaSyCypkxh4Yz5DOGFBt-1w9PPJI6h7v5-lfg"



##################
# USER INTERFACE #
##################

#page of route map
route_map <- tabPanel(
  title = 'Route Overview',
  verticalLayout(
    #tableau map
    tableauPublicViz(
      id='tableauViz',       
      url='https://public.tableau.com/views/Book1_16966699247180/Sheet1?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link',
      width = "100%",
      height = "40%"
    ),
    #bar chart of route patronage
    plotlyOutput("route_patronage_bar_chart",width = "400px")
  )
)

#page of station
station_map <- tabPanel(
  title = "Map of Station",
  google_mapOutput(outputId = "stop_map"),
  plotlyOutput("station_bar_chart")
)

#combine two pages above
public_transport <- tabPanel(
  title='Public Transport',
  sidebarLayout(
    #side bar show information about weather and direction function
    sidebarPanel(
      span(h2("Today is")),
      br(),
      span(h2(format(Sys.Date(), "%d/%m/%Y"))),
      br(),
      #Weather
      span(h2("Real-time weather")),
      uiOutput("Weather_icon"),
      valueBoxOutput("Weather_Today"),
      br(),
      valueBoxOutput("temp_Today"),
      br(),
      valueBoxOutput("feel_temp_Today"),
      br(),
      valueBoxOutput("wind_speed"),
      #direction
      span(h2("Where Are You Going")),
      textInput("origin", "Enter starting point:", value = "Y Suites on A Beckket Street"),
      textInput("destination", "Enter destination:", value = "University of Melbourne"),
      actionButton("search_btn", "Search")
    ),
    mainPanel(
      tabsetPanel(
      route_map,
      station_map)
    )
  )
)

Taxi_uber <- tabPanel(
  title='Drive'
)

Bicycle<- tabPanel(
  title='Bicycle'
)
ui <- navbarPage(
  header=setUpTableauInShiny(),
  #use bootstrap
  theme = bslib::bs_theme(bootswatch = "quartz"),
  title='Commute to CBD',
  public_transport,
  Taxi_uber,
  Bicycle
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {

  #route patronage bar chart 
  output$route_patronage_bar_chart <- renderPlotly({
    #get changes of parameter in tableau map
    transport_mode <- input$tableauViz_parameter_changed$value[1]
    #print(transport_mode)
    if(is.null(transport_mode)){
      #the default selection of the transport mode is MetroBus
      weekday_mode_patronage <- mode_patronage %>% filter(Mode == "MetroBus" & Day_type == "Normal Weekday") %>% 
        group_by(Day_of_week) %>% summarise(Patronage = sum(Pax_daily))
      transport_mode<- "MetroBus"
    }
    else{
    #change the bar chart when the transport mode in tableau map is changed
    weekday_mode_patronage <- mode_patronage %>% filter(
      Mode ==  transport_mode & Day_type == "Normal Weekday") %>% 
      group_by(Day_of_week) %>% summarise(Patronage = sum(Pax_daily))}
    weekday_mode_patronage$Day_of_week <- factor(weekday_mode_patronage$Day_of_week, 
                                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                            "Friday"),
                                                 ordered = TRUE)
    #get the date type
    timestamp <- weather_information$dt
    date <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
    day_of_week <- weekdays(date)
    #day_of_week <- "Monday"
    #Change the color of today's corresponding bar chart
    bar_colors <- ifelse(weekday_mode_patronage$Day_of_week == day_of_week, 'grey','silver')
    plot_ly(data= weekday_mode_patronage,
            x=~Day_of_week,
            y=~Patronage,
            name="Patronage by day type",
            type='bar',
            marker = list(color = bar_colors)) %>%
      layout(title = list(text = paste("Daily Patronage of",transport_mode), font = list(color = "white")),
             xaxis = list(title = "type of day", showgrid = FALSE, tickfont = list(color = "white"), titlefont = list(color = "white")),
             yaxis = list(title = "",showticklabels = FALSE, showgrid = FALSE, tickfont = list(color = "white")),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(t = 50, b = 50),
             legend = list(font = list(color = "white")))
  })
  
  output$Weather_Today <- renderValueBox({
    valueBox(
      weather_information$weather$main[1], "Weather now"
    )
  })
  
  output$temp_Today <- renderValueBox({
    valueBox(
      paste(weather_information$main$temp, "°C"), "Temperature now"
    )
  })
  
  output$feel_temp_Today <- renderValueBox({
    valueBox(
      paste(weather_information$main$feels_like, "°C"), "Feels like"
    )
  })
  output$wind_speed <- renderValueBox({
    valueBox(
      paste(weather_information$wind$speed,"m/s"), "Wind speed now"
    )
  })
  
  output$Weather_icon <- renderUI({
    tags$img(src = icon_url)
  })
  
  output$stop_map <- renderGoogle_map({
    #use google map API draw the station map
    google_map(
      key = map_api,
      data = station_data,
      zoom = 15
    ) %>%
      add_markers(lat = "Stop_lat", lon = "Stop_long")
  })
  
  directions_data <- reactiveVal(NULL)
  
  observeEvent(input$search_btn, {
    directions <- google_map_directions(
      origin = input$origin,
      destination = input$destination,
      travel_mode = "transit"
    )
    
    directions_data(directions)
  })
  
  output$station_bar_chart <-renderPlotly({
    plot_ly(data = crowd_station, 
            x = ~Pax_norm_weekday, 
            y = ~Stop_name, 
            type = 'bar', 
            marker = list(color = 'silver'))%>%
      layout(title = "Daily Patronage",
             xaxis = list(title = "type of day",showgrid = FALSE),
             yaxis = list(title = NULL,showticklabels = FALSE,showgrid = FALSE),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(t = 50, b = 50))
  })
  
}



#############
# RUN SHINY #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))

