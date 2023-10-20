#install library this project use
list_packages <- c(
  'ggplot2', 'shiny', 'dplyr', 'maps', 'leaflet', 'ggthemes', 'shinydashboard', 
  'scales', 'shinyWidgets', 'plotly', 'sf', 'readr', 'shinyjs', "httr", "jsonlite", "png", 
  "grid", "googleway", 'ggiraph', 'tidyr'
)
new_packages <- list_packages[!list_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

lapply(list_packages, library, character.only = TRUE)
source('tableau-in-shiny-v1.0.R')

##################
# DATA   PROCESS #
##################
mode_patronage <- read.csv("dataset/patronage_by_transport_mode.csv", stringsAsFactors = FALSE) 
bike_data <- read.csv("dataset/bicycle.csv")
#only remain the data of weekday because our topic is commute to CBD
weekday_mode_patronage <- mode_patronage %>% filter(Day_type == "Normal Weekday") %>% group_by(Day_of_week) %>% summarise(Patronage = sum(Pax_daily))
weekday_mode_patronage$Day_of_week <- factor(weekday_mode_patronage$Day_of_week, 
                                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                        "Friday"),
                                             ordered = TRUE)

#read in a csv file about station location and patronage of each station  
station_data <- read.csv("dataset/station_information.csv", stringsAsFactors = FALSE)  %>%
                arrange(desc(Pax_norm_weekday))

#get the most crowd station
crowd_station <- station_data %>%      # Sort by pax_norm_weekday in descending order
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

weather_information <- get_weather_by_coordinates(city,api_key)

#the key of Google map api
map_api = "AIzaSyCypkxh4Yz5DOGFBt-1w9PPJI6h7v5-lfg"


res <- google_places(location = c(-38.37423454,145.2218375),
              place_type = "restaurants",
              radius = 200,
              key = map_api)$results
print(names(res))
res <- res %>% select(types,name)
print(res)
##################
# USER INTERFACE #
##################

#page of route map
route_map <- tabPanel(
  title = 'Route Overview',
  verticalLayout(
    #tableau map
    tableauPublicViz(
      id='tableau_route_map',       
      url='https://public.tableau.com/views/Book1_16966699247180/Sheet1?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link',
      width = "100%",
      height = "40%"
    ),
    #bar chart of route patronage
    splitLayout(
    plotlyOutput("route_patronage_bar_chart",width = "400px"),
    div(
      h2("Check Timetable Here"),
      a(href = "https://www.ptv.vic.gov.au/timetables",
        target = "_blank",
        img(src = "logo-simple.svg", width = "20%")),
      h2("Top Up Your Myki"),
      a(href = "https://www.ptv.vic.gov.au/tickets/myki/?misdirected=1#topup",
        target = "_blank",
        img(src = "myki.png", width = "20%")),
    ),
      cellWidths = c("50%", "50%") 
    )
    )
  )

#page of station
station_map <- tabPanel(
  title = "Map of Station",
  #select a staion
  div(
    selectInput(
      "stop_select","Stop Selection:",
      choices = unique(station_data$Stop_name)
    ),
  verticalLayout(
    #station map
  tableauPublicViz(
    id='tableau_station_map',       
    url='https://public.tableau.com/views/Book2_16974540932260/Sheet1?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link',
    width = "100%",
    height = "40%"
  ),
  splitLayout(
    div(
    h2("About This Station"),
    #show patronage
    valueBoxOutput("station_patronage"),
    h3("Search Nearby"),
    #search locations nearby station
    textInput("location_type", "Enter location:", value = "Restaurants"),
    actionButton("location_search_btn", "Search")),
    tableOutput("location_near_station"),
    cellWidths = c("30%","70%"))
  ),
  cellWidths = c("50%","50%"))
)

#page of car traffic volumn
volumn_tab <-tabPanel("Traffic Volumn", 
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput(inputId = "dayOfWeek", 
                                        label = "Day Of Week", 
                                        min = 1, max = 7, value = c(1, 7), step = 1),
                            
                            checkboxGroupInput(inputId = "bound", 
                                               label = "Bound", 
                                               choices = c("EAST BOUND", "NORTH BOUND", 
                                                           "NORTH EAST BOUND", "NORTH WEST BOUND", 
                                                           "SOUTH BOUND", "SOUTH EAST BOUND", 
                                                           "SOUTH WEST BOUND", "WEST BOUND"),
                                               selected = c("EAST BOUND", "NORTH BOUND", 
                                                            "NORTH EAST BOUND", "NORTH WEST BOUND", 
                                                            "SOUTH BOUND", "SOUTH EAST BOUND", 
                                                            "SOUTH WEST BOUND", "WEST BOUND"))
                          ),
                          mainPanel(
                            tableauPublicViz(     
                              id='volumn_tableau',       
                              url = 'https://public.tableau.com/views/volumn_draft3/map1?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link',
                              height="1000px"
                            ),
                          )
                        )
                      )
)
#page of car parking area
parking_tab <- tabPanel("Parking Area", 
                        fluidPage(
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(
                                inputId = "parkingType", 
                                label = "Parking type", 
                                choices = c("Commercial", "Private", "Residential"),
                                selected = c("Commercial", "Private", "Residential")
                              )
                            ),
                            mainPanel(
                              tableauPublicViz(
                                id='parking_tableau',
                                url = 'https://public.tableau.com/views/ParkingAreainMelbourne0/1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
                                height="1000px",
                                width="1000px"
                              )
                            )
                          )
                        )
)

#combine all page above
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
      actionButton("direction_search_btn", "Search")
    ),
    mainPanel(
      tabsetPanel(
      route_map,
      station_map)
    )
  )
)

bike <- tabPanel(
  title='Bike Transport',
  h2('Bicycle route in Melbourne'),
  splitLayout(
    girafeOutput('plot_bike'),
    tableauPublicViz(
      id="tableauviz",
      url='https://public.tableau.com/views/Bicycle_16972615762500/BicyclerouteinMelbourne?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
      height='500px',
      width='800px'
    ),
    girafeOutput('plot_new')
  )
)

driver <- tabPanel(
  title='Car Transport',
  tabsetPanel(
    volumn_tab,parking_tab)
)


ui <- navbarPage(
  header=setUpTableauInShiny(),
  #use bootstrap
  theme = bslib::bs_theme(bootswatch = "quartz"),
  title='Commute to CBD',
  public_transport,
  bike,
  driver
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {
# public transport server  
  #station selected by user
  station_selected <-reactiveVal(NULL)
  
  #when user select a station, change the tableau map and filter the station data
  observeEvent(input$stop_select, {
    station_selected(station_data %>% filter(Stop_name == input$stop_select))
    # Filter Tableau viz by the state that was clicked on the bar chart
    station <- station_selected()$Stop_name
    runjs(sprintf('let viz = document.getElementById("tableau_station_map");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Name", ["%s"], FilterUpdateType.Replace);', station))
  }
  )
  
  #route patronage bar chart 
  output$route_patronage_bar_chart <- renderPlotly({
    #get changes of parameter in tableau map
    transport_mode <- input$tableau_route_map_parameter_changed$value[1]
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
             xaxis = list(title = "", showgrid = FALSE, tickfont = list(color = "white"), titlefont = list(color = "white")),
             yaxis = list(title = "",showticklabels = FALSE, showgrid = FALSE, tickfont = list(color = "white")),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(t = 50, b = 50,r = 50),
             legend = list(font = list(color = "white")))
  })
  
  #show weather 
  output$Weather_Today <- renderValueBox({
    valueBox(
      weather_information$weather$main[1], "Weather now"
    )
  })
  
  #show temperature
  output$temp_Today <- renderValueBox({
    valueBox(
      paste(weather_information$main$temp, "°C"), "Temperature now"
    )
  })
  #show feels like temperature
  output$feel_temp_Today <- renderValueBox({
    valueBox(
      paste(weather_information$main$feels_like, "°C"), "Feels like"
    )
  })
  
  #show wind speed
  output$wind_speed <- renderValueBox({
    valueBox(
      paste(weather_information$wind$speed,"m/s"), "Wind speed now"
    )
  })
  
  #show average patronage of selected station
  output$station_patronage <- renderValueBox({
    valueBox(
      station_selected()$Pax_norm_weekday, "Average Patronage Weekday"
    )
  })
  
  #weather icon
  output$Weather_icon <- renderUI({
    tags$img(src = icon_url)
  })
  
  directions_data <- reactiveVal(NULL)
  
  #when click the direction search button
  observeEvent(input$direction_search_btn, {
    #googleway function, which is used to find route between to location
    directions <- google_map_directions(
      origin = input$origin,
      destination = input$destination,
      travel_mode = "transit"
    )
    
    directions_data(directions)
  })
  
  location_data <- reactiveVal(NULL)

  #when click location search button
  observeEvent(input$location_search_btn, {
    #print(station_selected())
    places <- google_places(
      search_string = paste(input$location_type,station_selected()$Stop_name,"Melbourne",sep = ','),
      radius = 1000,
      key = map_api
    )$results
    places <- places[,c("name","business_status","user_ratings_total","rating")]
    location_data(places)
    }
    )

  #show location searched
  output$location_near_station <- renderTable({
    location_data() 
  })

# bike transport server
  output$plot_bike <- renderGirafe({
    
    # Filter the data and count the number of sightings in each state
    bike_counts <- bike_data %>%
      group_by(name) %>%
      summarize(Count = n())
    
    # ggplot graph for Statistics
    p <- ggplot(bike_counts) + aes(x = name, y = Count, tooltip = paste("Name: ", name, "\nCount: ", Count), data_id=name) +
      geom_bar_interactive(stat = 'identity', width = 0.8, fill = '#8f00b6') +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      labs(x = 'Bicycle Route type', y = 'Count') +
      theme_minimal() +
      ggtitle(paste("Bicycle Type Statistics"))+
      theme(panel.background=element_blank(),
            panel.grid.major.y=element_line(color='#e2e2e2'),
            axis.ticks=element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    girafe(ggobj=p, height_svg=8, width_svg = 12)
    
  })
  
  
  #javascript part
  observeEvent(input$plot_bike_selected, {
    # Clear selection from bar chart
    session$sendCustomMessage(type='plot_bike_set', message=character(0))
    
    # Filter Tableau map by the selected bike route
    name <- input$plot_bike_selected
    runjs(sprintf('let viz = document.getElementById("tableauviz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Name", ["%s"], FilterUpdateType.Replace);', name))
  })
  
  
  #tableau 
  output$plot_new <- renderGirafe({
    bike_tableau_name <- input$tableauviz_mark_selection_changed$Name[1]
    if(is.null(bike_tableau_name)) return()
    # Filter the data by the selected bike route
    filtered_data <- bike_data %>% 
      filter(name == bike_tableau_name)
    
    direction_counts <- filtered_data %>%
      group_by(direction) %>%
      summarize(Count = n())
    
    p <- ggplot(direction_counts) + aes(x = direction, y = Count, tooltip = paste("Direction: ", direction, "\nCount: ", Count), data_id=direction) +
      geom_bar_interactive(stat = 'identity', width = 0.8, fill = '#8f00b6') +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      labs(x = 'Direction', y = 'Count') +
      theme_minimal() +
      ggtitle(paste("Direction statistics"))+
      theme(axis.text.x = element_text(hjust = 1))
    girafe(ggobj=p, height_svg= 4)
  })

# car transport server
  ## Volumn - Slide for Day of Week 
  observeEvent(input$dayOfWeek, {
    min_val <- input$dayOfWeek[1]
    max_val <- input$dayOfWeek[2]
    runjs(sprintf('let viz = document.getElementById("volumn_tableau");
                   let sheet = viz.workbook.activeSheet;
                   sheet.applyRangeFilterAsync("Day Of Week", { min: %d, max: %d }, FilterUpdateType.REPLACE);', 
                  min_val, max_val))
  })
  
  
  ## Volumn - Bound's Check Box
  observeEvent(input$bound, {
    bound_selections <- input$bound
    bound_str <- paste(bound_selections, collapse = ", ")
    cat("Selected bound types:", bound_str, "\n")
    
    runjs(sprintf('let viz = document.getElementById("volumn_tableau");
        let sheet = viz.workbook.activeSheet;
        let boundTypesArray = "%s".split(", ").map(s => s.trim());
        sheet.applyFilterAsync("Bound", boundTypesArray, FilterUpdateType.Replace);', bound_str))
  })
  
  
  ## Parking - Parking type's Check Box
  observeEvent(input$parkingType, { ##listen to parkingType checkbox， once user change checkbox， it will receive the values
    parking_selections <- input$parkingType
    parking_str <- paste(parking_selections, collapse = ", ")
    cat("Selected parking types:", parking_str, "\n")
    
    runjs(sprintf('let viz = document.getElementById("parking_tableau");
        let sheet = viz.workbook.activeSheet;
        let parkingTypesArray = "%s".split(", ").map(s => s.trim());
        sheet.applyFilterAsync("Parking type", parkingTypesArray, FilterUpdateType.Replace);', parking_str))
  })
  

}



#############
# RUN SHINY #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))

