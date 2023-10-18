library('shiny')
library('ggplot2')
library('ggiraph')
library('leaflet')
library('dplyr')
library('tidyr')
library('shinyjs')

source('tableau-in-shiny-v1.0.R')

# read csv
bike_data <- read.csv("bicycle.csv")

# tab
bike_tab<- tabPanel(
  title='Bicycle',
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

# create UI
ui <- navbarPage(
  header=setUpTableauInShiny(),
  title = 'Melbourne transport',
  bike_tab
)

# create server
server <- function(input, output, session) {
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
  
}


# run shiny
shinyApp(ui, server, options=list(launch.browser=TRUE))
