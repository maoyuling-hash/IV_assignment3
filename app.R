#install library this project use
list_packages <- c('ggplot2','shiny','dplyr','maps','leaflet','ggthemes','shinydashboard','scales','shinyWidgets','plotly',
                   'sf', 'readr', 'rnaturalearth', 'rnaturalearthdata','scales','shinyjs')
new_packages <- list_packages[!list_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

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
source('tableau-in-shiny-v1.0.R')
##################
# USER INTERFACE #
##################
public_transport <- tabPanel(
  title='Public Transport',
    tableauPublicViz(
      id='tableauViz',       
      url='https://public.tableau.com/views/Book1_16966699247180/Sheet1?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link',
      height="500px"
  )
  )


ui <- navbarPage(
  header=setUpTableauInShiny(),
  theme = bslib::bs_theme(bootswatch = "quartz"),
  title='Commute to CBD',
  public_transport
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {
  
}



#############
# RUN SHINY #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))

