# This is the script that was shown in the "Integrating Shiny into Tableau"
# video. Run this app before opening the provided Tableau workbook.

library('shiny')
library('ggiraph')

hosp_data <- read.csv('Hospitals in Australia with childbirth stats.csv')

# keep only maternity hospitals
hosp_data <- hosp_data[hosp_data$Childbirths.total != 0,]

##################
# USER INTERFACE #
##################

ui <- girafeOutput('plot_hospitals')

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  # A function to get the query parameters - the parts of the URL after
  # the ? sign, written in the form key=value&key=value&key=value
  getQueryStringData <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  # A function to get the states of Australia chosen by the user
  # in Tableau. Note that multiple selections are possible in Tableau
  # by dragging a box, so we need to account for that possibility.
  # You need to turn on the option "Allow multiple values via URL parameters"
  # in the Tableau "Edit URL Action" dialog.
  getChosenStates <- reactive({
    # If no state specified, return an empty vector
    result <- c()
    
    # If states were chosen, return the state names in a vector
    if ("state" %in% names(getQueryStringData())) {
      result <- strsplit(getQueryStringData()$state, ",")
    }
    
    result
  })
  
  output$plot_hospitals <- renderGirafe({
    hosp_data_filtered <- hosp_data[hosp_data$State %in% getChosenStates(),]
    
    p <- ggplot(hosp_data_filtered) + 
      aes(x=reorder(Name, Childbirths.total), y=Childbirths.total) +
      coord_flip() +
      geom_bar_interactive(stat='identity')
    
    girafe(ggobj=p)
  })
}

#############
# Run Shiny #
#############

shinyApp(ui, server, options=list(port=6245))
