# GEOM90007 Information Visualisation
#
# A simple debugging script that simply displays the values being sent
# from Tableau in the query string.
#
# For example, if you create a Tableau URL action associated to a Source Sheet
# with a "State" dimension, with the URL of that action set to
#    http://127.0.0.1:6245?state=<State>
# and then you select Western Australia from the sheet, this script will display
#    List of 1
#     $ state: chr "Western Australia"
# This confirms that R is correctly receiving and decoding the information from
# Tableau.

library('shiny')

ui <- verbatimTextOutput('one')

server <- function(input, output, session) {
  # Get the values from the query string (the part of the URL after ?)
  getQueryStringData <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  # Render the query string values as parsed by R
  output$one <- renderText({
    paste0(capture.output(str(getQueryStringData())), collapse='\n')
  })
}

shinyApp(ui, server, options=list(port=6245))
