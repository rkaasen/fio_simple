




methodology_UI <- function(id) {
  
  fluidPage(
    fluidRow(
      style = "min-height: 95vh;",
      column(width = 6, offset = 3,style = "background-color: #eef2f5;", class = "rounded-column",
             h2("THE CHOICE IS YOURS"),
             br(), br(),
             h4("Predicting the outcome of a game should be based on data"),
             br(), br(),
             h4("Metrics used for prediction should vary based on:"),
             # br(),
             h6("- The game that is beeing predicted"), 
             # br(),
             h6("- The beliefs of the predicter (the user)"), 
             # br(),
             h6("- and much more..")
             
             
             
      )
      
    )
    
  )
  
}


methodology_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}