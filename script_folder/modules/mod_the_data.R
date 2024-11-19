

the_data_UI <- function(id) {
  
  fluidPage(
    fluidRow(
      style = "min-height: 95vh;",
      
      column(width = 6, offset = 3,style = "background-color: #eef2f5;", class = "rounded-column",
             h2("The data sources:"),
             br(), br(),
             h4("Historical Match data:"),
             br(), 
             h5("Historical data is beeing loaded from "), 
             a("Football-Data.co.uk", href = "https://www.football-data.co.uk/", target = "_blank", style = "color: blue; text-decoration: none;"), br(),
             "(Updates each Monday- and Thursday morning)",
             
             br(), br(), br(),
             h5("Fixture data is beeing loaded from "), 
             a("Fixturedownload.com", href = "https://fixturedownload.com/", target = "_blank", style = "color: blue; text-decoration: none;"), br(),
             "(Updates each Monday morning)"
             
      )
    )
  )
  
}


the_data_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}