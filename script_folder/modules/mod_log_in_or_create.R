# login_module.R

login_UI <- function(id) {
  fluidPage(
    style = "min-height: 85vh; display: flex; flex-direction: column; justify-content: flex-start;", 
    useShinyjs(),
    fluidRow(
      div(style = "height: 15vh;") # Adds space above the main content
    ),
    fluidRow(
      column(
        width = 6, 
        offset = 3,
        align = "center",
        style = "background-color: #eef2f5; min-height: 50vh; padding-top: 20px;", # Add padding for internal spacing
        class = "rounded-column",
        
        # Main content
        h3("Account activation coming with betting feature!")
      )
    )
  )
  
}




login_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
  })
}


