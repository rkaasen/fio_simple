# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Buttom to go to estimation page

button_to_estimation_UI <- function(id) {
  
  tagList(
    actionButton(NS(id,"button_to_estimation"), label = "Choose!")
  )
  # ~~~~~~~~~~~
}


button_to_estimation_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$button_to_estimation, {
      trigger("button_to_estimation")
    })
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Buttom to go to sselection page

button_to_selection_UI <- function(id) {
  
  
  tagList(
    actionButton(NS(id,"button_to_selection"), label = "Back to Team Selection",class = "reactable-button")
  )
  # ~~~~~~~~~~~
}


button_to_selection_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$button_to_selection, {
      trigger("button_to_selection")
    })
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}