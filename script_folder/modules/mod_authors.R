

authors_UI <- function(id) {
  
  fluidPage(
    fluidRow(
      style = "min-height: 95vh;",
      
      column(width = 6, offset = 3,style = "background-color: #eef2f5;", class = "rounded-column",
             h2("Feedback"),
             h5("ALL and any feedback will be appriciated!"),
             "If you need more space, or want to discuss a matter, please reach out directly on one of the links further down the page",
             
             fluidRow(
               column(
                 width = 12,
                 div(
                   textInput(NS(id, "name"), "Name", placeholder = "Enter your name", width = "100%"),
                   style = "width: 25vw;"
                 ),
                 div(
                   textInput(NS(id, "email"), "Email (optional)", placeholder = "Enter your email", width = "100%"),
                   style = "width: 25vw;"
                 ),
                 div(
                   textAreaInput(NS(id, "feedback"), "Feedback", placeholder = "Enter your feedback here...", height = "150px", width = "100%"),
                   style = "width: 35vw;"
                 ),
                 actionButton(NS(id, "submit_feedback"), "Submit", class = "reactable-button")
               )
             ),
             
             fluidRow(
               column(
                 width = 12,
                 textOutput(NS(id,"submission_status")) # Feedback submission status
               )
             ),
             
             
             
             
             hr(),
             h2("Please do reach out!"), 
             br(),
             h5("- If you have FEEDBACK. Positive or Negative."), 
             # br(),
             h5("- If you have ideas and/or want to CONTRIBUTE."), 
             # br(),
             h5("- if you simply want to CHAT about this project, a project you are working on, or something else."), 
             # br(),
             h5("- Or anything something differnet.. :-)"), 
             br(), br(), 
             hr(),
             h2("The author:"),
             br(),
             h4("Rasmus Kaasen"),
             a("Linked", href = "https://www.linkedin.com/in/rasmus-kaasen/", target = "_blank", style = "color: blue; text-decoration: none;"),
             HTML("&nbsp; &nbsp; | &nbsp; &nbsp;"),
             a("Mail", href = "mailto:Kaasen1995@hotmail,com", target = "_blank", style = "color: blue; text-decoration: none;"),
             HTML("&nbsp; &nbsp; | &nbsp; &nbsp;"),
             a("Github", href = "https://github.com/rkaasen/fixtureio", target = "_blank", style = "color: blue; text-decoration: none;"),
             h6("(Github repo currently private.)")
             
             
             
      ) 
      
    )
    
  )
  
}


authors_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    # Observe the feedback submission event
    observeEvent(input$submit_feedback, {
      name <- input$name
      email <- input$email
      feedback <- input$feedback
      
      # Validate the inputs
      if (feedback == "") {
        output$submission_status <- renderText("Feedback cannot be empty!")
        return()
      }
      
      # Write feedback to the database
      tryCatch(
        {
          print("call function")
          write_feedback_to_db(name, email, feedback)
          
          
          output$submission_status <- renderText("Thank you for your feedback!")
          
          # Clear inputs after submission
          updateTextInput(session, "name", value = "")
          updateTextInput(session, "email", value = "")
          updateTextAreaInput(session, "feedback", value = "")
        },
        error = function(e) {
          output$submission_status <- renderText("An error occurred: Unable to save feedback.")
        }
      )
    })
    
    
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}