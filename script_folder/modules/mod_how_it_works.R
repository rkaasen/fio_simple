





how_it_works_UI <- function(id) {
  
  fluidPage(
    
    tags$style(HTML("
    #acc .accordion-item {
      margin-bottom: 10px; /* Adjust this value to change the space between panels */
    }

    #acc .accordion-item {
      margin-bottom: 10px; /* Space between panels */
      border-radius: 10px; /* Rounded corners */
      border: 2px solid #007bff; /* Thicker border with color */
    }

    #acc .accordion-button {
      border-radius: 10px; /* Rounded corners for the button */
      font-size: 24px; /* Adjust this value to change the title size */
      font-weight: 700;
    }

    #acc .accordion-body {
      border-radius: 0 0 10px 10px; /* Rounded bottom corners for the content */
    }
    
    .rounded-column-long {
    border-radius: 5px;
    background-color: #f0f0f0;
    padding: 10px;
    margin-bottom: 10px;
    min-height: 90vh;
    background: linear-gradient(to bottom, #f3f6f8, #c1c3c4);
    
    .large-button {
      font-size: 26px; /* Adjust this value to change the text size */
      padding: 10px 20px; /* Optional: Adds padding to make the button larger */
      transition: background-color 0.3s; /* Smooth transition for hover effect */
    }
    .large-button:hover {
      background-color: #4CAF50; /* Color on hover */
    }
    
}
    
  ")),
    
    br(),
    
    column(6, offset = 3,
           class = "rounded-column-long",
           
           column(10, offset = 1,
                  h1("Steps to using the app:"),
                  h4("Note that the app is still developing, so the steps might change"),
                  div(style = "height: 12px;")
           ),
           hr(),
           
           column(10, offset = 1,
                  
                  # 1
                  accordion(
                    accordion_panel( 
                      title = "   |    LOAD THE ANALYSIS PAGE", 
                      icon = bsicons::bs_icon("1-circle-fill"),
                      column(10,offset = 1, 
                             
                             # 'Click on the "YOUR ANALYSIS" Pane on the top right',
                             
                             # Add a div to contain the text with the highlighted word
                             div(
                               'Click on the  "',
                               span("  YOUR ANALYSIS  ", style = "background-color: #14499F; color: white; padding: 4px 8px; border-radius: 5px;"),
                               '"  Pane on the top right'
                             ),
                             
                             br(),br(),
                             "Or",
                             br(),br(),
                             'Go to home by clicking the logo in the top-left of the app or the "HOME" tab in the top right.',
                             br(),
                             
                             div(
                               # style = "display: flex; align-items: center; justify-content: center; ",
                               "Then click the", 
                               actionButton(NS(id,"try_it_btn_top"), label = "TRY IT YOURSELF !", width = "160px",style = "height: 35px",class = "reactable-button" ),
                               '  button in the middle of the screen'
                             ),

                             
                      )
                    ),  
                    
                    
                    #2
                    accordion_panel(
                      title = "   |    SELECT A MATCHUP TO ANALYSE",
                      icon = bsicons::bs_icon("2-circle-fill"),
                      column(10,offset = 1, 
                             
                             div(
                               # style = "display: flex; align-items: center; justify-content: center; ",
                               "After finding the match you want to analyze:", 
                               br(),
                               'Click the  ',
                               actionButton(NS(id,"dummy1"), label = "ANALYZE", width = "95px",style = "height: 35px",class = "reactable-button" ),
                               '  button in the right side of the table'
                             ),
# 
#                              br(),br(),
#                              "To find the Specific match you want to analyze,",
#                              br(),
#                              "Scroll the table, or use the navigation in the left side of the selection page"
                             
                      )
                    ),  
                    
                    #3
                    accordion_panel(
                      title = "   |    SELECT THE STATS YOU BELIEVE TO BE RELEVANT",
                      icon = bsicons::bs_icon("3-circle-fill"),
                      column(10,offset = 1, 
                             
                             'In the left side of the screen, you can choose the stats in which you believe will define the match outcome:',
                             br(),br(),
                             
                             column(8, offset = 2, align = "center", class = "rounded-column",
                                    column(12, align = "left",
                                           h5("Select Stats:"),
                                           checkboxGroupInput(
                                             NS(id,"dummy2"), label = "",
                                             choices = list("POINTS" = "POINTS", 
                                                            "GOALS SCORED" = "GOALS SCORED",
                                                            "GOALS CONCEDED" = "GOALS CONCEDED",
                                                            "SHOTS ON TARGET" = "SHOTS ON TARGET",
                                                            "SHOTS ON TARGET AGAINST" = "SHOTS ON TARGET AGAINST"
                                             ), 
                                             selected = c("POINTS", "GOALS SCORED", "GOALS CONCEDED")
                                           )
                                    )
                             )
                             
                             
                      )
                    ),  
                    
                    #4
                    accordion_panel(
                      title = "   |    ADJUST IMPORTANCE OF EACH STAT TO YOUR BELIEF",
                      icon = bsicons::bs_icon("4-circle-fill"),
                      column(10,offset = 1, 
                             
                             'On the right side of each stat, you can chose the weight of each stat',
                             br(),br(),
                             
                             column(8, offset = 2, align = "left", class = "rounded-column",
                                    
                                    h6("Weight of stat:"),
                                    div(style = "height: 10px;"),
                                    column(12, align = "center",
                                           sliderTextInput(NS(id,"dummy3"),choices = paste0(seq(0,200,25), "%"), label = NULL, selected = "100%", width = "65%"),
                                    )
                             ),
                             
                             
                             br(),br(),
                             
                             div(
                               # style = "display: flex; align-items: center; justify-content: center; ",
                               'If you want to see more info on the stat, click the',
                               br(),
                               h6("Analyze Statistic:"), 
                               actionButton(NS(id,"dummy4"), label = icon("chart-line"), width = "60px",style = "height: 35px",class = "reactable-button" ),
                               "  button on the left of each stat"
                             )
                             
                             
                             
                             
                             
                      ) 
                    ),  
                    
                    #5
                    accordion_panel(
                      title = "   |    YOU HAVE NOW PREDICTED THE OUTCOME",
                      icon = bsicons::bs_icon("5-circle-fill"),
                      column(10,offset = 1, 
                             
                             'Now you can see the prediction for your match',
                             br(),
                             'It shows in the top-middle of the screen between the logos of your selected teams',
                             br(),br(),
                             
                             column(8, offset = 2, align = "center", class = "rounded-column",
                                    br(),
                                    img(src = "Example_outcome.jpg", width = "80%"),
                                    br()
                             ),
                             
                             
                             
                      ) 
                    ),
                    
                    #6
                    accordion_panel(
                      title = "   |    TRY IT OUT!",
                      icon = bsicons::bs_icon("6-circle-fill"),
                      column(10,offset = 1, align = "center",
                             actionButton(NS(id,"try_it_btn"), label = "TRY IT NOW", class = "large-button")
                      ) 
                    ),
                    id = "acc"
                  ),
                  div(style = "height: 40px;"),
           )
    )
  )
  
}


how_it_works_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$try_it_btn, {
      trigger("button_app") 
    })
    observeEvent(input$try_it_btn_top, {
      trigger("button_app") 
    })
    
    observeEvent(input$dummy1, ignoreInit = T, {
      showNotification("Example button only.", type = "error")
    })
    observeEvent(input$dummy2, ignoreInit = T, {
      showNotification("Example list only.", type = "error")
    })
    observeEvent(input$dummy3, ignoreInit = T, {
      showNotification("Example slider only.", type = "error")
    })
    observeEvent(input$dummy4, ignoreInit = T, {
      showNotification("Example list only.", type = "error")
    })
    
    
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}