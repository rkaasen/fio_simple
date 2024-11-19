



home_page_UI <- function(id) {
  
  
  fluidPage(
    
    # Hero section with overlay and content
    div(
      class = "hero-section",
      div(class = "hero-overlay"),
      div(
        class = "hero-content",
        div(style = "height: 100px;"),
        div(class = "big-text2", "Bringing Football analytics to you!"),
        div(style = "height: 130px;"),
        actionButton(NS(id,"how_to_use_btn"), "HOW TO USE", class = "reactable-button", width = "10%", style = "font-size: 22px; padding: 12px 10px; min-width: 100px;"),
        actionButton(NS(id,"try_it_btn"), "TRY IT YOURSELF !", class = "reactable-button", width = "12%", style = "font-size: 22px; padding: 12px 5px; min-width: 130px;")
      )
    ),
    # fluidRow(
    #   div(style = "height: 50px;"),
    #   column(offset = 2, width = 8, align = "center",
    #          h1("The first App where YOU are in charge of the stats:")
    #   )
    # ),
    fluidRow(
      column(offset = 2, width = 8,
             hr()
      )
    ),
    fluidRow(
      div(style = "height: 25px;"),
      column(width = 4, align = "center", style = "background-color: #eef2f5;", class = "rounded-column",
             div(style = "height: 25px;"),
             
             column(10, offset = 1, align = "center",
                    div(class = "methodology-box",
                        style = "height: 400px;",
                        div(style = "height: 25px;"),
                        h2("METHODOLOGY:"),
                        div(style = "height: 75px;"),
                        actionButton(NS(id,"methodology_btn"), "Learn more about the methods", class = "reactable-button", width = "55%", style = "font-size: 16px; padding: 10px 8px;")
                    ),
             ),
             div(style = "height: 50px;")
      ),
      column(width = 4, align = "center",
             div(style = "height: 25px;"),
             
             column(10, offset = 1, align = "center",
                    div(class = "data-box",
                        style = "height: 400px;",
                        div(style = "height: 25px;"),
                        h2("THE DATA:"),
                        div(style = "height: 75px;"),
                        actionButton(NS(id,"data_btn"), "The data we use", class = "reactable-button", width = "50%", style = "font-size: 18px; padding: 10px 8px;")
                    ),
             ),
             div(style = "height: 50px;")
      ),
      column(width = 4, align = "center",style = "background-color: #eef2f5;", class = "rounded-column",
             div(style = "height: 25px;"),
             
             column(10, offset = 1, align = "center", 
                    div(class = "authors-box",
                        style = "height: 400px;",
                        div(style = "height: 25px;"),
                        h2("THE AUTHORS:"),
                        div(style = "height: 75px;"),
                        actionButton(NS(id,"authors_btn"), "Who are we?", class = "reactable-button", width = "50%", style = "font-size: 18px; padding: 10px 8px;")
                    ),
             ),
             div(style = "height: 50px;")
      ),
    ),
    
    
  )
}


home_page_Server <- function(id, r6, navset_id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$try_it_btn, {
      trigger("button_app") 
    })
    observeEvent(input$how_to_use_btn, {
      trigger("button_HTU") 
    })
    
    observeEvent(input$methodology_btn, {
      trigger("button_methodology") 
    })
    observeEvent(input$data_btn, {
      trigger("button_data") 
    })
    observeEvent(input$authors_btn, {
      trigger("button_authors") 
    })
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}
