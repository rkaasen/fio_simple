

card_for_match_details_UI <- function(id) {
  
  fluidRow(
    
    
    hidden(
      div(
        id = NS(id, "card_div"),
        align = "left",
        
        card(
          id = NS(id, "my_card"),
          full_screen = TRUE,
          class = "p-3",
          # card_header(h3("Teams- and Match details")),
          card_body(
            navs_tab_card(
              id = NS(id,"tabs"),  
              # HOME TEAM TAB
              nav(value = NS(id,"home_team_tab"), h4(textOutput(NS(id,"home_team"))), class = "my-gradient-background-white-start",
                  div( align = "center",
                       uiOutput(NS(id,"logo_ht")),
                       div(style = "height: 10px;"),
                       reactableOutput( NS(id,"home_last_10"))
                  )
                  
              ),
              
              # MATCH TAB
              nav(value = NS(id,"the_match_tab"), h4("SELECTED MATCH"), class = "my-gradient-background-white-start",
                  div( align = "center",
                       div(style = "height: 20px;"),
                       match_data_row_UI(NS(id,"in_main_card")),
                       
                       div(style = "height: 10px;"),
                       
                       # fair offs row
                       fluidRow(
                         column(12, offset = 0,  
                                class = "rounded-column",
                                style = "background: #7283b9;",
                                fluidRow(
                                  column(4, align = "left", div(style = "height: 7px;"),
                                         h5("'Fair' odds based on estimation:", style = "color: white;"),
                                  ),
                                  column(4,
                                         fluidRow(
                                           column(2, offset = 1, align = "middle", h4(textOutput(NS(id,"fair_home")), style = "color: white;")),
                                           column(2, offset = 2, align = "middle", h4(textOutput(NS(id,"fair_draw")), style = "color: white;")),
                                           column(2, offset = 2, align = "middle", h4(textOutput(NS(id,"fair_away")), style = "color: white;")),
                                         )),
                                  column(2, offset = 0, align = "right", div(style = "height: 12px;"),
                                         h5("Not live yet: ", style = "color: white;"),
                                         
                                  ),
                                  column(1, offset = 0, align = "left",div(style = "height: 3px;"),
                                         
                                         # disabled(
                                         actionButton(NS(id,"bet_btn"), " BET " ,class = "reactable-button", width = "75%", style = "padding-top: 8px;" )
                                         # )
                                  )
                                  
                                  
                                  
                                  
                                )
                         )
                       ),
                       
                       # ~~~~~~~~~~~~~~~~~~~~~~~~~~
                       # Your odds row
                       hidden(
                         fluidRow( id = NS(id,"your_odds_row"),
                                   
                                   column(12, offset = 0,  
                                          class = "rounded-column",
                                          style = "background: #505c83;",
                                          fluidRow(
                                            column(4, align = "left", div(style = "height: 9px;"),
                                                   h5("Your current odds (Placeholder values):", style = "color: white;"),
                                            ),
                                            column(4,
                                                   fluidRow(
                                                     column(2, offset = 1, align = "middle", actionButton(NS(id,"btn_home_odds"), textOutput(NS(id,"home_odds")))),
                                                     column(2, offset = 2, align = "middle", actionButton(NS(id,"btn_draw_odds"), textOutput(NS(id,"draw_odds")))),
                                                     column(2, offset = 2, align = "middle", actionButton(NS(id,"btn_away_odds"), textOutput(NS(id,"away_odds"))))
                                                   ))
                                            
                                            
                                          )
                                   ),
                                   
                                   div(style = "height: 25px;"),
                                   
                                   fluidRow(
                                     style = "display: flex;",
                                     
                                     column(8, offset = 2,
                                            h3("Betting feature not open yet in app"),
                                            h5("Sign up for early access (link in the yellow info bar)"),
                                     )
                                     
                                   )
                         )
                       )
                  ),
              ),
              # AWAY TEAM TAB
              nav(value = NS(id,"away_team_tab"),h4(textOutput(NS(id,"away_team"))), class = "my-gradient-background-white-start",
                  div( align = "center",
                       uiOutput(NS(id,"logo_at")),
                       div(style = "height: 10px;"),
                       reactableOutput( NS(id,"away_last_10"))
                  )
              )
              
              
            )
          )
        )
      )
    )
  )
  
}


card_for_match_details_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    home_odds <- reactiveVal(NULL)
    draw_odds <- reactiveVal(NULL)
    away_odds <- reactiveVal(NULL)
    
    fair_home_odds <- reactiveVal(NULL)
    fair_draw_odds <- reactiveVal(NULL)
    fair_away_odds <- reactiveVal(NULL)
    
    to_win <- reactiveVal(NULL)
    to_win_odds <- reactiveVal(NULL)
    
    
    
    match_id_chosen <- reactiveVal(NULL)
    bet_open <- reactiveVal(NULL)
    


    
    #~~~~~~~~~~~~~~~~~
    # Module Server
    
    match_data_row_Server("in_main_card", r6, expl_stats = F)
    
    # toggle the card:
    
    observeEvent(list(input$external_toggle, watch("open_analytics_card")), ignoreInit = T, {
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-the_match_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    observeEvent(watch("open_analytics_card_home"), ignoreInit = T,{
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-home_team_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    observeEvent(watch("open_analytics_card_away"), ignoreInit = T,{
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-away_team_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    
    observeEvent(watch("full_screen_card_closed"), {
      shinyjs::hide("card_div")
    })
    
    
    # code for server:
    
    output$logo_ht <- renderUI({
      img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "10%", width = "10%")
    })
    output$logo_at <- renderUI({
      img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "10%", width = "10%")
    })
    
    
    observeEvent(list(input$external_toggle,watch("open_analytics_card_home"), watch("open_analytics_card_away"), watch("open_analytics_card")), ignoreInit = T, {
      
      shinyjs::removeClass(id = "top_of_estimation_tab-card_module-in_main_card-ggplot_overall_percent_div", class = "clickable-border", asis = T)
      shinyjs::removeClass(id = "top_of_estimation_tab-in_main_page-ggplot_overall_percent_div", class = "clickable-border", asis = T)
      
      home_team <- r6$selected_home_team
      away_team <- r6$selected_away_team
      
      match_id_chosen(paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending))
      
      bet_open_val <- f_match_open_for_betting() %>% 
        filter(match_id == match_id_chosen()) %>% 
        pull(bet_open)
      bet_open(bet_open_val)
      
      
      output$home_team <- renderText(toupper(home_team))
      output$away_team <- renderText(toupper(away_team))
      
      output$home_last_10 <- renderReactable(
        f_last_10_table(r6$data$filtered, home_team)
      )
      output$away_last_10 <- renderReactable(
        f_last_10_table(r6$data$filtered, away_team)
      )
      
      # hide the odds each time the card is opened
      shinyjs::hide("your_odds_row")
      
      # Reset the 3 buttons, so they dont show last game's values
      output$home_odds <- renderText("NA")
      output$away_odds <- renderText("NA")
      output$draw_odds <- renderText("NA")
      
      # text under the buttons not to show last selected team (from last card)
      # shinyjs::hide("place_bet_row")
      # shinyjs::show("chose_bet_row")
      
      # check if betting is open
      
      # match_id_chosen = paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending)
      
      
    })
    
    
    observeEvent(watch("update_main_prediction"), ignoreInit = T, {
      metrics <- r6$metrics
      
      # Calculate overall percentages
      overall_percentages <-
        f_calculate_overall_percentages(
          metrics,
          Home_label = r6$selected_home_team_short,
          Away_label = r6$selected_away_team_short,
          return_only_percentage_df = TRUE
        )
      
      home_perc = overall_percentages[1]
      draw_perc = overall_percentages[2]
      away_perc = overall_percentages[3]
      
      fair_home_odds(round(100/home_perc,2))
      fair_draw_odds(round(100/draw_perc,2))
      fair_away_odds(round(100/away_perc,2))
      
      output$fair_home <- renderText(fair_home_odds())
      output$fair_draw <- renderText(fair_draw_odds())
      output$fair_away <- renderText(fair_away_odds())
      
    })
    
    observeEvent(input$bet_btn, {
      
      shinyjs::show("your_odds_row")
      
      if(bet_open()){
      odds_row <- r6$odds$pl %>% 
        filter(
          HomeTeam == r6$selected_home_team,
          AwayTeam == r6$selected_away_team
        )
      
      home_perc_bet <- odds_row %>% pull(`Home Win Probability (%)`)
      away_perc_bet <- odds_row %>% pull(`Away Win Probability (%)`)
      draw_perc_bet <- odds_row %>% pull(`Draw Probability (%)`)
      
      # Needs to be calculated better:
      home_odds((100/home_perc_bet))
      away_odds((100/away_perc_bet))
      draw_odds((100/draw_perc_bet))
      
      output$home_odds <- renderText(round(as.numeric(home_odds()),2))
      output$away_odds <- renderText(round(as.numeric(away_odds()),2))
      output$draw_odds <- renderText(round(as.numeric(draw_odds()),2))
      
      if(home_odds()-fair_home_odds() > 0.2){
        shinyjs::addClass(id = "btn_home_odds", class = "odds-button-better")
      } else if(home_odds()-fair_home_odds() < -0.2){
        shinyjs::addClass(id = "btn_home_odds", class = "odds-button-worse")
      } else{
        shinyjs::addClass(id = "btn_home_odds", class = "odds-button-same")
      }
      
      if(draw_odds()-fair_draw_odds() > 0.2){
        shinyjs::addClass(id = "btn_draw_odds", class = "odds-button-better")
      } else if(draw_odds()-fair_draw_odds() < -0.2){
        shinyjs::addClass(id = "btn_draw_odds", class = "odds-button-worse")
      } else{
        shinyjs::addClass(id = "btn_draw_odds", class = "odds-button-same")
      }
      
      if(away_odds()-fair_away_odds() > 0.2){
        shinyjs::addClass(id = "btn_away_odds", class = "odds-button-better")
      } else if(away_odds()-fair_away_odds() < -0.2){
        shinyjs::addClass(id = "btn_away_odds", class = "odds-button-worse")
      } else{
        shinyjs::addClass(id = "btn_away_odds", class = "odds-button-same")
      }
      
      }
    })
    
    
    
    observeEvent(input$btn_home_odds, {
      if(bet_open()){
        showNotification("Betting feature to be released")
      } else{
        showNotification("NA, as game is out of betting scope")
      }
      
    })
    observeEvent(input$btn_draw_odds, {
      
      if(bet_open()){
        showNotification("Betting feature to be released")
      } else{
        showNotification("NA, as game is out of betting scope")
      }
      
    })
    observeEvent(input$btn_away_odds, {
      
      if(bet_open()){
        showNotification("Betting feature to be released")
      } else{
        showNotification("NA, as game is out of betting scope")
      }
    })
    
    
   
    
    
    
  })
}