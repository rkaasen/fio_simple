


match_data_row_UI <- function(id) {
  
  fluidRow(
    
    class = "sticky-fluidrow",
    column(12, 
           # offset = 1,  
           style = "background: linear-gradient(to bottom, #eef2f5, #98AFD3); margin-bottom: 0px;",
           class = "rounded-column",
           
           fluidRow(
             column(3,align = 'right',
                    column(10, offset = 2, h2(textOutput(NS(id,"home_team")), style = "font-weight: bold;")),
                    column(10, offset = 2, align = "right", 
                           plotOutput(NS(id,"form_home"), width = "150px", height = "40px")),
                    column(10, offset = 2, 
                           div(h6(textOutput(NS(id,"home_team_league_rank")), style = "display: inline; font-weight: bold; margin-right: 5px;"), 
                               h6("in league",style = "display: inline;  margin-left: 0px;"),
                               style  = "display: inline-flex; align-items: right; "
                           )
                    )
                    
             ), 
             # LOGO HOME TEAM
             column(1, align = "center", 
                    div(style = "height: 10px;"),
                    uiOutput(NS(id,"logo_ht"), style = "cursor: pointer;")
             ),
             column(4,align = 'center',
                    style = "cursor: pointer;",
                    id = NS(id,"clicked_result_col"), style = "cursor: pointer;",
                    
                    h3(textOutput(NS(id, "league"))),
                    h5(textOutput(NS(id, "date"))),
                    h5(textOutput(NS(id, "time"))),
                    hidden(div(id = NS(id, "hide_row"), h5("YOUR ESTIMATED OUTCOME:"))),
                    div( id = NS(id,"ggplot_overall_percent_div"), class = "clickable-border",
                         plotOutput(NS(id,"ggplot_overall_percent"), width = "100%", height = "90px")
                    )
             ),
             
             # LOGO AWAY TEAM
             column(1, align = "center", 
                    div(style = "height: 10px;"),
                    uiOutput(NS(id,"logo_at"), style = "cursor: pointer;")
                    
             ),
             
             column(3, offset = 0,align = 'left',
                    h2(textOutput(NS(id,"away_team")), style = "font-weight: bold;"),
                    
                    plotOutput(NS(id,"form_away"), width = "150px", height = "40px"),
                    column(10, offset = 2, 
                           div(h6(textOutput(NS(id,"away_team_league_rank")), style = "display: inline; font-weight: bold; margin-right: 5px;"), 
                               h6("in league",style = "display: inline;  margin-left: 0px;"),
                               style  = "display: inline-flex; align-items: left; "
                           )
                    )
                    
             ),
             
           ),
           # column(width = 1)
    )
  )
  
  
}




match_data_row_Server <- function(id, r6, expl_stats = T) {
  moduleServer(id, function(input, output, session) {
    
    if(expl_stats == F){
      shinyjs::hide("league")
      shinyjs::hide("date")
      shinyjs::hide("time")
      shinyjs::show("hide_row")
    }
    
    
    observeEvent(watch("button_to_estimation"),ignoreInit = T,   {
      
      league_selected <- r6$data$league
      output$league <- renderText(league_selected)
      
      date_to_use <- paste0(f_format_date_with_suffix(as.Date(r6$data$match_date)), " ", r6$data$match_time, "h")
      
      output$date <-renderText(date_to_use)
      
      output$home_team <- renderText(r6$selected_home_team)
      output$away_team <- renderText(r6$selected_away_team)
      
      home_placement_string <- paste0(r6$standings[[league_selected]] %>% filter(Team == r6$selected_home_team) %>% pull(rank) %>% toOrdinal())
      output$home_team_league_rank <- renderText(home_placement_string)
      
      away_placement_string <- paste0(r6$standings[[league_selected]] %>% filter(Team == r6$selected_away_team) %>% pull(rank) %>% toOrdinal())
      output$away_team_league_rank <- renderText(away_placement_string)
      
      
      output$form_home <-  f_form_plot(r6$data$filtered, r6$selected_home_team, r6$data$match_date)
      output$form_away <-  f_form_plot(r6$data$filtered, r6$selected_away_team, r6$data$match_date)
      
      output$logo_ht <- renderUI({
        img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "100%", width = "100%")
      })
      output$logo_at <- renderUI({
        img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "100%", width = "100%")
      })
      
    })
    
    observeEvent(watch("update_main_prediction"), ignoreInit = T, {
      metrics <- r6$metrics
      
      # Calculate overall percentages
      overall_percentages <-
        f_calculate_overall_percentages(
          metrics,
          Home_label = r6$selected_home_team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short),
          Away_label = r6$selected_away_team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short)
        )
      
      output$ggplot_overall_percent = renderPlot({
        overall_percentages
      }, bg="transparent")
      
      
    })
    
    # card_for_match_details_Server("card_module")
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}



