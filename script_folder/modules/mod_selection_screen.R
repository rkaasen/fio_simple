

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI


team_select_UI <- function(id) {
  
  
  fluidPage(
    
    
    
    div(class = "gradient-bg-estimation",
        br(),
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
            radioButtons(
              inputId = NS(id, "league_select_buttons"), label = "Select League",
              # choices = c("EPL - English Premier League" = "pl", "La Liga" = "ll", "League 3" = "l3", "Other League" = "other")),
              choices = c("EPL - English Premier League"
                          #, "La Liga", "League 3", "Other League"
              )),
            # hidden(
            #   selectInput(
            #     inputId = NS(id, "league_select"), label = "Other League",
            #     choices = c("Other 1", "Other 2", "Other 3"))
            # ),
            # 
            hr(),
            
            selectInput(
              inputId = NS(id, "team_select"), label = "Select Team",
              choices = c("Select Team")),
            
            
            
            
          ), # END SIDEBAR
          
          
          mainPanel(
            fluidRow(
              div(style = "height: 6px;"),
              column(9,
                     align = "center",
                     reactableOutput( NS(id,"schedule_table"))
              ),
              
              
            )
          )
        )
    )
    
  )}


team_select_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # CONTROLS ON SIDE BAR
    observeEvent(input$league_select_buttons, {
      
      # set the selected league in the r6
      r6$selected_league <- input$league_select
      # update the dataset to be used:
      if(input$league_select_buttons == "EPL - English Premier League") {
        hide("league_select")
        r6$data$league = "English Premier League"
        r6$data$filtered = r6$data$pl_historic
        r6$data$schedule = r6$data$pl_schedule
        r6$team_list$league = r6$team_list$pl_teams 
        
      } else if(input$league_select_buttons == "La Liga"){
        hide("league_select")
        r6$data$league = "La Liga"
        r6$data$filtered = r6$data$ll_historic
        r6$data$schedule = r6$data$ll_schedule
        # r6$team_list$league = r6$team_list$ll_teams
        r6$team_list$league = c("Sorry, League not available")
      } else if(input$league_select_buttons == "Other League"){
        show("league_select")
        r6$data$schedule = r6$data$ll_schedule %>% filter(1==2)
      }
      else {
        hide("league_select")
        r6$team_list$league = c("Sorry, League not available")
        r6$data$schedule = r6$data$ll_schedule %>% filter(1==2)
      }
      
      if(input$league_select_buttons != "EPL - English Premier League") { # DONT KEEP THIS PART WHEN MORE LEAGUES THAN PL
        # update available teams in the team selectors:
        updateSelectInput(session, "team_select",
                          choices = c("Select Team", r6$team_list$league)
        )
      } else{ 
        # update available teams in the team selectors:
        updateSelectInput(session, "team_select",
                          choices = c("Select Team", r6$team_list$league %>% pull(Team) )
        )
      }
      
    })
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # CONTROLS main table
    
    observeEvent(list(input$league_select_buttons,input$league_select, input$team_select, watch("user_logged_in"), watch("user_logged_out")), {
      
      # filter for the right data to be showed
      if(input$team_select != "Select Team") {
        df_filtered_team <- 
          rbind(r6$data$schedule %>% filter(HomeTeam == input$team_select),
                r6$data$schedule %>% filter(AwayTeam == input$team_select)
          ) 
      } else{
        
        df_filtered_team <- r6$data$schedule
      }
      
      
      df_use <- df_filtered_team %>% 
        select(Date, Time, HomeTeam, AwayTeam) %>% 
        filter(Date >= yesterday) %>%
        arrange(Date) %>% 
        f_prepare_schedule_view %>% 
        mutate(
          analyze = "", date_use = Date)
      
      formatted_dates_list <- lapply(df_use$date_use, f_format_date_with_suffix) 
      
      df_use <- cbind(df_use, tibble(formatted_dates = unlist(formatted_dates_list))) %>% 
        mutate(Date = formatted_dates) %>% 
        select(-formatted_dates) 
      
      
      columns_list = list(
        Date = colDef(minWidth = 165, align = "left", vAlign = "center"),
        date_use = colDef(show = F), 
        Time = colDef(minWidth = 85, align = "center", , vAlign = "center"), 
        HomeTeam = colDef(name = "Home Team", minWidth = 200, align = "right", vAlign = "center"),
        vs_col = colDef(name = "", minWidth = 40, align = "center", vAlign = "center"),
        AwayTeam = colDef(name = "Away Team", minWidth = 200, vAlign = "center", align = "left"),
        # Analyze column:
        analyze = colDef(
          name = "",
          sortable = FALSE, vAlign = "center",
          cell = function() htmltools::tags$button(class = "reactable-button", "Analyze")
        )
      )
      
      df_odds_open <- f_match_open_for_betting() %>% 
        mutate(Bets = ifelse(bet_open, "Open", "Not Open")) %>% 
        select(match_id, Bets)
      
      
      df_use <- df_use %>%
        
        mutate(
          season_ending = f_calc_season_ending(utc_date),
          match_id = paste0(HomeTeam, "-", AwayTeam , "-", season_ending),
        ) %>% 
        left_join(df_odds_open) %>% 
        
        mutate(
          col_def = case_when(
            Bets == "Open" ~ "#4CAF50",
            Bets == "Not Open" ~ "#e22020",
            T ~ "#4CAF50"
          )
        ) %>% 
        select(-match_id, -season_ending)
      
      
      columns_list$Bets <- colDef( minWidth = 110 , style = list(fontSize = "12px") , vAlign = "center",
                                   cell = pill_buttons( data = df_use,
                                                        color_ref  = "col_def",
                                                        text_color = "white"  # White text color for contrast
                                   )
      )
      
      columns_list$col_def <-  colDef(show = F)
      
      
      
      output$schedule_table <- renderReactable({
        reactable(df_use, 
                  columns = columns_list,
                  # groups (by date)
                  groupBy = "Date",  defaultExpanded = TRUE,
                  # Default stuff
                  pagination = FALSE, sortable = FALSE, fullWidth = FALSE,
                  onClick = JS("function(rowInfo, column) {
                    // Only handle click events on the 'details' column
                    if (column.id !== 'analyze') {
                      return
                    }

                    if (window.Shiny) {
                      Shiny.setInputValue('ui_on_selection_tab-analyze', rowInfo.values['HomeTeam'] + '&&' + rowInfo.values['AwayTeam']
                      + '&&' + rowInfo.values['date_use'] + '&&' + rowInfo.values['Time'], { priority: 'event' })
                    }
                  }"),
                  
                  class = "selection_table",
                  
                  theme = reactableTheme(
                    backgroundColor = "transparent"
                  )
        )
        
        
      })
      
      
      
    })
    
    
    
    observeEvent(input$analyze, {
      
      if(!input$analyze == "null&&null&&null&&null") {
        split <- input$analyze %>% str_split(string = ., pattern =  '&&')
        r6$selected_home_team <- split[[1]][1]
        r6$selected_away_team <- split[[1]][2]
        r6$data$match_date    <- split[[1]][3]
        r6$data$match_time    <- split[[1]][4]
        
        r6$selected_home_team_short = r6$team_list$league %>% filter(Team == split[[1]][1]) %>% pull(Team_short)
        r6$selected_away_team_short = r6$team_list$league %>% filter(Team == split[[1]][2]) %>% pull(Team_short)
        
        trigger("button_to_estimation")
      }
      
    })
    
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}










