

source("script_folder/modules/mod_match_data_row.R")
source("script_folder/modules/mod_stat_numeric_4.R")
source("script_folder/modules/mod_card_for_match_details.R")


estimation_page_info_UI <- function(id) {
  
  fluidPage(
    
    # hidden(
    #   div(class = "full-page-spinner", tags$img(src = "spinner.gif"))
    # ),
    # 
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
      tags$script(src = "customtwo.js?v=1.0") # Increment the version (v=1.0) when updating
    ),
    
    div(class = "my-gradient-background",
        style = "min-height: 95vh;",
        
        fluidPage(
          
          page_sidebar(
            sidebar = sidebar(
              width = 400,
              class = "my-gradient-background",
              
              fluidRow(
                style = "display: flex; align-items: center;",
                column(6,card_for_match_details_UI(NS(id,"card_module"))),
                column(6,button_to_selection_UI("button_on_estimation_tab")),
              ),
              
              hr(),
              column(11, offset = 1,
                     
                     h4("Main stats:"),
                     
                     div( class = "groupbasic",
                          checkboxGroupInput(
                            NS(id,"choose_stats"), label = "",
                            choices = list(
                              "POINTS" = "POINTS", 
                              "GOALS SCORED" = "GOALS_SCORED",
                              "GOALS CONCEDED" = "GOALS_CONCEDED",
                              "SHOTS ON TARGET" = "SHOTS_ON_TARGET",
                              "SHOTS ON TARGET CONCEDED" = "SHOTS_ON_TARGET_CONCEDED"
                            ), 
                            selected = c("POINTS", "GOALS_SCORED", "GOALS_CONCEDED")
                          )
                     ),
                     
                     hr(),
                     
                     h4("Head-To-Head Stats:"),
                     
                     div( class = "grouph2h",
                          checkboxGroupInput(
                            NS(id,"choose_stats_h2h"), label = "",
                            choices = list(
                              "POINTS H2H" = "POINTS_H2H",
                              "GOALS SCORED H2H" = "GOALS_SCORED_H2H",
                              "GOALS CONCEDED H2H" = "GOALS_CONCEDED_H2H"
                            ), 
                            selected = c("POINTS_H2H")
                          )
                     ),
                     
                     hr(),
                     
                     h4("Home/Away Specific Stats:"),
                     div( class = "groupha",
                          checkboxGroupInput(
                            NS(id,"choose_stats_ha"), label = "",
                            choices = list(
                              "POINTS HA" = "POINTS_HA",
                              "GOALS SCORED HA" = "GOALS_SCORED_HA",
                              "GOALS CONCEDED HA" = "GOALS_CONCEDED_HA"
                            ), 
                            selected = c("POINTS_HA")
                          )
                     ),
                     
              )
              
            ),
            
            fluidRow(
              column(10, offset = 1,
                     match_data_row_UI(NS(id,"in_main_page")),
              )
            ),
            
            
            fluidRow(
              column(10, offset = 1,  
                     
                     class = "rounded-column",
                     style = "background: #98AFD3; display: flex; flex-direction: column; justify-content: flex-end; height: 100%; margin-top: 0px; margin-bottom: 0px;",
                     fluidRow(
                       column(1, align = "center",
                              h6("Analyze Statistic:"),
                       ),
                       
                       column(width = 2, offset = 0, align = "left",
                              column(10, offset = 2, 
                                     h6("Number of games considered:"),
                              )
                       ),
                       column(width = 6, offset = 0, align = "center",
                              div(style = "height: 10px;"),
                              h4("Statistic:"),
                              
                       ),
                       column(width = 3, offset =  0, align = "left",
                              column(6, offset = 2,
                                     div(style = "height: 12px;"),
                                     h6("Weight of stat:"),
                                     
                              ),
                       ),
                       
                       
                     ) 
              )),
            
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Make the stat rows: ----
            
            
            
            estimation_page_stats_numeric_UI(NS(id,"POINTS")),
            estimation_page_stats_numeric_UI(NS(id,"GOALS_SCORED"),
                                             header = "GOALS SCORED"),
            estimation_page_stats_numeric_UI(NS(id,"GOALS_CONCEDED"),
                                             header = "GOALS CONCEDED"),
            
            estimation_page_stats_numeric_UI(NS(id,"SHOTS_ON_TARGET"),
                                             header = "SHOTS ON TARGET"),
            estimation_page_stats_numeric_UI(NS(id,"SHOTS_ON_TARGET_CONCEDED"),
                                             header = "SHOTS ON TARGET \n CONCEDED"),
            
            estimation_page_stats_numeric_UI(NS(id,"POINTS_H2H"), h2h = T, home_away = F,
                                             header = "POINTS H2H"),
            estimation_page_stats_numeric_UI(NS(id,"GOALS_SCORED_H2H"), h2h = T, home_away = F,
                                             header = "GOALS SCORED H2H"),
            estimation_page_stats_numeric_UI(NS(id,"GOALS_CONCEDED_H2H"), h2h = T, home_away = F,
                                             header = "GOALS CONCEDED H2H"),
            
            estimation_page_stats_numeric_UI(NS(id,"POINTS_HA"), h2h = F, home_away = T,
                                             header = "POINTS HA"),
            estimation_page_stats_numeric_UI(NS(id,"GOALS_SCORED_HA"), h2h = F, home_away = T,
                                             header = "GOALS SCORED HA"),
            estimation_page_stats_numeric_UI(NS(id,"GOALS_CONCEDED_HA"), h2h = F, home_away = T,
                                             header = "GOALS CONCEDED HA")
            
            
            
          )
        ) # MAIN PAGE
    )  # SIDEBAR PAGE
    # ~~~~~~~~~~~
  )
}


estimation_page_info_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent( list(input$choose_stats, input$choose_stats_h2h, input$choose_stats_ha), {
      
      r6$chosen_stats_list <- c(input$choose_stats, input$choose_stats_h2h, input$choose_stats_ha)
      
      # Identify added items
      added_items <- setdiff(r6$chosen_stats_list, r6$chosen_stats_list_prev)
      removed_items <- setdiff(r6$chosen_stats_list_prev, r6$chosen_stats_list)
      
      r6$chosen_stats_list_changed <-  c(added_items, removed_items)
      # print(r6$chosen_stats_list_changed)
      trigger("new_stats_chosen")
      
      r6$chosen_stats_list_prev <- r6$chosen_stats_list
      
    }, ignoreNULL = FALSE)
    
    
    card_for_match_details_Server("card_module", r6)
    match_data_row_Server("in_main_page", r6)
    
    
    estimation_page_stats_numeric_Server("POINTS", r6, metric_name = "Points_lx", positive_metric = T)
    estimation_page_stats_numeric_Server("GOALS_SCORED", r6, metric_name = "Goals_Scored_lx", positive_metric = T,
                                         header = "GOALS SCORED")
    estimation_page_stats_numeric_Server("GOALS_CONCEDED", r6, metric_name = "Goals_Conceded_lx", positive_metric = F,
                                         header = "GOALS CONCEDED")
    
    
    estimation_page_stats_numeric_Server("SHOTS_ON_TARGET", r6, metric_name = "Shots_OT_For_lx", positive_metric = T,
                                         header = "SHOTS ON TARGET")
    estimation_page_stats_numeric_Server("SHOTS_ON_TARGET_CONCEDED", r6, metric_name = "Shots_OT_Conceded_lx", positive_metric = F,
                                         header = "SHOTS ON TARGET \n CONCEDED")
    # 
    
    estimation_page_stats_numeric_Server("POINTS_H2H", r6, metric_name = "Points_lx", positive_metric = T, h2h = T, home_away=F,
                                         header = "POINTS H2H")
    estimation_page_stats_numeric_Server("GOALS_SCORED_H2H", r6, metric_name = "Goals_Scored_lx", positive_metric = T, h2h = T, home_away=F,
                                         header = "GOALS SCORED H2H")
    estimation_page_stats_numeric_Server("GOALS_CONCEDED_H2H", r6, metric_name = "Goals_Conceded_lx", positive_metric = F, h2h = T, home_away=F,
                                         header = "GOALS CONCEDED H2H")
    
    estimation_page_stats_numeric_Server("POINTS_HA", r6, metric_name = "Points_lx", positive_metric = T, h2h = F, home_away=T,
                                         header = "POINTS HA")
    estimation_page_stats_numeric_Server("GOALS_SCORED_HA", r6, metric_name = "Goals_Scored_lx", positive_metric = T, h2h = F, home_away=T,
                                         header = "GOALS SCORED HA")
    estimation_page_stats_numeric_Server("GOALS_CONCEDED_HA", r6, metric_name = "Goals_Conceded_lx", positive_metric = F, h2h = F, home_away=T,
                                         header = "GOALS CONCEDED HA")
    
    
    
    
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}
