
if(F){
  metric_name =  "Goals_Scored_lx"
}


estimation_page_stats_numeric_UI <- function(id, header = "DEFAULT_HEADER", h2h = F, home_away = F) {
  
  
  if(header == "DEFAULT_HEADER"){
    header = id
  }
  hidden(
    div(
      id = NS(id, "hide_all"),
      
      hidden(
        div(
          id = NS(id, "hide_stat"),  # Attach the id here for shinyjs::show/hide functionality
          fluidRow(
            
            
            style = "margin-top: -15px; margin-bottom: -15px; padding-top = 0px;",
            
            column(10, offset = 1,
                   
                   
                   class = "rounded-column",
                   style = if(h2h) {
                     "h2h-gradient"
                   } else if(home_away){
                     "ha-gradient"
                   } else {
                     "basic-gradient"
                   },
                   
                   
                   class = "rounded-column",
                   class = if(h2h) {
                     "rounded-column h2h-gradient"
                   } else if(home_away){
                     "rounded-column ha-gradient"
                   } else {
                     "rounded-column basic-gradient"
                   },
                   
                   fluidRow(
                     column(1,
                            
                            align = "center",
                            div(style = "height: 20px;"),
                            if(h2h){} else{
                              div(style="display:inline-block;text-align: center;",
                                  actionButton(NS(id,"compare_btn"), label = icon("chart-line"), width = "60px",style = "height: 35px",class = "reactable-button" ))
                            }
                     ),
                     
                     column(width = 2, offset = 0, align = "left",
                            column(10, offset = 2,
                                   
                                   
                                   # radioButtons(NS(id,"n_matches2"), label = "",
                                   #              choices = c("123" = 1, "13132" = 2,"123123123" = 3),
                                   #              selected = 1),
                                   # actionBttn(inputId = NS(id,"btn"), label = "b"),
                                   
                                   
                                   
                                   
                                   div(
                                     style = "margin-bottom: 0px;",  # adjust margin
                                     
                                     if(h2h | home_away ){
                                       radioButtons(NS(id,"n_matches"), label = "",
                                                    choices = list("Latest 3 Games" = "3", "Latest 5 Games" = "5"),
                                                    selected = "3")
                                     }
                                     else{
                                       radioButtons(NS(id,"n_matches"), label = "",
                                                    choices = list("Latest 5 Games" = 5, "Latest 10 Games" = 10),
                                                    selected = 5)
                                     }
                                     
                                   )
                            )
                     ),
                     
                     column(width = 6, offset = 0, align = "center",
                            column(width = 12,align = "center",
                                   div(style = "height: 10px;"),
                                   plotOutput(NS(id,"ggplot_divergent"), width = "80%", height = "70px")
                            ),
                            
                     ),
                     column(width = 3, offset =  0, align = "left",
                            column(6, offset = 2,
                                   div(style = "height: 20px;"),
                                   sliderTextInput(NS(id,"Slider"),choices = paste0(seq(0,200,25), "%"), label = NULL, selected = "100%", width = "120%"),
                            ),
                     ),
                   ),
                   
            )
          )
        )
      ),
      
      
      hidden(
        div(
          id = NS(id, "not_enough_data"),  # Set the ID here on a simpler container (div)
          fluidRow(
            style = "margin-top: -15px; margin-bottom: -15px; padding-top: 0px;",  # Adjust as needed
            column(10, offset = 1, align = "center",
                   class = "rounded-column", style = "background: linear-gradient(to bottom, #f3f6f8, #ffb3b3); padding-top: 0px;",
                   fluidRow(
                     column(3, id = NS(id,"set_n_games_row"),
                            actionButton(NS(id,"games_3_btn"), label = textOutput(NS(id,"lower_n_games_text")), width = "130px",class = "reactable-button" )
                     ),
                     column(6,
                            div(style = "height: 6px;"),
                            h6("Not enough data for:"),
                            h6(textOutput(NS(id,"not_enough_data_text"))),
                     )
                   )
            )
          )
        )
      )
      
    )
  )
  
}



estimation_page_stats_numeric_Server <- function(id, r6, 
                                                 metric_name =  "NO METIRC",
                                                 positive_metric = T,
                                                 header = "DEFAULT",
                                                 h2h = F,
                                                 home_away = F) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(input$btn, ignoreInit =  T, {
      
      print("123")
      
      updateRadioButtons(
        inputId = "n_matches2",
        selected = 1
      )
      
      print(input$n_matches2)
      
    })
    
    # 
    # 
    # 
    # 
    # if (header == "DEFAULT") {
    #   header <- id
    # }
    # # init lis. Will be overwritten
    # l_perc_results <- list()
    # 
    # previous_n_matches <- reactiveVal(NULL)
    # previous_button_to_estimation <- reactiveVal(1)
    # 
    # observe({
    #   previous_n_matches <- reactiveVal(input$n_matches)
    #   # previous_button_to_estimation <- reactiveVal(watch("button_to_estimation"))
    #   
    # })
    # 
    # 
    # 
    # observeEvent(input$games_3_btn, ignoreInit = T, {
    #   
    #   
    #   if(h2h){
    #     
    #     print(session)
    #     
    #     updateRadioButtons(
    #       session = session,
    #       inputId = session$ns("n_matches2"),
    #       choices = list("Option 1" = "1", "Option 2" = "2"),
    #       selected = "1"
    #     )
    #     
    #     
    #     
    #     trigger("test")
    #     
    #     print(input$n_matches2)
    #     print("h2h")
    #     trigger("new_stats_chosen")
    #     shinyjs::show("hide_stat")
    #     
    #     
    #     
    #   } else if(home_away){
    #     # updateRadioButtons(inputId = "n_matches", selected = "Latest 3 Games")
    #     # print("ha")
    #   }else {
    #     # updateRadioButtons(inputId = "n_matches", selected = "Latest 5 Games")
    #     # print("basic")
    #   }
    #   
    # })
    # 
    # 
    # # if(h2h | home_away){
    # #   output$lower_n_games_text <- renderText("Set #games to 3")
    # # }else {
    # #   output$lower_n_games_text <- renderText("Set #games to 5")
    # # }
    # 
    # output$lower_n_games_text <- renderText("Set #games to 3")
    # 
    # 
    # # SERVER:
    # observeEvent(list(input$n_matches,watch("button_to_estimation"), watch("new_stats_chosen")),ignoreInit = T, {
    #   
    #   # print(input$n_matches)
    #   
    #   if (id %in% r6$chosen_stats_list_changed) {
    #     added_or_removed <- reactiveVal(TRUE)
    #   } else {
    #     added_or_removed <- reactiveVal(FALSE)
    #   }
    #   
    #   if(! id %in% r6$chosen_stats_list) {
    #     # If stat not chosen
    #     
    #     if(added_or_removed()){
    #       
    #       shinyjs::hide("hide_all")
    #       
    #       r6$metrics[[id]]$weight <- "0%"
    #       r6$metrics[[id]]$home_team_perc <-0
    #       r6$metrics[[id]]$away_team_perc <- 0
    #       r6$metrics[[id]]$draw_team_perc <- 0
    #       trigger("update_main_prediction")
    #     }
    #     
    #   } else{
    #     
    #     # Check for h2h that we have enough data:
    #     
    #     # calculate all metrics
    #     n_matches = input$n_matches %>% as.numeric()
    #     
    #     if(h2h){
    #       h2h_teams = c(r6$selected_home_team, r6$selected_away_team)
    #     } else{ h2h_teams = "SKIP" }
    #     
    #     metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches, h2h_teams = h2h_teams, home_away = home_away) %>%
    #       filter(metric == metric_name)
    #     
    #     # filter to metriC and hometeam
    #     
    #     if(home_away){
    #       home_team_matches  = metrics_df %>%
    #         filter(
    #           Team==r6$selected_home_team,
    #           home_away == "home"
    #         ) %>% pull(n_matches_in_sample)
    #       
    #       # filter to metriC and awayteam
    #       away_team_matches  = metrics_df %>%
    #         filter(
    #           Team==r6$selected_away_team,
    #           home_away == "away"
    #         ) %>% pull(n_matches_in_sample)
    #       
    #     } else{
    #       home_team_matches  = metrics_df %>%
    #         filter(
    #           Team==r6$selected_home_team
    #         ) %>% pull(n_matches_in_sample)
    #       
    #       # filter to metriC and awayteam
    #       away_team_matches  = metrics_df %>%
    #         filter(
    #           Team==r6$selected_away_team
    #         ) %>% pull(n_matches_in_sample)
    #       
    #     }
    #     
    #     if(length(away_team_matches) == 0){
    #       away_team_matches = 0
    #     }
    #     if(length(home_team_matches) == 0){
    #       away_team_matches = 0
    #     }
    #     
    #     if((away_team_matches) < n_matches || (home_team_matches) < n_matches) {
    #       # NOT ENOUGH MATCHES
    #       
    #       shinyjs::show("hide_all")
    #       shinyjs::hide("hide_stat")
    #       shinyjs::show("not_enough_data")
    #       
    #       if(h2h | home_away){
    #         if (n_matches == 5){
    #           shinyjs::show("set_n_games_row")
    #         }
    #       }else if (n_matches==10) {
    #         shinyjs::show("set_n_games_row")
    #       }
    #       
    #       output$not_enough_data_text = renderText(glue('"{header}"'))
    #       
    #       r6$metrics[[id]]$weight <- "0%"
    #       r6$metrics[[id]]$home_team_perc <-0
    #       r6$metrics[[id]]$away_team_perc <- 0
    #       r6$metrics[[id]]$draw_team_perc <- 0
    #       trigger("update_main_prediction")
    #       
    #       
    #       
    #       
    #     } else {
    #       # STAT TO BE INCLUDED:
    #       
    #       shinyjs::show("hide_all")
    #       shinyjs::show("hide_stat")
    #       shinyjs::hide("not_enough_data")
    #       
    #       # make sure we need to do all re calculations:
    #       
    #       # print(gargoyle::watch("button_to_estimation"))
    #       # print(previous_button_to_estimation())
    #       
    #       # print( "__________________________________________")
    #       
    #       if(gargoyle::watch("button_to_estimation") != previous_button_to_estimation() || added_or_removed()==TRUE || input$n_matches != isolate(previous_n_matches())) {
    #         
    #         # print(id)
    #         # 
    #         # if(gargoyle::watch("button_to_estimation") != previous_button_to_estimation()){
    #         #   print("A")
    #         # } else if (added_or_removed()==TRUE){
    #         #   print ("B")
    #         # } else if(input$n_matches != isolate(previous_n_matches())){
    #         #   print("C")
    #         # }
    #         
    #         # print(id)
    #         
    #         # Make string for header
    #         output$n_matches_string = renderText(glue("Latest {input$n_matches} Matches"))
    #         
    #         # Take n_matches as numeric
    #         n_matches = input$n_matches %>% as.numeric()
    #         
    #         if(h2h){
    #           h2h_teams = c(r6$selected_home_team, r6$selected_away_team)
    #         } else{ h2h_teams = "SKIP" }
    #         
    #         # calculate all metrics
    #         metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches, h2h_teams = h2h_teams, home_away= home_away) %>% 
    #           filter(metric == metric_name)
    #         
    #         if(home_away) {
    #           # filter to metriC and hometeam
    #           home_team_df  = metrics_df %>%
    #             filter(
    #               Team==r6$selected_home_team,
    #               home_away == "home"
    #             )
    #           
    #           # filter to metriC and awayteam
    #           away_team_df  = metrics_df %>% 
    #             filter(
    #               Team==r6$selected_away_team,
    #               home_away == "away"
    #             )
    #         } else{
    #           # filter to metriC and hometeam
    #           home_team_df  = metrics_df %>%
    #             filter(
    #               Team==r6$selected_home_team,
    #             )
    #           
    #           # filter to metriC and awayteam
    #           away_team_df  = metrics_df %>% 
    #             filter(
    #               Team==r6$selected_away_team,
    #             )
    #         }
    #         
    #         # ~~~~~~~~~~~~~~~~~~~~
    #         # MAKE GGPLOTS
    #         
    #         # RUN FUNCTION FOR PERCENTAGES + PLOT:
    #         l_perc_results <- f_plot_winning_prediction_percent(Home_rank = home_team_df$rank, 
    #                                                             Away_rank = away_team_df$rank, 
    #                                                             range = (metrics_df$rank %>% max - metrics_df$rank %>% min), 
    #                                                             Home_label = r6$selected_home_team_short,
    #                                                             Away_label = r6$selected_home_away_short,
    #                                                             return_plot = F
    #                                                             
    #         )
    #         
    #         # MAIN PLOT:
    #         
    #         output$ggplot_divergent = renderPlot({
    #           f_plot_divergent_bar(metrics_df = metrics_df,
    #                                home_team = r6$selected_home_team, 
    #                                away_team = r6$selected_away_team,
    #                                title_stat = header,
    #                                home_away = home_away
    #           )
    #         }, bg="transparent")
    #         
    #         
    #         r6$metrics[[id]]$weight <- input$Slider
    #         r6$metrics[[id]]$home_team_perc <- l_perc_results$home_team_perc
    #         r6$metrics[[id]]$away_team_perc <- l_perc_results$away_team_perc
    #         r6$metrics[[id]]$draw_team_perc <- 100 - l_perc_results$away_team_perc - l_perc_results$home_team_perc
    #         trigger("update_main_prediction")
    #         
    #         
    #         previous_n_matches(input$n_matches)
    #         previous_button_to_estimation(watch("button_to_estimation"))
    #         
    #       }
    #     }
    #   }
    #   
    #   
    #   
    # })
    # 
    # observeEvent(input$Slider, ignoreInit = T,{
    #   r6$metrics[[id]]$weight <- input$Slider
    #   trigger("update_main_prediction")
    # })
    # 
    # # modal:
    # observeEvent( input$compare_btn, {
    #   
    #   # Take n_matches as numeric
    #   n_matches = input$n_matches %>% as.numeric()
    #   
    #   if(h2h){
    #     h2h_teams = c(r6$selected_home_team, r6$selected_away_team)
    #   } else{ h2h_teams = "SKIP" }
    #   
    #   # calculate all metrics
    #   metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches, h2h_teams = h2h_teams, home_away = home_away) %>% 
    #     filter(metric == metric_name)
    #   
    #   
    #   if(home_away){
    #     home_team_df  = metrics_df %>%
    #       filter(
    #         Team==r6$selected_home_team,
    #         home_away == "home"
    #       )
    #     
    #     
    #     # filter to metriC and awayteam
    #     away_team_df  = metrics_df %>% 
    #       filter(
    #         Team==r6$selected_away_team,
    #         home_away == "away"
    #       )
    #     
    #     
    #     output$ggplot_bar_home = renderPlot({
    #       f_plot_ranks(metrics_df = metrics_df,
    #                    home_team = r6$selected_home_team,
    #                    away_team = r6$selected_away_team,
    #                    positive_metric_plot = positive_metric,
    #                    home_away_value = "home"
    #       )
    #     }, bg="transparent")
    #     
    #     output$ggplot_bar_away = renderPlot({
    #       f_plot_ranks(metrics_df = metrics_df,
    #                    home_team = r6$selected_home_team,
    #                    away_team = r6$selected_away_team,
    #                    positive_metric_plot = positive_metric,
    #                    home_away_value = "away"
    #       )
    #     }, bg="transparent")
    #     
    #     
    #   } else{
    #     home_team_df  = metrics_df %>%
    #       filter(
    #         Team==r6$selected_home_team,
    #       )
    #     
    #     # filter to metriC and awayteam
    #     away_team_df  = metrics_df %>% 
    #       filter(
    #         Team==r6$selected_away_team,
    #       )
    #     
    #     output$ggplot_bar = renderPlot({
    #       f_plot_ranks(metrics_df = metrics_df,
    #                    home_team = r6$selected_home_team,
    #                    away_team = r6$selected_away_team,
    #                    positive_metric_plot = positive_metric
    #       )
    #     }, bg="transparent")
    #     
    #   }
    #   
    #   # RUN FUNCTION FOR PERCENTAGES + PLOT:
    #   l_perc_results <- f_plot_winning_prediction_percent(Home_rank = home_team_df$rank, 
    #                                                       Away_rank = away_team_df$rank, 
    #                                                       range = (metrics_df$rank %>% max - metrics_df$rank %>% min), 
    #                                                       Home_label = r6$selected_home_team_short, 
    #                                                       Away_label = r6$selected_away_team_short
    #                                                       
    #   )
    #   
    #   output$ggplot_percent = renderPlot({
    #     l_perc_results$plot
    #   }, bg="transparent")
    #   
    #   
    #   output$logo_ht <- renderUI({
    #     img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "80%", width = "80%")
    #   })
    #   output$logo_at <- renderUI({
    #     img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "80%", width = "80%")
    #   })
    #   
    #   showModal(
    #     
    #     tagList(
    #       tags$style(HTML("
    #     .modal-dialog {
    #       width: 50vw !important;
    #       max-width: 80vw !important;
    #     }
    #   ")),
    #       
    #       modalDialog(
    #         title = paste0("Details for  ", header),
    #         
    #         if(home_away){
    #           column(width = 12, align = "center",
    #                  h5(paste0(header, "  at HOME compared to league at HOME:")),
    #                  fluidRow(
    #                    column(2, align = "right",
    #                           uiOutput(session$ns("logo_ht"), width = "65%")
    #                    ),
    #                    column(10,
    #                           plotOutput(session$ns("ggplot_bar_home"), width = "80%", height = "250px"),
    #                    )
    #                  ),
    #                  br(),
    #                  h5(paste0(header, "  AWAY compared to league at AWAY:")),
    #                  fluidRow(
    #                    column(2, align = "right",
    #                           uiOutput(session$ns("logo_at"), width = "65%")
    #                    ),
    #                    column(10,
    #                           plotOutput(session$ns("ggplot_bar_away"), width = "80%", height = "250px"),
    #                    )
    #                  ),
    #                  
    #                  
    #           )
    #         }else {
    #           
    #           column(width = 12, align = "center",
    #                  h5(paste0("Comparison of ", header, " to league:")),
    #                  plotOutput(session$ns("ggplot_bar"), width = "80%", height = "300px")
    #           )
    #         },
    #         
    #         hr(),
    #         column(width = 12, align = "center",
    #                h5(paste0("Contribution of ", header, " only:")),
    #                plotOutput(session$ns("ggplot_percent"), width = "65%", height = "125px"),
    #         ),
    #         easyClose = TRUE,  # Allows closing the modal by clicking outside
    #         footer = tagList(
    #           modalButton("Close"),  # A button to close the modal
    #         ),
    #         # size = "l"
    #         
    #       ))
    #   )
    #   
    # }, ignoreInit = T
    # ) 
    
    
    
    
    # }
    
    # }) # observe on output trigger
    
  }) # module
}
