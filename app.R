# Sys.setenv(LANG = "en")

# Load necessary libraries
library(rsconnect)
library(bslib)

library(shiny)
library(gargoyle)
library(tidyverse)
library(purrr)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(glue)
library(shinyWidgets)
library(toOrdinal)
library(showtext)
library(sysfonts)

library(RPostgres)
library(bcrypt)
library(uuid)

# library(cowplot)

library(reactable)
library(reactable.extras)
library(reactablefmtr)

library(bsicons)

library(plotly)





source("shh.R")

source(("script_folder/functions_server/functions.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source modules
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("script_folder/modules/mod_navigation_buttons.R")

# home page:
source("script_folder/modules/mod_home_page.R")

# how it works page:
source("script_folder/modules/mod_how_it_works.R")

# selection page:
source("script_folder/modules/mod_selection_screen.R")

# estimation page:
source("script_folder/modules/mod_estimation_info_2.R")

# the data page:
source("script_folder/modules/mod_the_data.R")
source("script_folder/modules/mod_methodology.R")
source("script_folder/modules/mod_authors.R")

# estimation page:
source("script_folder/modules/mod_log_in_or_create.R")



sysfonts::font_add( family = "Ahronbdgg", regular = "www/ahronbd.ttf")
showtext::showtext_auto()



# Define UI function
ui <- function(request) {
  
  fluidPage(
    
    # Hidden div to help simulate focus release
    tags$div(id = "hidden_fullscreen_toggle", style = "display: none;"),
    
    title = "FixtureIO",
    
    theme = bs_theme(version = 5),  # Set the Bootstrap theme globally
    useShinyjs(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
      tags$link(rel = "icon", type = "image/png", href = "fav_icon.png"),
      tags$script(src = "customtwo.js?v=1.0") # Increment the version (v=1.0) when updating
    ),
    
    
    # Container for positioning image and header
    div(
      class = "container",
      # Image element with the class 'top-image' for styling
      tags$img(src = "Logo1.jpg", class = "top-image", id = "logo_img", width = "250px"),  # Adjust the path to your image
      
      # H1 element to the right of the image
      tags$h1("PREDICTIONS YOU BELIEVE IN", class = "header-right", style = "color: white !important;")
    ),
    
    
    navset_underline(
      id = "navbar",
      
      
      # HOME PANEL
      nav_panel(
        title = icon("home"),
        value = "HOME",
        notification_bar_UI(),
        home_page_UI("home_page") # Include module content
        
        
      ),
      
      # NAV PANEL ONE
      nav_panel(
        title = "YOUR ANALYSIS",
        value = "ANALYSIS",
        
        # Analytics page
        navbarPage(
          useShinyjs(),
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
            tags$script(src = "customtwo.js?v=1.0") # Increment the version (v=1.0) when updating
          ),
          
          tabsetPanel(
            id = "tabset",
            type = "hidden",  # Hides the tab headers
            
            # TAB 1
            tabPanel(
              "Matchup Select",
              reactable_extras_dependency(),
              notification_bar_UI(),
              team_select_UI("ui_on_selection_tab"),
              
            ),
            
            # TAB 2:
            tabPanel(
              "Estimation",
              notification_bar_UI(),
              estimation_page_info_UI("top_of_estimation_tab"),
            ),
          )
        )
      ), # NAV PANEL ANALYSIS
      
      nav_panel(
        title = "HOW IT WORKS",
        value = "HOW-IT-WORKS",
        notification_bar_UI(),
        how_it_works_UI("how_it_works")
      ),
      nav_menu(
        title = icon("bars"),
        value = "ABOUT",  # This creates a dropdown menu
        nav_panel("METHODOLOGY",value = "ABOUT/METHODOLOGY",  
                  notification_bar_UI(),
                  br(),
                  methodology_UI("methodology")),
        
        nav_panel("THE DATA", value = "ABOUT/THE-DATA", 
                  notification_bar_UI(),
                  br(), 
                  the_data_UI("the_data")),
        
        nav_panel("AUTHORS", value = "ABOUT/AUTHORS",  
                  notification_bar_UI(),
                  br(),
                  authors_UI("authors"))
      ),
      
      nav_panel(
        title = icon("user"),
        value = "USER_PAGE",
        
        # user
        navbarPage(
          useShinyjs(),
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
            tags$script(src = "customtwo.js?v=1.0") # Increment the version (v=1.0) when updating
          ),
          
          tabsetPanel(
            id = "tabset_user",
            type = "hidden",  # Hides the tab headers
            
            # TAB 1
            tabPanel(
              "Log_in",
              value = "LOG-IN",
              notification_bar_UI(),
              login_UI("login_page"),
              
            ),
            
            # TAB 2:
            # tabPanel(
            #   "User_page",
            #   value = "USER-INFO",
            #   notification_bar_UI(),
            #   user_page_UI("user_page"),
            #   
            # ),
          )
        )
        
      ),
      
      
    ),# NAVSET
    
    
    
    
    
    # Footer section
    tags$footer(
      style = "background-color: #333; color: white; padding: 15px; text-align: center; font-size: 0.9em;",
      
      # Link to Privacy Policy
      tags$a(href = "privacy-policy.html", "Privacy Policy", style = "color: white; margin-right: 20px;"),
      
      # Link to Terms & Conditions
      tags$a(href = "terms-and-conditions.html", "Terms & Conditions", style = "color: white; margin-right: 20px;"),
      
      # Copyright Notice
      tags$span(paste0("© ", format(Sys.Date(), "%Y"), ". All rights reserved."), style = "color: white;")
    )
  ) # first fluid page
}

# Define server function
server <- function(input, output, session) {
  
  # Observe changes to the current tab to update URL and active tab
  observeEvent(input$current_tab, {
    # Ensure valid tab is selected and update the URL accordingly
    if (!is.null(input$current_tab) && input$current_tab %in% c("HOME", "ANALYSIS", "HOW-IT-WORKS", "ABOUT/METHODOLOGY", "ABOUT/THE-DATA", "ABOUT/AUTHORS", "LOG-IN")) {
      # Update URL without reloading the page
      new_url <- input$current_tab
      session$sendCustomMessage(type = 'updateURL', message = new_url)
      
      # Update the active tab to match the URL using nav_select
      nav_select("navbar", input$current_tab)
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)  # Ensure all changes are observed
  
  
  # INIT r6
  r6 = R6::R6Class()
  
  r6$user_info$logged_in <- FALSE
  r6$user_info$user_id <- NULL
  r6$user_info$username <- NULL
  r6$user_info$bets <- NULL
  r6$user_info$bets_week_starting <- NULL
  
  
  r6$data$pl_historic <- raw_pl_data_all
  # r6$data$ll_historic <- read.csv("Data/SP1.csv") %>% as_tibble()
  
  r6$data$pl_schedule <- pl_schedule
  # r6$data$ll_schedule <- ll_schedule
  
  r6$team_list$pl_teams <- pl_teams
  # r6$team_list$ll_teams <- ll_teams
  
  r6$odds$pl <- pl_enriched_schedule
  
  
  r6$standings$`English Premier League` <- pl_standings_table
  r6$chosen_stats_list_prev <- c()
  
  
  
  
  # Initialize triggers
  init("test")
  init("button_to_estimation", "button_to_selection", "button_select_matchup")
  init("button_app", "button_HTU", "button_methodology", "button_data", "button_authors")
  init("teams_selected_from_table")
  init("update_main_prediction")
  init("new_stats_chosen")
  init("full_screen_card_closed")
  init("open_analytics_card_home", "open_analytics_card", "open_analytics_card_away")
  init("user_logged_in", "user_logged_out")
  init("divergent_click", "compare_btn_click")
  init("set_odds_and_update_pie")
  
  # Read modules
  button_to_estimation_Server("button_on_selection_tab")
  button_to_selection_Server("button_on_estimation_tab")
  team_select_Server("ui_on_selection_tab", r6)
  estimation_page_info_Server("top_of_estimation_tab", r6)
  home_page_Server("home_page", r6)
  how_it_works_Server("how_it_works")
  the_data_Server("the_data")
  methodology_Server("the_data")
  authors_Server("authors")
  login_Server("login_page", r6)
  # user_page_Server("user_page", r6)
  
  # Some code for navigating the app
  observeEvent(watch("button_to_estimation"), ignoreInit = T, {
    updateTabsetPanel(session, "tabset", selected = "Estimation")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  observeEvent(watch("button_to_selection"), ignoreInit = T, {
    updateTabsetPanel(session, "tabset", selected = "Matchup Select")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  observeEvent(watch("user_logged_in"), ignoreInit = T, {
    updateTabsetPanel(session, "tabset_user", selected = "USER-INFO")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  observeEvent(watch("user_logged_out"), ignoreInit = T, {
    updateTabsetPanel(session, "tabset_user", selected = "LOG-IN")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  
  
  
  # navbar navigatoin:
  observeEvent(watch("button_app"), ignoreInit = T, {
    nav_select("navbar", "ANALYSIS")
  })
  observeEvent(watch("button_HTU"), ignoreInit = T, {
    nav_select("navbar", "HOW-IT-WORKS")
  })
  observeEvent(watch("button_methodology"), ignoreInit = T, {
    nav_select("navbar", "ABOUT/METHODOLOGY")
  })
  observeEvent(watch("button_data"), ignoreInit = T, {
    nav_select("navbar", "ABOUT/THE-DATA")
  })
  observeEvent(watch("button_authors"), ignoreInit = T, {
    nav_select("navbar", "ABOUT/AUTHORS")
  })
  
  observeEvent(input$image_clicked, {
    nav_select("navbar", "HOME")
  })
  
  observeEvent(input$full_screen_closed, {
    trigger("full_screen_card_closed")
  })
  
  observeEvent(input$div_clicked_home_team, {
    trigger("open_analytics_card_home")
  })
  observeEvent(input$div_clicked_away_team, {
    trigger("open_analytics_card_away")
  })
  observeEvent(input$div_clicked_result_col, {
    trigger("open_analytics_card")
  })
  
  observeEvent(input$go_to_login_page, {
    nav_select("navbar", "USER_PAGE")
  })
  
  observeEvent(input$div_clicked_divergent, ignoreInit =  T, {
    
    str_without_number <- strsplit(input$div_clicked_divergent, "--")[[1]][1]
    id_clicked <- strsplit(str_without_number, "-")[[1]][2]
    
    r6$stat_modal_id <- id_clicked
    trigger("divergent_click")
  })
  
  observeEvent(input$feedback_clicked, ignoreInit = T, {
    nav_select("navbar", "ABOUT/AUTHORS")
  })
  
  # Handle betting link clicks
  observeEvent(input$betting_clicked, ignoreInit = TRUE, {
    showModal(
      tagList(
        tags$style(HTML("
        .modal-dialog {
          width: 50vw !important;
          max-width: 80vw !important;
        }
      ")),
      modalDialog(
        title = "Exciting New Betting Feature!",
        div(
          tags$p("Get ready for our exciting new betting feature:", style = "font-weight: 450;"),
          tags$ul(
            tags$li(
              tags$span(
                "Weekly Free Bets: ", 
                style = "font-weight: 750;"
              ),
              tags$span(
                "Receive free virtual bets every Monday to wager on upcoming matches.", 
                style = "font-weight: 500;"
              )
            ),
            tags$li(
              tags$span(
                "Community-Driven Odds: ", 
                style = "font-weight: 750;"
              ),
              tags$span(
                "Watch odds update continuously based on the community's bets—you're not just predicting outcomes, you're competing with fellow fans!", 
                style = "font-weight: 500;"
              )
            ),
            tags$li(
              tags$span(
                "Join the Competition: ", 
                style = "font-weight: 750;"
              ),
              tags$span(
                "Sharpen your prediction skills, strategize your bets, and see how you stack up against others.", 
                style = "font-weight: 500;"
              )
            )
          ),
          tags$p("Stay tuned—this is just the beginning of a more immersive and interactive football experience!", style = "font-weight: 6000;")
        ),
        br(),
        hr(),
        h5("If you want early access to the betting feature:"),
        textInput("signup_name", "Name", placeholder = "Enter your name"),
        textInput("signup_email", "Email", placeholder = "Enter your email"),
        actionButton("submit_signup", "Submit", class = "btn-success"),
        
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
    )
  })
  
  
  # Handle signup submission
  observeEvent(input$submit_signup, {
    name <- input$signup_name
    email <- input$signup_email
    
    # Validate inputs
    if (is.null(name) || name == "" || is.null(email) || email == "") {
      showNotification("Please provide both name and email.", type = "error")
      return()
    }
    
    # Write signup to the database
    tryCatch({
      write_signup_to_db(name, email)
      showNotification("Thank you for signing up!", type = "message")
      removeModal() # Close the modal upon successful submission
    }, error = function(e) {
      showNotification("An error occurred: Could not save your signup.", type = "error")
    })
  })
  
  
  
}

# Run the Shiny app
shinyApp(ui, server)


# deployApp()

