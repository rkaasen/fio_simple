

notification_bar_UI <- function() {
  div(
    class = "notification-bar",
    "App is in testing. Please leave feedback ",
    tags$a(
      href = "#",  # Prevent navigation
      class = "feedback-link notification-link",  # Use a specific class for feedback
      "here. "
    ),
    "This is a limited version of the app.",
    tags$a(
      href = "#",  # Prevent navigation
      class = "betting-link notification-link",  # Use a specific class for betting
      "Betting Features "
    ),
    "launching soon."
  )
}


write_feedback_to_db <- function(name, email, feedback) {
  # Establish the database connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  print("after_con")
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  # SQL query for parameterized insertion
  sql <- "
    INSERT INTO feedback (name, email, feedback, created_at)
    VALUES ($1, $2, $3, NOW())
  "
  
  # Execute the query with parameters
  tryCatch({
    dbExecute(con, sql, params = list(name, email, feedback))
    print("Feedback added successfully!")
  }, error = function(e) {
    print(paste("Error adding feedback:", e$message))
  })
}

write_signup_to_db <- function(name, email) {
  # Establish a database connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure the connection is closed at the end of the function
  on.exit(dbDisconnect(con), add = TRUE)
  
  # SQL query for parameterized insertion
  sql <- "
    INSERT INTO early_access_signups (name, email)
    VALUES ($1, $2)
  "
  
  # Execute the query with parameters
  tryCatch({
    dbExecute(con, sql, params = list(name, email))
    message("Signup added successfully!")
  }, error = function(e) {
    message(paste("Error adding signup:", e$message))
  })
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INTERNAL FUNCTIONS ----


# Team List

f_team_list  <-  function(data) {
  
  team_list <- rbind(data %>% select(team = AwayTeam), 
                     data %>%  select(team = HomeTeam)
  ) %>% 
    arrange(team) %>% 
    unique() %>% 
    pull
  
  return(team_list)
  
}


f_select_columns_historcial_data <- function(df){
  
  df_out <-  
    cbind(
      df[,1] %>% as_tibble() %>% set_names("Div")
      ,
      df %>% select(
        Date, Time, HomeTeam, AwayTeam, 
        FTHG, FTAG, FTR, # goals and result
        Referee, # ref
        HS, AS, HST, AST, # shots / on target
        Season_ending_year # Season indicator
      ) %>% 
        
        mutate(
          FTHG = as.numeric(FTHG),
          FTAG = as.numeric(FTAG),
          HS = as.numeric(HS),
          AS = as.numeric(AS),
          HST = as.numeric(HST),
          AST = as.numeric(AST),
          Season_ending_year = as.numeric(Season_ending_year),
        )
    )
  
  return(df_out)
}

f_team_cols_rename <- function(df, Home_Metrics = T) {
  
  if(Home_Metrics == T) {
    
    df_mod <- df %>% 
      select(
        # Overall data
        Team, Opponent, Div, Date, Time,
        
        # Match key-stats (HOME):
        FT_Scored = FTHG, FT_Conceded = FTAG, 
        
        # Match add. stats (HOME):
        Shots_For = HS, Shots_Conceded = AS, Shots_OT_For = HST, Shots_OT_Conceded = AST,
        
        #Ref + season indicator
        Referee = Referee, 
        Season_ending_year = Season_ending_year
      )
    
  } else{
    df_mod <- df %>% 
      select(
        # Overall data
        Team, Opponent, Div, Date, Time,
        
        # Match key-stats (HOME):
        FT_Scored = FTAG, FT_Conceded = FTHG, 
        
        # Match add. stats (HOME):
        Shots_For = AS, Shots_Conceded = HS, Shots_OT_For = AST, Shots_OT_Conceded = HST,
        
        #Ref + season indicator
        Referee = Referee, 
        Season_ending_year = Season_ending_year
      )
  }
  
  # Calculate columns
  df_mod <- df_mod %>% 
    mutate(
      Date = Date %>% as.Date(format = "%d/%m/%Y"),
      Result = ifelse(FT_Scored>FT_Conceded, "W", 
                      ifelse(FT_Scored < FT_Conceded, "L", "D")),
      Points = ifelse(Result == "W", 3,
                      ifelse(Result == "D", 1, 0))
    )
  
}


colMax <- function(X) apply(X, 2, max)
colMin <- function(X) apply(X, 2, min)


f_team_3_letter <- function(league){
  
  if(league == "pl"){
    PL_short <- tribble(
      ~Team, ~Team_short, ~Team_name_from_schedule_data,
      "Arsenal", "ARS", "Arsenal",
      "Aston Villa", "AVA","Aston Villa",
      "Bournemouth", "BOU", "Bournemouth",
      "Brentford", "BRE", "Brentford",
      "Brighton", "BRH", "Brighton",
      "Burnley", "BUR", "Burnley",
      "Chelsea", "CHE", "Chelsea",
      "Crystal Palace", "CRY", "Crystal Palace",
      "Everton", "EVE", "Everton",
      "Fulham","FUL", "Fulham",
      "Ipswich","IPS","Ipswich",
      "Leeds", "LEE", "Leeds",
      "Leicester","LEI","Leicester",
      "Liverpool", "LIV", "Liverpool",
      "Luton", "LUT", "Luton",
      "Man City", "MCI", "Man City",
      "Man United", "MUN", "Man Utd",
      "Newcastle", "NEW", "Newcastle",
      "Norwich", "NOR", "Norwich",
      "Nott'm Forest", "NTG", "Nott'm Forest",
      "Sheffield United", "SHU", "Sheffield Utd",
      "Southampton","SOU","Southampton",
      "Tottenham", "TOT", "Spurs",
      "Watford", "WAT", "Watford",
      "West Ham", "WHU", "West Ham",
      "Wolves", "WLV", "Wolves"
    )
  }
  
  return(PL_short)
  
}



f_calculate_premier_league_table <- function(df) {
  # Create a data frame to store the table with explicit numeric initialization
  teams <- unique(c(df$HomeTeam, df$AwayTeam))
  league_table <- data.frame(
    Team = teams,
    Played = integer(length(teams)),
    Wins = integer(length(teams)),
    Draws = integer(length(teams)),
    Losses = integer(length(teams)),
    GoalsFor = numeric(length(teams)),
    GoalsAgainst = numeric(length(teams)),
    GoalDifference = numeric(length(teams)),
    Points = integer(length(teams)),
    stringsAsFactors = FALSE
  )
  
  # Loop through each match and update the table
  for (i in 1:nrow(df)) {
    home_team <- df$HomeTeam[i]
    away_team <- df$AwayTeam[i]
    home_goals <- df$FTHG[i] %>% as.numeric()
    away_goals <- df$FTAG[i] %>% as.numeric()
    result <- df$FTR[i]
    
    # Update matches played
    league_table$Played[league_table$Team == home_team] <- league_table$Played[league_table$Team == home_team] + 1
    league_table$Played[league_table$Team == away_team] <- league_table$Played[league_table$Team == away_team] + 1
    
    # Update goals for and against
    league_table$GoalsFor[league_table$Team == home_team] <- league_table$GoalsFor[league_table$Team == home_team] + home_goals
    league_table$GoalsAgainst[league_table$Team == home_team] <- league_table$GoalsAgainst[league_table$Team == home_team] + away_goals
    league_table$GoalsFor[league_table$Team == away_team] <- league_table$GoalsFor[league_table$Team == away_team] + away_goals
    league_table$GoalsAgainst[league_table$Team == away_team] <- league_table$GoalsAgainst[league_table$Team == away_team] + home_goals
    
    # Update win, draw, loss, and points based on result
    if (result == "H") {
      league_table$Wins[league_table$Team == home_team] <- league_table$Wins[league_table$Team == home_team] + 1
      league_table$Losses[league_table$Team == away_team] <- league_table$Losses[league_table$Team == away_team] + 1
      league_table$Points[league_table$Team == home_team] <- league_table$Points[league_table$Team == home_team] + 3
    } else if (result == "A") {
      league_table$Wins[league_table$Team == away_team] <- league_table$Wins[league_table$Team == away_team] + 1
      league_table$Losses[league_table$Team == home_team] <- league_table$Losses[league_table$Team == home_team] + 1
      league_table$Points[league_table$Team == away_team] <- league_table$Points[league_table$Team == away_team] + 3
    } else if (result == "D") {
      league_table$Draws[league_table$Team == home_team] <- league_table$Draws[league_table$Team == home_team] + 1
      league_table$Draws[league_table$Team == away_team] <- league_table$Draws[league_table$Team == away_team] + 1
      league_table$Points[league_table$Team == home_team] <- league_table$Points[league_table$Team == home_team] + 1
      league_table$Points[league_table$Team == away_team] <- league_table$Points[league_table$Team == away_team] + 1
    }
  }
  
  # Calculate goal difference
  league_table$GoalDifference <- league_table$GoalsFor - league_table$GoalsAgainst
  
  # Order the table by points, goal difference, and goals scored
  league_table <- league_table[order(-league_table$Points, -league_table$GoalDifference, -league_table$GoalsFor), ]
  
  league_table <- league_table %>% mutate(rank= row_number())
  
  return(league_table)
}

f_calculate_la_liga_table <- function(df) {
  # Create a data frame to store the table with explicit numeric initialization
  teams <- unique(c(df$HomeTeam, df$AwayTeam))
  league_table <- data.frame(
    Team = teams,
    Played = integer(length(teams)),
    Wins = integer(length(teams)),
    Draws = integer(length(teams)),
    Losses = integer(length(teams)),
    GoalsFor = numeric(length(teams)),
    GoalsAgainst = numeric(length(teams)),
    GoalDifference = numeric(length(teams)),
    Points = integer(length(teams)),
    stringsAsFactors = FALSE
  )
  
  # Loop through each match and update the table
  for (i in 1:nrow(df)) {
    home_team <- df$HomeTeam[i]
    away_team <- df$AwayTeam[i]
    home_goals <- df$FTHG[i] %>% as.numeric()
    away_goals <- df$FTAG[i] %>% as.numeric()
    result <- df$FTR[i]
    
    # Update matches played
    league_table$Played[league_table$Team == home_team] <- league_table$Played[league_table$Team == home_team] + 1
    league_table$Played[league_table$Team == away_team] <- league_table$Played[league_table$Team == away_team] + 1
    
    # Update goals for and against
    league_table$GoalsFor[league_table$Team == home_team] <- league_table$GoalsFor[league_table$Team == home_team] + home_goals
    league_table$GoalsAgainst[league_table$Team == home_team] <- league_table$GoalsAgainst[league_table$Team == home_team] + away_goals
    league_table$GoalsFor[league_table$Team == away_team] <- league_table$GoalsFor[league_table$Team == away_team] + away_goals
    league_table$GoalsAgainst[league_table$Team == away_team] <- league_table$GoalsAgainst[league_table$Team == away_team] + home_goals
    
    # Update win, draw, loss, and points based on result
    if (result == "H") {
      league_table$Wins[league_table$Team == home_team] <- league_table$Wins[league_table$Team == home_team] + 1
      league_table$Losses[league_table$Team == away_team] <- league_table$Losses[league_table$Team == away_team] + 1
      league_table$Points[league_table$Team == home_team] <- league_table$Points[league_table$Team == home_team] + 3
    } else if (result == "A") {
      league_table$Wins[league_table$Team == away_team] <- league_table$Wins[league_table$Team == away_team] + 1
      league_table$Losses[league_table$Team == home_team] <- league_table$Losses[league_table$Team == home_team] + 1
      league_table$Points[league_table$Team == away_team] <- league_table$Points[league_table$Team == away_team] + 3
    } else if (result == "D") {
      league_table$Draws[league_table$Team == home_team] <- league_table$Draws[league_table$Team == home_team] + 1
      league_table$Draws[league_table$Team == away_team] <- league_table$Draws[league_table$Team == away_team] + 1
      league_table$Points[league_table$Team == home_team] <- league_table$Points[league_table$Team == home_team] + 1
      league_table$Points[league_table$Team == away_team] <- league_table$Points[league_table$Team == away_team] + 1
    }
  }
  
  # Calculate goal difference
  league_table$GoalDifference <- league_table$GoalsFor - league_table$GoalsAgainst
  
  # Apply tiebreaker rules
  tie_resolve <- function(tied_teams) {
    if (length(tied_teams) <= 1) return(tied_teams)  # No tie or only one team
    
    # Subset the data frame to matches involving only tied teams
    head_to_head_matches <- df[df$HomeTeam %in% tied_teams & df$AwayTeam %in% tied_teams, ]
    
    # Calculate head-to-head points, goals, and goal differential for tied teams
    head_to_head_table <- data.frame(
      Team = tied_teams,
      HeadToHeadPoints = integer(length(tied_teams)),
      HeadToHeadGoalDifference = numeric(length(tied_teams)),
      HeadToHeadGoalsFor = numeric(length(tied_teams)),
      stringsAsFactors = FALSE
    )
    
    # Populate head-to-head statistics
    for (i in 1:nrow(head_to_head_matches)) {
      home_team <- head_to_head_matches$HomeTeam[i]
      away_team <- head_to_head_matches$AwayTeam[i]
      home_goals <- head_to_head_matches$FTHG[i]
      away_goals <- head_to_head_matches$FTAG[i]
      result <- head_to_head_matches$FTR[i]
      
      if (result == "H") {
        head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == home_team] <- head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == home_team] + 3
      } else if (result == "A") {
        head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == away_team] <- head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == away_team] + 3
      } else if (result == "D") {
        head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == home_team] <- head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == home_team] + 1
        head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == away_team] <- head_to_head_table$HeadToHeadPoints[head_to_head_table$Team == away_team] + 1
      }
      
      # Update goals for and goal difference in head-to-head
      head_to_head_table$HeadToHeadGoalsFor[head_to_head_table$Team == home_team] <- head_to_head_table$HeadToHeadGoalsFor[head_to_head_table$Team == home_team] + home_goals
      head_to_head_table$HeadToHeadGoalsFor[head_to_head_table$Team == away_team] <- head_to_head_table$HeadToHeadGoalsFor[head_to_head_table$Team == away_team] + away_goals
      head_to_head_table$HeadToHeadGoalDifference[head_to_head_table$Team == home_team] <- head_to_head_table$HeadToHeadGoalDifference[head_to_head_table$Team == home_team] + (home_goals - away_goals)
      head_to_head_table$HeadToHeadGoalDifference[head_to_head_table$Team == away_team] <- head_to_head_table$HeadToHeadGoalDifference[head_to_head_table$Team == away_team] + (away_goals - home_goals)
    }
    
    # Sort by head-to-head criteria
    head_to_head_table <- head_to_head_table[order(-head_to_head_table$HeadToHeadPoints, -head_to_head_table$HeadToHeadGoalDifference, -head_to_head_table$HeadToHeadGoalsFor), ]
    return(head_to_head_table$Team)
  }
  
  # Rank teams in the table, resolving ties
  league_table <- league_table[order(-league_table$Points, -league_table$GoalDifference, -league_table$GoalsFor), ]
  league_table <- league_table %>% group_by(Points) %>%
    mutate(Rank = tie_resolve(Team)) %>% ungroup() %>%
    arrange(Points %>% desc)
  
  league_table <- league_table %>% mutate(rank= row_number())
  
  return(league_table)
}



f_prepare_schedule_data <- function(df, enriched = F) {
  df_out <- df %>% 
    mutate(
      utc_date = dmy_hm(Date, tz = "UTC"),
      utc_time = paste0(hour(ymd_hms(utc_date)), ":", ifelse(minute(ymd_hms(utc_date))==0, "00", minute(ymd_hms(utc_date)))),
      Date_time_local = with_tz(utc_date, tzone = Sys.timezone()),
      Date_local = Date_time_local %>% as.Date(format = "%d/%m/%Y"),
      Time_local = paste0(hour(ymd_hms(Date_time_local)), ":", ifelse(minute(ymd_hms(Date_time_local))==0, "00", minute(ymd_hms(Date_time_local))))
    ) %>% 
    mutate(
      Time = Time_local,
      Date = Date_local %>% as.Date(format = "%d/%m/%Y"),
      HomeTeam_original = `Home Team`,
      AwayTeam_original = `Away Team`
    ) 
  
  if(!enriched){
    df_out <- df_out %>% 
      # Join home team names
      left_join(pl_teams %>% select(Team, Team_name_from_schedule_data), by = c("HomeTeam_original"="Team_name_from_schedule_data")) %>% 
      rename(HomeTeam = Team) %>%
      # Join away team names
      left_join(pl_teams %>% select(Team, Team_name_from_schedule_data), by = c("AwayTeam_original"="Team_name_from_schedule_data")) %>% 
      rename(AwayTeam = Team) 
  } else {}
  
  return(df_out)
  
}


f_calc_season_ending <- function(date){
  
  now_utc <- as_datetime(Sys.time(), tz = "UTC")
  
  year <- now_utc %>% year()
  month <- now_utc %>% month()
  day <- now_utc %>% day()
  
  if(month < 7){
    return(year)
  } else if(month > 7 ){
    return(year + 1)
  } else if(day>15) {
    return(year + 1)
  } else{
    return(year)
  }
  
  
}

f_match_open_for_betting <- function(df_enriched = pl_enriched_schedule, df_schedule = pl_schedule){
  
  df_odds_open <- df_enriched %>% 
    mutate(
      season_ending = f_calc_season_ending(utc_date),
      match_id = paste0(HomeTeam, "-", AwayTeam , "-", season_ending),
      odds_calculated = T
    ) %>% 
    select(
      match_id,
      odds_calculated
    )
  
  now_utc <- as_datetime(Sys.time(), tz = "UTC")
  
  df_bet_open <- df_schedule %>% 
    mutate(
      season_ending = f_calc_season_ending(utc_date),
      match_id = paste0(HomeTeam, "-", AwayTeam , "-", season_ending),
    ) %>% 
    select(match_id, utc_date) %>% 
    left_join(
      df_odds_open
    ) %>% 
    mutate(
      game_15_started = ifelse(utc_date - as.difftime(15, units = "mins") < now_utc, TRUE, FALSE),
      odds_calculated = ifelse(is.na(odds_calculated), F, odds_calculated),
      bet_open = ifelse(odds_calculated & game_15_started == F, TRUE, FALSE)
    )
  
  return(df_bet_open)
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DB FUNCTIONS ----

fetch_data_from_db <- function(query) {
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Execute the query and fetch the results
  df <- dbGetQuery(con, query)
  
  # dbDisconnect(con)
  
  return(df)
}

write_data_to_db_users <- function(username, password_hash, email = "dummy", role = "user") {
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  # SQL query for parameterized insertion
  sql <- "
    INSERT INTO users (username, password_hash, email, role)
    VALUES ($1, $2, $3, $4)
  "
  
  # Execute the query with parameters
  tryCatch({
    dbExecute(con, sql, params = list(username, password_hash, email, role))
    print("New user created successfully!")
  }, error = function(e) {
    print(paste("Error creating user:", e$message))
  })
  
  # dbDisconnect(con)
  
}

write_data_to_db_bets <- function(bet, odds, match_id, user_id, bet_concluded = NULL, cancelled = FALSE) {
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  
  # SQL query for parameterized insertion
  sql <- "
    INSERT INTO bets (bet, odds, placed, bet_concluded, match_id, user_id, cancelled, bet_id)
    VALUES ($1, $2, NOW(), $3, $4, $5, $6, uuid_generate_v4())
  "
  
  # Execute the query with parameters
  tryCatch({
    dbExecute(con, sql, params = list(bet, odds, if (is.null(bet_concluded)) NA else bet_concluded, match_id, user_id, cancelled ))
    # print("New bet added successfully!")
  }, error = function(e) {
    print(paste("Error adding bet:", e$message))
  })
  
  # dbDisconnect(con)
  
}


cancel_bet_in_db <- function(bet_id,  new_cancelled = TRUE) {
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  
  # Build the SQL update query
  sql <- paste0(
    "UPDATE bets SET cancelled = ", ifelse(new_cancelled, "TRUE", "FALSE"), 
    " WHERE bet_id = '", bet_id, "'"
  )
  
  # Execute the query
  tryCatch({
    dbExecute(con, sql)
    showNotification("Bet updated successfully!")
  }, error = function(e) {
    # showNotification(paste("Error updating bet:", e$message), type = "error")
  })
  
  # dbDisconnect(con)
  
}


format_bets_for_match_bets <- function(bets, match_id_input) {
  
  df_your_odds <- bets %>% 
    filter(match_id == match_id_input) %>% 
    select(bet, odds, placed, bet_id)
  
  df_your_odds <- df_your_odds %>% 
    mutate(
      placed_local = with_tz(placed, tzone = Sys.timezone()),
      placed = format(placed_local, "%Y-%m-%d %H:%M")
    ) %>% 
    arrange(placed) %>% 
    select(-placed_local)
  
  return(df_your_odds)
  
  
  
}

fetch_table_all_bets <- function(r6) {
  
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  query <- "SELECT bet, odds, placed, bet_concluded, bet_id, match_id FROM bets WHERE user_id = $1 AND cancelled = FALSE"
  all_bets <- dbGetQuery(con, query,
                         list(r6$user_info$user_id
                         )
  )
  
  all_bets <- all_bets %>%
    mutate(
      placed_local = with_tz(placed, tzone = Sys.timezone()),
      placed = format(placed_local, "%Y-%m-%d %H:%M")
    ) %>%
    arrange(placed) %>%
    select(-placed_local)
  
  return(all_bets)
  
}

fetch_total_bets_on_match <- function(match_id_input) {

  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )

  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)

  query <- "SELECT bet, match_id FROM bets WHERE cancelled = FALSE"
  all_bets <- dbGetQuery(con, query)

  match_bets <- all_bets %>% filter(match_id == match_id_input)

  split_text <- strsplit(match_id_input, "-")[[1]]
  home_team_input <- split_text[1]
  away_team_input <- split_text[2]


  match_bets_home <- match_bets %>% filter(bet == home_team_input) %>% nrow()
  match_bets_away <- match_bets %>% filter(bet == away_team_input) %>% nrow()
  match_bets_draw <- match_bets %>% filter(bet == "DRAW") %>% nrow()

  l_bets <- c(match_bets_home, match_bets_draw, match_bets_away)
  
  return(l_bets)
}





# update_n_bets_in_db <- function(user_id, bets_df, bets_week_starting, match_id_input) {
#   
#   bets_used <- bets_df %>% 
#     filter((is.na(bet_concluded))) %>% 
#     nrow()
#   
#   bets_available <- bets_week_starting - bets_used
#   
#   
#   # Establish the connection
#   con <- dbConnect(
#     RPostgres::Postgres(),
#     host = Sys.getenv("DB_HOST"),
#     dbname = Sys.getenv("DB_NAME"),
#     user = Sys.getenv("DB_USER"),
#     password = Sys.getenv("DB_PASSWORD"),
#     port = Sys.getenv("DB_PORT", "5432")
#   )
#   
#   # Ensure connection closes at the end of the function, even if an error occurs
#   on.exit(dbDisconnect(con), add = TRUE)
#   
#   # Parameterized SQL update query
#   sql <- "UPDATE users SET bets_available = $1 WHERE user_id = $2"
#   
#   # Execute the query
#   tryCatch({
#     dbExecute(con, sql, params = list(bets_available, user_id))
#     message("Bet availability updated successfully!")
#   }, error = function(e) {
#     message("Error updating bet availability: ", e$message)
#   })
# }

update_last_logged_in_db <- function(user_id) {
  
  utc_date <- as_datetime(Sys.time(), tz = "UTC")
  
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Parameterized SQL update query
  sql <- "UPDATE users SET last_login = $1 WHERE user_id = $2"
  
  # Execute the query
  tryCatch({
    dbExecute(con, sql, params = list(utc_date, user_id))
  }, error = function(e) {
    message("Error updating bet availability: ", e$message)
  })
}





full_update_after_bet_place <- function(user_id, bets_df, bets_week_starting, match_id_input) {
  
  if(F){
    user_id = r6$user_info$user_id
    bets_df = r6$user_info$bets
    bets_week_starting = r6$user_info$bets_week_starting
    match_id_input = "Fulham-Wolves-2025"
  }
  
  
  bets_used <- bets_df %>% 
    filter((is.na(bet_concluded))) %>% 
    nrow()
  
  bets_available <- bets_week_starting - bets_used
  
  
  # Establish the connection
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT", "5432")
  )
  # Ensure connection closes at the end of the function, even if an error occurs
  on.exit(dbDisconnect(con), add = TRUE)  
  
  
  # DOWNLOAD ALL BETS
  query <- "SELECT bet, match_id, odds FROM bets WHERE cancelled = FALSE"
  all_bets <- dbGetQuery(con, query)
  
  # if(F){
  # query <- "SELECT * FROM bets WHERE cancelled = FALSE"
  # all_bets <- dbGetQuery(con, query)
  # }
  
  
  # Schedules ENRICHED:
  enriched_raw_from_df <- fetch_data_from_db("SELECT * FROM premier_league_fixtures_historical_enriched")
  
  raw_pl_schedules_enriched <- enriched_raw_from_df %>% 
    mutate(
      Date = format(as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"), "%d/%m/%Y %H:%M"),
      season_ending = f_calc_season_ending(Date),
      match_id = paste0(HomeTeam, "-", AwayTeam, "-", season_ending)
    )  
  
  raw_pl_schedules_enriched_first_upload <- 
    raw_pl_schedules_enriched %>% 
    filter(match_id == match_id_input) %>% 
    group_by(match_id) %>%
    filter(TimeStamp_Uploaded == min(TimeStamp_Uploaded)) %>% 
    ungroup() %>% 
    f_prepare_schedule_data(enriched = T)

  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CODE FOR PIE:
  
  match_bets <- all_bets %>% filter(match_id == match_id_input)
  
  split_text <- strsplit(match_id_input, "-")[[1]]
  home_team_input <- split_text[1]  
  away_team_input <- split_text[2]  
  
  
  match_bets_home <- match_bets %>% filter(bet == home_team_input) %>% nrow()
  match_bets_away <- match_bets %>% filter(bet == away_team_input) %>% nrow()
  match_bets_draw <- match_bets %>% filter(bet == "DRAW") %>% nrow()
  
  
  if(F){
    match_bets_home <- 1
    match_bets_away <- 0
    match_bets_draw <- 0
  }
  
  
  l_bets <- c(match_bets_home, match_bets_draw, match_bets_away)
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CODE FOR UPDATING USER BETS IN r6
  
  query <- "SELECT bet, odds, placed, bet_concluded, bet_id, match_id FROM bets WHERE user_id = $1 AND cancelled = FALSE"
  all_bets_user <- dbGetQuery(con, query, 
                              list(user_id
                              )
  ) 
  
  all_bets_user <- all_bets_user %>% 
    mutate(
      placed_local = with_tz(placed, tzone = Sys.timezone()),
      placed = format(placed_local, "%Y-%m-%d %H:%M")
    ) %>% 
    arrange(placed) %>% 
    select(-placed_local)
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update odds
  
  total_payout_df = rbind(
    all_bets %>% filter(match_id==match_id_input),
    tibble(
      "bet" = c(home_team_input, "DRAW", away_team_input),
      "match_id" = c(match_id_input, match_id_input, match_id_input),
      "odds" = c(20,20,20)
    )
  ) %>% 
  group_by(bet) %>% summarise(payout = sum(odds)) %>% ungroup() %>% t() %>% as_tibble()
  
  
  df_total_payout_clean <- data.frame(total_payout_df[2,]) %>% setNames(total_payout_df[1,]) %>% as_tibble %>%  # select data
    mutate_all(as.numeric) %>% 
    select(home_team_input, "DRAW", away_team_input) %>% # reorder to home- DRAW - away %>% 
    set_names(c("payout_if_home_win", "payout_if_draw_win", "payout_if_away_win")) %>%  # rename to cols to use 
    mutate(match_id = match_id_input)    
  
  
  
  if(sum(l_bets)>0){
    
    df_with_new_odds <- 
      raw_pl_schedules_enriched_first_upload %>% 
      filter(match_id == match_id_input) %>% 
      left_join(
        df_total_payout_clean,
        by = c("match_id")
      ) %>% 
    mutate(
      total_n_odds_match = sum(l_bets),
      
      perc_to_distribute = 2*log(total_n_odds_match) + total_n_odds_match/100,
      
      payout_total = payout_if_home_win + payout_if_draw_win + payout_if_away_win,
      
      payout_perc_if_home_win = 100 * payout_if_home_win / payout_total,
      payout_perc_if_draw_win = 100 * payout_if_draw_win / payout_total,
      payout_perc_if_away_win = 100 * payout_if_away_win / payout_total,
      
      perc_correction_odds_home = (33 - payout_perc_if_home_win) * perc_to_distribute/100,
      perc_correction_odds_draw = (33 - payout_perc_if_draw_win) * perc_to_distribute/100,
      perc_correction_odds_away = (33 - payout_perc_if_away_win) * perc_to_distribute/100,
      
      new_odds_home = 100/(`Home Win Probability (%)` - perc_correction_odds_home),
      new_odds_draw = 100/(`Draw Probability (%)` - perc_correction_odds_draw),
      new_odds_away = 100/(`Away Win Probability (%)` - perc_correction_odds_away),
    )
    
    df_to_upload <- df_with_new_odds %>% 
      mutate(
        changed_since_start_home = new_odds_home - 100/`Home Win Probability (%)`,
        changed_since_start_draw = new_odds_draw - 100/`Draw Probability (%)`,
        changed_since_start_away = new_odds_away - 100/`Away Win Probability (%)`,
        
        `Home Win Probability (%)` = `Home Win Probability (%)` - perc_correction_odds_home,
        `Draw Probability (%)` = `Draw Probability (%)` - perc_correction_odds_draw,
        `Away Win Probability (%)` = `Away Win Probability (%)` - perc_correction_odds_away,
        
        TimeStamp_Uploaded = as_datetime(Sys.time(), tz = "UTC")
      ) %>% 
      select(enriched_raw_from_df %>% names())
    
    r6_update_odds_pl <- df_to_upload
    
    dbWriteTable(
      conn = con,
      name = "premier_league_fixtures_historical_enriched",
      value = r6_update_odds_pl,
      append = TRUE,
      row.names = FALSE
    )
    
  }
  else{
    r6_update_odds_pl <- enriched_raw_from_df
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # UPDATE N BETS FOR USER:
  
  # Parameterized SQL update query
  sql <- "UPDATE users SET bets_available = $1 WHERE user_id = $2"
  
  # Execute the query
  tryCatch({
    dbExecute(con, sql, params = list(bets_available, user_id))
    message("Bet availability updated successfully!")
  }, error = function(e) {
    message("Error updating bet availability: ", e$message)
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lout <- list(
    l_bets,
    r6_update_odds_pl,
    all_bets_user
  )
  
  return(lout)
  
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USE FUNCTIONS ----

percent_formatter <- function(x) {
  paste0(x, "%")
}

if(F){
  data = raw_pl_data_all
  n_matches = 3
  h2h_teams = c("Brighton", "Man City") 
}

f_part_calculate_scores <- function(df, n_matches = n_matches){
  
  df_out <- df %>% group_by(Team) %>% arrange(Date %>% desc) %>% 
    mutate(recent_match_nr = row_number()) %>% ungroup %>% 
    
    filter(recent_match_nr <= n_matches) %>%
    group_by(Team) %>% 
    summarise(
      Goals_Scored_lx = sum(FT_Scored),
      Goals_Conceded_lx = sum(FT_Conceded),
      Points_lx = sum(Points),
      Shots_For_lx = sum(Shots_For),
      Shots_Conceded_lx = sum(Shots_Conceded),
      Shots_OT_For_lx = sum(Shots_OT_For),
      Shots_OT_Conceded_lx = sum(Shots_OT_Conceded),
      
      n_matches_in_sample = n()
    ) %>% ungroup()
  
  return(df_out)
}

f_part_calculate_ranks <- function(df) {
  
  df_out  <-
    df %>% 
    mutate(
      Rank_Goals_Scored_lx =       rank(desc(Goals_Scored_lx)   , ties.method = "min"),
      Rank_Goals_Conceded_lx =     rank(Goals_Conceded_lx       , ties.method = "min"),
      Rank_Points_lx =             rank(desc(Points_lx)         , ties.method = "min"),
      Rank_Shots_For_lx =          rank(desc(Shots_For_lx)      , ties.method = "min"),
      Rank_Shots_Conceded_lx =      rank(Shots_Conceded_lx        , ties.method = "min"),
      Rank_Shots_OT_For_lx =       rank(desc(Shots_OT_For_lx)   , ties.method = "min"),
      Rank_Shots_OT_Conceded_lx =   rank(Shots_OT_Conceded_lx     , ties.method = "min")
    )
  
  return(df_out)
  
}

f_metrics_league <-  function(data, n_matches = 5, h2h_teams = "SKIP", home_away = F) {
  
  if(h2h_teams[1] != "SKIP"  &  home_away){
    print("Cant run home/away and head-to-head at the same time")
    stop()
  }
  
  this_season_teams = 
    rbind(
      # home teams
      data %>% 
        filter(Season_ending_year == data$Season_ending_year %>% max) %>% 
        select(Team = HomeTeam),
      # away teams
      data %>% 
        filter(Season_ending_year == data$Season_ending_year %>% max) %>% 
        select(Team = AwayTeam)
    ) %>% 
    unique()
  
  team_list = f_team_list(data) %>% as_tibble() %>% rename(Team = value)
  
  if(home_away){
    df_results_home <- 
      team_list %>% left_join(data, by = c("Team" = "HomeTeam")) %>% mutate(Home = T) %>% rename(Opponent = AwayTeam) %>% 
      f_team_cols_rename(Home_Metrics = T) %>% 
      f_part_calculate_scores(. , n_matches = n_matches) %>% 
      filter(Team %in% (this_season_teams %>% pull)) %>%
      f_part_calculate_ranks()
    
    stats_home = df_results_home %>%
      select(Team, !starts_with("Rank")) %>%
      pivot_longer(-Team) %>%
      mutate(name = glue("Rank_{name}"), 
             value = round(value,0))
    
    df_out_home <- 
      df_results_home %>% 
      select(Team, starts_with("Rank"), n_matches_in_sample) %>% 
      pivot_longer(c(-Team, -n_matches_in_sample)) %>% 
      rename(rank = value) %>% 
      left_join(stats_home, by = c("Team" = "Team", "name" = "name") ) %>% 
      mutate(metric = str_sub(name, start = 6)) %>% 
      select(-name)%>% 
      mutate(home_away = "home")
    
    
    df_results_away <- 
      team_list %>% left_join(data, by = c("Team" = "AwayTeam")) %>% mutate(Home = F) %>% rename(Opponent = HomeTeam) %>% 
      f_team_cols_rename(Home_Metrics = F) %>% 
      f_part_calculate_scores(. , n_matches = n_matches) %>% 
      filter(Team %in% (this_season_teams %>% pull)) %>%
      f_part_calculate_ranks()
    
    stats_away = df_results_away %>%
      select(Team, !starts_with("Rank")) %>%
      pivot_longer(-Team) %>%
      mutate(name = glue("Rank_{name}"), 
             value = round(value,0))
    
    df_out_away <- 
      df_results_away %>% 
      select(Team, starts_with("Rank"), n_matches_in_sample) %>% 
      pivot_longer(c(-Team, -n_matches_in_sample)) %>% 
      rename(rank = value) %>% 
      left_join(stats_away, by = c("Team" = "Team", "name" = "name") ) %>% 
      mutate(metric = str_sub(name, start = 6)) %>% 
      select(-name) %>% 
      mutate(home_away = "away")
    
    df_out = rbind(
      df_out_home
      ,
      df_out_away
      
    )
    
  } else {
    
    df_results <- rbind(
      team_list %>% left_join(data, by = c("Team" = "HomeTeam")) %>% mutate(Home = T) %>% rename(Opponent = AwayTeam) %>% 
        f_team_cols_rename(Home_Metrics = T)
      ,
      
      team_list %>% left_join(data, by = c("Team" = "AwayTeam")) %>% mutate(Home = F) %>% rename(Opponent = HomeTeam) %>% 
        f_team_cols_rename(Home_Metrics = F)
    )
    
    
    df_results_latest_x <- df_results %>% 
      f_part_calculate_scores(. , n_matches = n_matches)
    
    if (h2h_teams[1] != "SKIP") {
      
      # Calculate score for all OTHER teams as is:
      df_results_latest_x_other_teams <- df_results_latest_x %>% 
        filter(
          Team != h2h_teams[1],
          Team != h2h_teams[2]
        )
      
      # filter matches to only h2h
      df_results_h2h <- df_results %>%
        mutate(h2h_string = paste0(Team, "_",Opponent)) %>% 
        filter(h2h_string == paste0(h2h_teams[1], "_", h2h_teams[2]) |  h2h_string == paste0(h2h_teams[2], "_", h2h_teams[1]))
      
      # Calculate score for the teams, using only h2h, and concat with other teams
      df_results_latest_x <- 
        
        rbind(
          # h2h scores:
          df_results_h2h %>% 
            f_part_calculate_scores(. , n_matches = n_matches)
          ,
          df_results_latest_x_other_teams
          
        )
    }
    
    # latest x matches
    df_results_latest_x_with_ranks <- 
      df_results_latest_x %>% 
      filter(Team %in% (this_season_teams %>% pull)) %>%
      f_part_calculate_ranks()
    
    stats = df_results_latest_x_with_ranks %>%
      select(Team, !starts_with("Rank")) %>%
      pivot_longer(-Team) %>%
      mutate(name = glue("Rank_{name}"), 
             value = round(value,0))
    
    df_out <- 
      df_results_latest_x_with_ranks %>% 
      select(Team, starts_with("Rank"), n_matches_in_sample) %>% 
      pivot_longer(c(-Team, -n_matches_in_sample)) %>% 
      rename(rank = value) %>% 
      left_join(stats, by = c("Team" = "Team", "name" = "name") ) %>% 
      mutate(metric = str_sub(name, start = 6)) %>% 
      select(-name)
  }
  
  return(df_out)
  
}



if(F){
  df_schedule <- ll_schedule
}

f_prepare_schedule_view <- function(df_schedule) {
  
  df_schedule_filtered = df_schedule %>% arrange(Date) %>% 
    # filter(Date>= Sys.Date()) %>% 
    mutate(vs_col = "VS",
           # Date    # Convert date here?
    ) %>% 
    select(Date, Time, HomeTeam, vs_col, AwayTeam)
  
  
  
  return(df_schedule_filtered)
  
  
}




f_form_plot <- function(data, team, date){
  
  
  data <- data
  
  team_list = f_team_list(data) %>% as_tibble() %>% rename(Team = value)
  
  df_results <- rbind(
    team_list %>% left_join(data, by = c("Team" = "HomeTeam")) %>% rename(Opponent = AwayTeam) %>% 
      f_team_cols_rename(Home_Metrics = T) %>% mutate(Home = T),
    team_list %>% left_join(data, by = c("Team" = "AwayTeam")) %>% mutate(Home = F) %>% rename(Opponent = HomeTeam) %>% 
      f_team_cols_rename(Home_Metrics = F)%>% mutate(Home = F)
  ) %>% 
    group_by(Team) %>% arrange(Date %>% desc) %>% 
    mutate(recent_match_nr = row_number(), Home = ifelse(Home, "Yes", "No")) %>% ungroup %>% 
    filter(recent_match_nr <= 5) %>% 
    select(Team, Opponent, Home, Date, FT_Scored, FT_Conceded, recent_match_nr, Result) %>% 
    filter(Date < date)
  
  plot_df <- df_results %>%  filter(Team == team) %>% 
    mutate(home_away = ifelse(Home == "Yes", "H", "A"))
  
  
  p_out <- renderPlot({
    ggplot() + 
      theme_minimal() +
      geom_point(
        data = plot_df,
        aes(x = recent_match_nr, y = 1, color=Result), size = 8, fill = "white", shape = 22
      ) + 
      geom_text(data = plot_df, aes(x = recent_match_nr, y = 1, label = home_away, 
                                    fontface = "bold", color = Result, family = "Ahronbdgg")) +
      
      scale_color_manual(values = c("L" = "red", "D" = "#36383a", "W" = "#4CAF50")) +
      
      theme(
        text = element_text(family = "Ahronbdgg"), # Set the default text family
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(),
        legend.position = 'none'
      ) +
      scale_x_continuous(limits = c(-0.7,5.2)) + 
      # geom_label(aes(x = -0.5, y = 1, label = "Latest \n Game"), fill = 'transparent') + 
      geom_label(aes(x = -0.3, y = 1, label = "?"), fill = 'transparent', family = "Ahronbdgg") + 
      scale_y_continuous(limits = c(0.98, 1.02))
    
    
  }, bg="transparent")
  
  
  return(p_out)
  
}


f_format_date_with_suffix <- function(input_date) {
  get_ordinal_suffix <- function(day) {
    if (day %in% c(11, 12, 13)) {
      return("th")
    } else {
      suffix <- c("th", "st", "nd", "rd", rep("th", 16))
      return(suffix[day %% 10 + 1])
    }
  }
  
  # Format the date without leading zeros for single-digit days
  formatted_date <- sprintf("%s %s %d", weekdays(input_date, abbreviate = TRUE), 
                            months(input_date, abbreviate = TRUE), 
                            as.numeric(format(input_date, "%e")))
  
  # Get the day of the month as a numeric value
  day <- as.numeric(format(input_date, "%d"))
  
  # Get the ordinal suffix
  ordinal_suffix <- get_ordinal_suffix(day)
  
  # Combine formatted date and ordinal suffix
  formatted_date_with_suffix <- paste0(formatted_date, ordinal_suffix)
  
  return(formatted_date_with_suffix)
}


if(F){
  Home_rank = 6
  Away_rank = 18
  range = 20-1
  Home_label = "ARS"
  Away_label = "MCI"
}

f_plot_winning_prediction_percent <- function(Home_rank, Away_rank, range, Home_label, Away_label, return_plot = T) {
  rank_spread = abs(Away_rank-Home_rank)
  
  pool_for_teams = (range - rank_spread) / 4
  pool_for_draw =  (range - rank_spread) / 2
  
  perc_for_best  =  100* (pool_for_teams + rank_spread) /  range
  perc_for_worst =  100* pool_for_teams / range
  perc_for_draw  =  100* pool_for_draw  / range
  
  
  
  colors <- c("#14499F", "#6b7073", "#98AFD3")
  
  home_text_bg = "white"
  away_text_bg = "white"
  draw_text_bg = "#6b7073"
  
  if(Home_rank<Away_rank){
    Home_perc = perc_for_best
    Away_perc = perc_for_worst
    
    names(colors) <- c(Home_label, "Draw", Away_label)
    
    home_text_bg = "#00BF62"
    
  } else if(Home_rank>Away_rank){
    Home_perc = perc_for_worst
    Away_perc = perc_for_best
    
    names(colors) <- c(Away_label, "Draw", Home_label)
    
    away_text_bg = "#00BF62"
  } else{
    Home_perc = perc_for_best
    Away_perc = perc_for_worst
    colors <- c("#98AFD3", "#6b7073", "#98AFD3")
    names(colors) <- c(Home_label, "Draw", Away_label)
  }
  
  if(return_plot) {
    
    df_plot <- tribble(
      ~Team,      ~Team_perc,    ~Team_label,                       ~Xaxis,        ~Label_pos,   ~bg_col, 
      Home_label, Home_perc,     paste0(Home_label, "\n", round(Home_perc, 0), "%"),     1,    -5, home_text_bg,
      "Draw",     perc_for_draw, paste0("Draw", "\n", round(perc_for_draw, 0), "%"),     1.4,  Home_perc + perc_for_draw/2, draw_text_bg,
      Away_label, Away_perc,     paste0(Away_label, "\n", round(Away_perc, 0), "%"),     1,    105, away_text_bg,
    ) %>% 
      left_join(
        tibble(colors, names(colors)) %>% set_names(c("bar_col", "Team"))
      ) 
    
    # Reverse the order of levels so that "MCI" is stacked at the bottom
    df_plot <- df_plot %>%
      mutate(
        Team = factor(Team, levels = c(Away_label, "Draw", Home_label)) # Reversed order
      )
    
    # Create a named vector for the colors
    team_colors <- setNames(df_plot$bar_col, df_plot$Team)
    
    # Plot
    p <-  ggplot(df_plot) + 
      # Use Team for fill and directly use Team_perc for y-values
      geom_bar(aes(x = 1, y = Team_perc, fill = Team), stat = "identity", position = "stack") +
      # Use scale_fill_manual to map the correct colors for Team
      scale_fill_manual(values = team_colors) +
      # Add labels with correct fill from bg_col
      geom_label(aes(x = Xaxis, y = Label_pos, label = Team_label), fill = df_plot$bg_col, color = "black", fontface = "bold", size = 5, family = "Ahronbdgg") +
      scale_y_continuous(breaks = c(0, 25, 50, 75, 100), labels = scales::percent_format(scale = 1), limits = c(-15, 115)) +
      scale_x_continuous(breaks = NULL, limits = c(0.5, 2)) +
      coord_flip() + 
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        # text = element_text(size = 15, family = "Ahronbdgg"),
        axis.title.y.left =  element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        # axis.text = element_text( family = "Ahronbdgg"),
        panel.border = element_blank()
        
      )
    
  } else {
    p = NULL
  }
  
  lout <- list()
  
  lout$plot <-  p
  lout$home_team_perc <- Home_perc
  lout$away_team_perc <- Away_perc
  
  return(lout)
}


f_plot_ranks <- function(metrics_df, home_team, away_team, positive_metric_plot = T, home_away_value = "SKIP") {
  
  if (home_away_value == "home"){
    metrics_df = metrics_df %>% filter(home_away == "home") %>% 
      mutate(
        color = case_when(
          Team == home_team ~ "home/away",
          T ~ "normal"
        )
      )
  } else if (home_away_value == "away"){
    metrics_df = metrics_df %>% filter(home_away == "away") %>% 
      mutate(
        color = case_when(
          Team == away_team ~ "home/away",
          T ~ "normal"
        )
      )
  } else {
    
    Home_rank =  metrics_df %>% 
      filter(Team == home_team) %>% 
      pull(rank)
    
    Away_rank =  metrics_df %>% 
      filter(Team == away_team) %>% 
      pull(rank)
    
    if(Away_rank < Home_rank) {
      
      metrics_df <- metrics_df %>% 
        mutate(
          color = case_when(
            Team == away_team ~ "best",
            Team == home_team ~ "worse",
            T ~ "normal"
          )
        )
      
    } else if (Away_rank > Home_rank)  {
      metrics_df <- metrics_df %>% 
        mutate(
          color = case_when(
            Team == away_team ~ "worse",
            Team == home_team ~ "best",
            T ~ "normal"
          )
        )
    } else{
      metrics_df <- metrics_df %>% 
        mutate(
          color = case_when(
            Team == away_team ~ "worse",
            Team == home_team ~ "worse",
            T ~ "normal"
          )
        )
    }
  }
  
  
  if (positive_metric_plot){
    p_init <- ggplot(metrics_df, aes(x = reorder(interaction(rank, Team), -value), y = value, fill = color))
  } else{
    p_init <- ggplot(metrics_df, aes(x = reorder(interaction(rank, Team), value), y = value, fill = color))
  }
  
  p_final <- p_init +
    geom_bar(stat = "identity", width = 0.8) +  
    scale_fill_manual(values = c("normal" = "#6b7073","worse" = "#98AFD3", "best" = "#00BF62", "home/away" = "#14499F" )) +
    scale_x_discrete(labels = function(x) sub("^[0-9]+\\.", "", x)) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 70, vjust = 0.95, hjust=1),
      axis.title.x = element_blank(),
      axis.title.y.left =  element_blank(),
      text = element_text(size = 15, family = "Ahronbdgg"),
      axis.text.y = element_text(size = 18, family = "Ahronbdgg"),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      axis.text = element_text( family = "Ahronbdgg")
    )
  
  return(p_final)
  
}




# Function to calculate overall weighted percentages
f_calculate_overall_percentages <- function(metrics, Home_label, Away_label, return_only_percentage_df = F) {
  
  total_weight <- 0
  Home_perc <- 0
  Away_perc <- 0
  perc_for_draw <- 0
  
  for (metric in metrics) {
    weight <- as.numeric(sub("%", "", metric$weight))
    total_weight <- total_weight + weight
    Home_perc <- Home_perc + weight * metric$home_team_perc
    Away_perc <- Away_perc + weight * metric$away_team_perc
    perc_for_draw <- perc_for_draw + weight * metric$draw_team_perc
  }
  
  if(total_weight>0){
    Home_perc <- Home_perc / total_weight
    Away_perc <- Away_perc / total_weight
    perc_for_draw <- perc_for_draw / total_weight
    
    if(return_only_percentage_df){
      return(c("Home_perc" = Home_perc, "perc_for_draw" = perc_for_draw, "Away_perc" = Away_perc))
    }
    
    colors <- c("#14499F", "#6b7073", "#98AFD3")
    
    if(Home_perc > Away_perc){
      names(colors) <- c(Home_label, "Draw", Away_label)
    } else if(Home_perc < Away_perc){
      names(colors) <- c(Away_label, "Draw", Home_label)
    } else{
      colors <- c("#98AFD3", "#6b7073", "#98AFD3")
      names(colors) <- c(Home_label, "Draw", Away_label)
    }
    
    df_plot <- tribble(
      ~Team,      ~Team_perc,    ~Team_label,                       ~Xaxis,  ~Label_pos, 
      Away_label, Away_perc,     paste0(Away_label, "\n", round(Away_perc, 0), "%"),     1,    105, #Home_perc + perc_for_draw + Away_perc/2,
      "Draw",     perc_for_draw, paste0("Draw", "\n", round(perc_for_draw, 0), "%"),     1.4,    Home_perc + perc_for_draw/2, #50, #Home_perc + perc_for_draw/2,
      Home_label, Home_perc,     paste0(Home_label, "\n", round(Home_perc, 0), "%"),     1,    -5, #Home_perc/2,
    ) %>% 
      mutate(
        Team = factor(c(Away_label, "Draw",Home_label), levels = c(Away_label, "Draw",Home_label))
      )
    
    p <- 
      ggplot() + 
      geom_bar(data = df_plot, aes(x = 1, y = Team_perc, fill = Team), position="stack", stat="identity") + 
      geom_label(data = df_plot, aes(x = Xaxis, y = Label_pos, color = Team, label = Team_label), fontface = "bold", size=5, family = "Ahronbdgg") + 
      # geom_label(data = df_plot, aes(x = Xaxis + 0.7,  y = Label_pos, color = Team, label = Team)      , fontface = "bold", label.size = NA, fill = NA,size=4) + 
      scale_fill_manual(values = colors) + 
      scale_color_manual(values = colors) + 
      scale_y_continuous(breaks = c(0, 50, 100),labels = percent_formatter, limits = c(-10,110)) +
      scale_x_continuous(breaks=NULL, limits = c(0.5, 1.9)) + 
      coord_flip() + 
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        text = element_text(size = 18, family = "Ahronbdgg"),
        axis.title.y.left =  element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(),
        axis.text = element_text( family = "Ahronbdgg")
      )
  } else{
    
    p <- ggplot() + 
      geom_point(aes(x=1,y=1), alpha=0) +
      geom_label(aes(x = 1, y=1),label = "Select Minium 1 stat", size  =8, fill = NA, family = "Ahronbdgg") +
      scale_x_continuous(breaks=NULL) + 
      scale_y_continuous(breaks=NULL) + 
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        text = element_text(size = 14, family = "Ahronbdgg"),
        axis.title.y.left =  element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(),
        axis.text = element_text( family = "Ahronbdgg")
      )
  }
  return(p)
  
}


f_plot_divergent_bar <- function(metrics_df, home_team, away_team, title_stat, home_away = F) {
  
  if(home_away) {
    Home_rank =  metrics_df %>% 
      filter(Team == home_team,
             home_away == "home"
      ) %>% 
      pull(rank)
    
    Away_rank =  metrics_df %>% 
      filter(Team == away_team,
             home_away == "away") %>% 
      pull(rank)
    
    Home_value =  metrics_df %>% 
      filter(Team == home_team,
             home_away == "home"
      ) %>% 
      pull(value)
    
    Away_value =  metrics_df %>% 
      filter(Team == away_team,
             home_away == "away") %>% 
      pull(value)
    
  } else{
    
    Home_rank =  metrics_df %>% 
      filter(Team == home_team) %>% 
      pull(rank)
    
    Away_rank =  metrics_df %>% 
      filter(Team == away_team) %>% 
      pull(rank)
    
    Home_value =  metrics_df %>% 
      filter(Team == home_team
      ) %>% 
      pull(value)
    
    Away_value =  metrics_df %>% 
      filter(Team == away_team) %>% 
      pull(value)
    
  }
  
  colors <- c("#14499F", "#BBD7E3")
  
  home_text_bg = "white"
  away_text_bg = "white"
  
  if(Home_rank<Away_rank){
    
    names(colors) <- c(home_team, away_team)
    home_text_bg = "#00BF62"
    
  } else if(Home_rank>Away_rank){
    
    names(colors) <- c(away_team, home_team)
    away_text_bg = "#00BF62"
    
  } else{
    
    colors <- c("#98AFD3",  "#98AFD3")
    names(colors) <- c(home_team, away_team)
  }
  
  
  if(home_away) {
    
    plot_df <- rbind(
      metrics_df %>% 
        filter(Team == home_team,
               home_away == "home") %>% 
        mutate(rank_plot = -5 + (sqrt(rank)) -6)
      ,
      metrics_df %>%
        filter(Team == away_team,
               home_away == "away") %>% 
        mutate(rank_plot = 5 - (sqrt(rank)) + 6)
    )
  } else{
    plot_df <- rbind(
      metrics_df %>% 
        filter(Team == home_team) %>% 
        mutate(rank_plot = -5 + (sqrt(rank)) -6)
      ,
      metrics_df %>%
        filter(Team == away_team) %>% 
        mutate(rank_plot = 5 - (sqrt(rank)) + 6)
    )
  }
  
  
  plot_df_misc1 <- tribble(
    ~fill, ~val, 
    "top", 1,
    "bottom", 2.7,
    "top", -1,
    "bottom", -2.7,
  ) %>% 
    mutate(fill =  factor(fill, levels= c("top", "bottom")))
  
  colors2 <- c('transparent',  "white")
  names(colors2) <- c("bottom", "top")
  
  plot_df_misc2 <- tribble(
    ~fill, ~val, 
    "top", 3.7,
    "bottom", 0.4,
    "top", -3.7,
    "bottom", -0.4,
  ) %>% 
    mutate(fill =  factor(fill, levels= c("top", "bottom")))
  
  
  p <- ggplot() + 
    geom_bar(data = plot_df, stat = "identity",
             aes(y = rank_plot, x = 1, fill = Team),
             width = 0.8
    ) + 
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks=NULL, limits = c(0.5,1.5)) +
    scale_y_continuous(breaks=NULL, limits = c(-10.1,10.1)) +
    
    # BACKGROUND
    geom_bar(stat = "identity", aes(x=1,y=5),fill = "#00BF62", width = 0.8) +
    geom_bar(stat = "identity", aes(x=1,y=-5),fill = "#00BF62", width = 0.8) +
    
    # SIDE LINES
    geom_bar(data = plot_df_misc1, stat = "identity",
             aes(x = 1, y = val, color = fill),
             alpha = 0 , width = 0.6, size = 3) +
    
    # GOALS
    geom_bar(data = plot_df_misc2, stat = "identity",
             aes(x = 1, y = val, color = fill),
             alpha = 0 , width = 0.2, size = 2) +
    scale_color_manual(values = colors2) + 
    
    # FILL ON TOP
    geom_bar(stat = "identity", aes(x=1,y=3.7),fill = "#00BF62", width = 0.6) +
    geom_bar(stat = "identity", aes(x=1,y=-3.7),fill = "#00BF62", width = 0.6) +
    
    geom_text(aes(x = 1, y = 0, label = title_stat), color = "black",fontface = "bold", size = 4, family = "Ahronbdgg") + 
    geom_label(aes(x = 1, y = 5.5, label = paste0(Away_value)),fontface = "bold", size  = 6, family = "Ahronbdgg", fill = away_text_bg) + 
    geom_label(aes(x = 1, y = -5.5, label = paste0(Home_value)),fontface = "bold", size  = 6, family = "Ahronbdgg", fill = home_text_bg) + 
    
    
    coord_flip() + 
    theme(
      legend.position = 'none',
      axis.title.x = element_blank(),
      text = element_text(size = 10, family = "Ahronbdgg"),
      axis.title.y.left =  element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
    )
  return(p)
  
}



f_last_10_table <- function(df, team) {
  
  
  df_use_home_away <- rbind(
    df %>% 
      filter(HomeTeam == team),
    df %>% 
      filter(AwayTeam == team)
  ) %>% 
    mutate(`Date` = `Date` %>% as.Date(format = "%d/%m/%Y")) %>% 
    slice_max(Date, n = 10) %>% 
    mutate(
      `Score` = paste0(FTHG, " - ", FTAG),
      Result = case_when(
        HomeTeam == team & FTR == "H" ~ "W",
        AwayTeam == team & FTR == "A" ~ "W",
        FTR == "D" ~ "D",
        T ~ "L",
      )
    ) %>% 
    select(
      Date, Result, HomeTeam, `Score`, AwayTeam
    )
  
  t_out <- reactable(df_use_home_away, 
                     columns = list(
                       Date = colDef(minWidth = 110, align = "left", style = list(fontSize = "14px")),
                       # Time = colDef(minWidth = 80, align = "center", style = list(fontSize = "18px")), 
                       Result = colDef(
                         name = "", minWidth = 40, align = "center", vAlign = "center", style = list(fontSize = "20px"),
                         cell = function(value) {
                           color <- if (value == "W") {
                             "#4CAF50"
                           } else if (value == "L") {
                             "#e22020"
                           } else {
                             "#6b7073"
                           }
                           htmltools::tags$span(style = paste("color:", color, "; font-weight: bold;"), value)
                         }
                       ),
                       HomeTeam = colDef(name = "Home", minWidth = 140, align = "right", vAlign = "center",
                                         cell = function(value) {
                                           if (value == team) {
                                             # Return a named list with the "style" key containing a list of CSS properties
                                             list(cell = htmltools::tags$span(style = "color: #14499F; font-weight: bold; fontSize: 17px;", value))
                                           } else {
                                             list(cell = htmltools::tags$span(style = "color: #6b7073; font-weight: light; fontSize: 14px;", value))
                                           }
                                         }),
                       Score = colDef(name = "", minWidth = 80, align = "center", vAlign = "center",
                                      cell = function(value) {
                                        value
                                      },
                                      style = function(value) list(fontSize = "16px")
                       ),
                       
                       AwayTeam = colDef(name = "Away", minWidth = 140, vAlign = "center",
                                         cell = function(value) {
                                           if (value == team) {
                                             # Return a named list with the "style" key containing a list of CSS properties
                                             list(cell = htmltools::tags$span(style = "color: #14499F; font-weight: bold; fontSize: 17px;", value))
                                           } else {
                                             list(cell = htmltools::tags$span(style = "color: #6b7073; font-weight: light; fontSize: 14px;", value))
                                           }
                                         }
                       )
                       
                     ),
                     # Default stuff
                     pagination = FALSE, sortable = FALSE, fullWidth = FALSE,
                     
                     theme = reactableTheme(
                       headerStyle = list(
                         backgroundColor = "#14499F",  # Set the desired background color
                         color = "white",              # Set header text color to white
                         fontWeight = "bold"           # Make header text bold (optional)
                       ),
                       backgroundColor = "transparent"
                       
                     ),
                     
                     class = "custom_table_10_matches"
                     
  )
  
  return(t_out)
  
  
} 



f_pie_n_bets <- function(list_n_bets, list_labels = c("Home", "Draw", "Away")) {
  
  
  if(sum(list_n_bets)==0){
    
    plot <- plot_ly(type = "scatter", mode = "text") %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(
            x = 0.5,
            y = 0.5,
            text = "No Bets placed yet",
            showarrow = FALSE,
            font = list(size = 20),
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "middle"
          )
        )
      ) %>% 
      layout(
        title = list(
          text = paste("<b>Total bets on game:</b><br><span style='font-size:24px;'>", 0, "</span>"),
          x = 0.54,
          y = 1.27,  # Increase this value to add more space above the pie chart
          font = list(family = "Ahronbdgg", size = 20) # Font for "Total bets on game" text
        ),
        
        showlegend = FALSE,             # Hide the legend
        paper_bgcolor = 'rgba(0, 0, 0, 0)', # Transparent background
        plot_bgcolor = 'rgba(0, 0, 0, 0)'   # Transparent plot background
        
      ) %>% 
      config(displayModeBar = FALSE)  # Hide the mode bar
    
    return(plot)
    
  }
  
  # Define team data with fixed ordering and labels
  # REVERSE LISTS:
  labels <- factor(c(list_labels[3], list_labels[2], list_labels[1]), levels = c(list_labels[3], list_labels[2], list_labels[1]))  # Fixed order
  values <- c(list_n_bets[3], list_n_bets[2], list_n_bets[1])  # Corresponding values for each team
  
  total_bets <- sum(values)
  
  if(values[1] > values[3]){
    color_vec = c("#14499F", "#8A8A8A", "#98AFD3")
  } else if (values[3] > values[1]){
    color_vec = c("#98AFD3", "#8A8A8A", "#14499F")
  } else {
    color_vec = c("#98AFD3", "#8A8A8A", "#98AFD3")
  }
  
  # Create the plot
  p <- plot_ly(
    labels = labels,
    values = values,
    type = 'pie',
    sort = FALSE,                    # Disable automatic sorting by values
    direction = "clockwise",         # Clockwise order
    textinfo = 'label+percent', 
    hoverinfo = 'value+percent',
    # hoverinfo = 'text',              
    # text = ~paste('</br> Bets: ', values,
    # '</br>', round(values / total_bets * 100, 1), '%'), 
    marker = list(
      colors = color_vec, # Colors for each segment
      line = list(color = "white", width = 2)       # White borders between segments
    ),
    textfont = list(
      size = 22,
      color = "white",
      family = "Ahronbdgg",
      bold = TRUE
    )
  ) %>%
    layout(
      title = list(
        text = paste("<b>Total bets on game:</b><br><span style='font-size:36px;'>", total_bets, "</span>"),
        x = 0.54,
        y = 1.27,  # Increase this value to add more space above the pie chart
        font = list(family = "Ahronbdgg", size = 20) # Font for "Total bets on game" text
      ),
      
      margin = list(t = 80, b = 0), # Increase top margin for more space
      showlegend = FALSE,             # Hide the legend
      paper_bgcolor = 'rgba(0, 0, 0, 0)', # Transparent background
      plot_bgcolor = 'rgba(0, 0, 0, 0)'   # Transparent plot background
      
    ) %>%
    style(
      hoverlabel = list(
        bgcolor = color_vec, # Hover background color per segment
        bordercolor = "white",
        font = list(size = 20, family = "Ahronbdgg", bold = TRUE)
      )
    ) %>% 
    config(displayModeBar = FALSE)  # Hide the mode bar
  
  return(p)
}


f_your_bets_table <- function(r6){
  
  # bet, odds, placed, bet_id
  
  df_use <- format_bets_for_match_bets(r6$user_info$bets, 
                                       paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending))
  
  r <- reactable(df_use, 
                 columns = list(
                   bet = colDef(name = "BET", minWidth = 250, align = "left", style = list(fontSize = "16px"), vAlign = "center"),
                   
                   odds = colDef(name = "ODDS", minWidth = 85, align = "center", style = list(fontSize = "18px"), vAlign = "center"),
                   placed = colDef(name = "Placed", minWidth = 200, align = "center", style = list(fontSize = "15px"), vAlign = "center"),
                   
                   bet_id = colDef(
                     name = "Cancel",
                     sortable = FALSE, vAlign = "center", align = "center",
                     cell = function() htmltools::tags$button(class = "odds-button-worse", "Cancel", style = "font-size: 18px;")
                   )
                 ),
                 
                 # Default stuff
                 pagination = FALSE, sortable = FALSE, fullWidth = FALSE,
                 onClick = JS("function(rowInfo, column) {
                    // Only handle click events on the 'details' column
                    if (column.id !== 'bet_id') {
                      return
                    }

                    if (window.Shiny) {
                      Shiny.setInputValue('top_of_estimation_tab-card_module-cancel', rowInfo.values['bet_id'], { priority: 'event' })
                    }
                  }"),
                 
                 class = "selection_table",
                 
                 theme = reactableTheme(
                   backgroundColor = "transparent"
                 )
  )
  
  return(r)
  
}


f_format_all_bets <- function(r6){
  
  df_schedule <- r6$data$pl_schedule %>% 
    mutate(
      match_id = paste0(HomeTeam, "-", AwayTeam , "-", current_season_ending),
    ) %>% 
    select(
      match_id, HomeTeam, AwayTeam, Date, Time
    )
  
  team_winning_df <- r6$data$pl_historic %>% filter(Season_ending_year == max(Season_ending_year)) %>% 
    mutate(
      match_id = paste0(HomeTeam, "-", AwayTeam , "-", current_season_ending),
      winning_team = ifelse(FTR == "H", HomeTeam, 
                            ifelse(FTR == "A", AwayTeam, 
                                   ifelse(FTR == "D", "DRAW", NA)))
    ) %>% 
    select(match_id, winning_team)
  
  
  df_all_bets = r6$user_info$bets %>% 
    mutate(
      placed_local = with_tz(placed, tzone = Sys.timezone()),
      placed = format(placed_local, "%Y-%m-%d %H:%M"),
    ) %>% 
    arrange(placed) %>% 
    select(-placed_local, -bet_id)
  
  
  df_use <- 
    df_all_bets %>% 
    left_join(
      df_schedule,
      by = c("match_id" = "match_id")
    ) %>% 
    left_join(
      team_winning_df
    )
  
  
  
  return(df_use)
}

f_all_bets_table <- function(r6){
  
  df_bet_status <- f_match_open_for_betting() %>% 
    select(match_id, game_15_started)
  
  df_use <- f_format_all_bets(r6) %>% 
    mutate(
      vs_col = "VS",
      Season_ending = f_calc_season_ending(utc_date)
    ) %>% 
    mutate(match_id = paste0(HomeTeam, "-", AwayTeam, "-", Season_ending)) %>% 
    left_join(
      df_bet_status, 
      by=c("match_id")
    ) %>% 
    mutate(
      Status = case_when(
        !is.na(bet_concluded) ~ "Concluded",
        game_15_started ~ "Pending",
        T ~ "Active"
      ),
      col_def = case_when(
        Status == "Concluded" ~ "#14499F",
        Status == "Pending" ~ "#6b7073",
        T ~ "#4CAF50"
      ),
    ) %>% 
    select(Status, HomeTeam, vs_col, AwayTeam, bet, odds, bet_concluded, Date, Time, col_def)
  
  
  
  
  r <- reactable(df_use, 
                 columns = list(
                   
                   Status = colDef(style = list(fontSize = "13px"), align = "center",minWidth = 110 , vAlign = "center",
                                   cell = reactablefmtr::pill_buttons( data =df_use,
                                                                       color_ref  = "col_def",
                                                                       text_color = "white"  # White text color for contrast
                                   )
                   ),
                   
                   
                   
                   Date = colDef(minWidth = 105, align = "center", 
                                 style = list(fontSize = "14px", color = "#14499F"), vAlign = "center"),
                   
                   Time = colDef(minWidth = 80, align = "center", 
                                 style = list(fontSize = "14px", color = "#14499F"), vAlign = "center"), 
                   
                   HomeTeam = colDef(name = "Home Team", minWidth = 170, align = "right", vAlign = "center",
                                     style = function(value, index) {
                                       # Set text to black and larger font if it matches the 'bet' column, else default
                                       if (value == df_use$bet[index]) {
                                         list(color = "black", fontSize = "18px")
                                       } else {
                                         list(color = "#14499F", fontSize = "14px")
                                       }
                                     }),
                   
                   vs_col = colDef(name = "", minWidth = 60, align = "center", vAlign = "center",
                                   style = function(value, index) {
                                     # Set text to black and larger font if it matches the 'bet' column, else default
                                     if ("DRAW" == df_use$bet[index]) {
                                       list(color = "black", fontSize = "18px")
                                     } else {
                                       list(color = "#14499F", fontSize = "14px")
                                     }
                                   }),
                   
                   AwayTeam = colDef(name = "Away Team", minWidth = 170, vAlign = "center",
                                     style = function(value, index) {
                                       # Set text to black and larger font if it matches the 'bet' column, else default
                                       if (value == df_use$bet[index]) {
                                         list(color = "black", fontSize = "18px")
                                       } else {
                                         list(color = "#14499F", fontSize = "14px")
                                       }
                                     }),
                   
                   bet = colDef(show = F), 
                   col_def = colDef(show = F), 
                   
                   odds = colDef(name = "ODDS", minWidth = 60, align = "center", 
                                 style = list(fontSize = "15px", color = "#14499F"), vAlign = "center"),
                   
                   bet_concluded = colDef(name = "Return", minWidth = 100, align = "center", vAlign = "center",
                                          style = function(value) {
                                            color <- if (is.na(value) | value > 0) "#4CAF50" else "#e22020"  # Green if above 0, red if below
                                            list(
                                              fontSize = "20px",color = color,  # Apply conditional color
                                              fontWeight = "bold"
                                            )
                                          }
                   )
                 ),
                 
                 # Default settings
                 pagination = TRUE, sortable = TRUE, fullWidth = FALSE,
                 defaultSorted = list(Date = "desc"),
                 class = "custom_table_10_matches",
                 theme = reactableTheme(
                   backgroundColor = "transparent"
                 )
  )
  
  
  
  return(r)
  
}


