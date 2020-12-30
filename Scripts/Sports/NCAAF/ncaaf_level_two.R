library(tidyr)
library(dplyr)
library(stringr)
library(rvest)
library(lubridate)
library(readr)

ncaaf_l1 <- read_csv("C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Level_One.csv")

ncaaf_l1 <- ncaaf_l1 %>% mutate(
  Line_Favored = "NA",
  Line_Amount = 0,
  Line_Over_Under = 0,
  
  ##
  
  First_Down = 0,
  Third_Down_Conversions = 0,
  Third_Down_Att = 0,
  Fourth_Down_Conversions = 0,
  Fourth_Down_Att = 0,
  Total_Yards = 0,
  Passing_Yards = 0,
  Pass_Completions = 0,
  Pass_Att = 0,
  Yards_Per_Pass = 0,
  Interceptions_Thrown = 0,
  Rushing_Yards = 0,
  Rushing_Att = 0,
  Yards_Per_Rush = 0,
  Penalties = 0,
  Penalties_Yards = 0,
  Turnovers = 0,
  Fumbles_Lost = 0,
  Possession = 0,
  Possession_Percent = 0,
  
  ##
  
  First_Down_Against = 0,
  Third_Down_Conversions_Against = 0,
  Third_Down_Att_Against = 0,
  Fourth_Down_Conversions_Against = 0,
  Fourth_Down_Att_Against = 0,
  Total_Yards_Against = 0,
  Passing_Yards_Against = 0,
  Pass_Completions_Against = 0,
  Pass_Att_Against = 0,
  Yards_Per_Pass_Against = 0,
  Interceptions_Thrown_Against = 0,
  Rushing_Yards_Against = 0,
  Rushing_Att_Against = 0,
  Yards_Per_Rush_Against = 0,
  Penalties_Against = 0,
  Penalties_Yards_Against = 0,
  Turnovers_Against = 0,
  Fumbles_Lost_Against = 0,
  Possession_Against = 0,
  Possession_Percent_Against = 0
)

# ncaaf_l1 <- ncaaf_l1 %>% filter(Game_ID %in% c(401249405, 401249012, 401212545, 401249861, 401207218, 401207200, 401135295))
# ncaaf_l1 <- ncaaf_l1 %>% filter(Result == "TBD" & Played == "FALSE")

for(i in 1:length(unique(ncaaf_l1$Game_ID))){
   # i = 1
  # game_id <- 401261245
  
  # This makes games that haven't been played first
  game_id <- sort(unique(ncaaf_l1$Game_ID), decreasing = T)[i]
  print(game_id)
  
  #### Update L2 Data ####

  end_while <- FALSE
  j <- 1
  while(!end_while){
    game <- try({read_html(paste0("https://www.espn.com/college-football/matchup?gameId=", game_id))})
    game_stats <- try(game %>% html_node(".mod-data") %>% html_table())
    
    if(is.data.frame(game_stats)){
      if(nrow(game_stats) > 0){
        update_data <- TRUE
      }else{
        ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
        update_data <- TRUE
      }
      
    }else{
      update_data <- FALSE
    }
    
    end_while <- ifelse(is.data.frame(game_stats) | j == 3, TRUE, FALSE)
    j <- j + 1
  }
  
  
  #### Correct for Neutral Location Home team ####
  if(nrow(ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Neutral_Location"]) == 2){
    teams <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Team"]
    home_team <- ifelse(game %>% str_detect(paste0("vs. ", as.character(teams[1, 1]))), teams[1, 1] %>% as.character(), teams[2, 1] %>% as.character())
    away_team <- ifelse(game %>% str_detect(paste0("vs. ", as.character(teams[1, 1]))), teams[2, 1] %>% as.character(), teams[1, 1] %>% as.character())
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Team == away_team, "Home"] <- 0
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Team == home_team, "Home"] <- 1

  }
  
  if(update_data){
    names(game_stats) <- c("Stat", "Home", "Away")
    class(game_stats)
    
    played <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Played"] %>% as.character()
    result <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Result"] %>% as.character()
    
    if(played == "TRUE"){
      Line <- game %>% 
        html_nodes(".odds-details") %>% 
        html_text("li") %>% 
        str_extract_all('(?<=Line: )(.*?)(?=\n)') %>% 
        unlist() %>% 
        str_split(" ") %>% 
        unlist()
      
      line_favored <- Line[1]
      line_amount <- ifelse(line_favored == "Even", 0, as.numeric(Line[2]))
      
      over_under <- game %>% 
        html_nodes(".odds-details") %>% 
        html_text("li") %>% 
        str_extract_all('(?<=Over/Under: )(.*?)(?=\n)') %>% 
        unlist() %>% 
        as.numeric()
      
    }else if(result == "TBD" & played == "FALSE"){
      # game <- try({read_html(paste0("https://www.espn.com/college-football/game/_/gameId/401212545"))})
      # game_id <- 401212545
      
      future_game_table <- try({read_html(paste0("https://www.espn.com/college-football/game/_/gameId/", game_id))})
      
      # First logic statement is to test if future game has a line yet 
      if(future_game_table %>% str_detect("mediumTable")){
        future_game_table <- try({future_game_table %>% html_node(".mediumTable") %>% html_table()})
        names(future_game_table) <- c("d1", "d2", "d3", "d4", "Spread", "ML", "Over_Under")
        future_game_table <- future_game_table %>% select(Spread, Over_Under)
        
        home_team <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Team"]
        away_team <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Opponent"]
        
        if(is.numeric(future_game_table$Spread[1]) & is.numeric(future_game_table$Over_Under[1])){
          line_favored <- ifelse(future_game_table$Spread[1] > 0, home_team, away_team)
          line_amount <- -abs(future_game_table$Spread[1])
          over_under <- future_game_table$Over_Under[1]
            
        }else{
          line_favored <- NA
          line_amount <- NA
          over_under <- NA
        }
        
      }else{
        line_favored <- NA
        line_amount <- NA
        over_under <- NA
        
      }
    }
    
    #### Get Team Stats ####
    
    First_Down_Home <- game_stats[game_stats$Stat == "1st Downs", 3] %>% as.numeric()
    First_Down_Away <- game_stats[game_stats$Stat == "1st Downs", 2] %>% as.numeric()
    
    Third_Down_Eff_Raw_Home <- game_stats[game_stats$Stat == "3rd down efficiency", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Third_Down_Eff_Raw_Away <- game_stats[game_stats$Stat == "3rd down efficiency", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Third_Down_Conversions_Home <- Third_Down_Eff_Raw_Home[1]
    Third_Down_Conversions_Away <- Third_Down_Eff_Raw_Away[1]
    
    Third_Down_Att_Home <- Third_Down_Eff_Raw_Home[2]
    Third_Down_Att_Away <- Third_Down_Eff_Raw_Away[2]
    
    Fourth_Down_Eff_Raw_Home <- game_stats[game_stats$Stat == "4th down efficiency", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Fourth_Down_Eff_Raw_Away <- game_stats[game_stats$Stat == "4th down efficiency", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Fourth_Down_Conversions_Home <- Fourth_Down_Eff_Raw_Home[1]
    Fourth_Down_Conversions_Away <- Fourth_Down_Eff_Raw_Away[1]
    
    Fourth_Down_Att_Home <- Fourth_Down_Eff_Raw_Home[2]
    Fourth_Down_Att_Away <- Fourth_Down_Eff_Raw_Away[2]
    
    Total_Yards_Home <- game_stats[game_stats$Stat == "Total Yards", 3] %>% as.numeric()
    Total_Yards_Away <- game_stats[game_stats$Stat == "Total Yards", 2] %>% as.numeric()
    
    Passing_Yards_Home <- game_stats[game_stats$Stat == "Passing", 3] %>% as.numeric()
    Passing_Yards_Away <- game_stats[game_stats$Stat == "Passing", 2] %>% as.numeric()
    
    Comp_Att_Raw_Home <- game_stats[game_stats$Stat == "Comp-Att", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Comp_Att_Raw_Away <- game_stats[game_stats$Stat == "Comp-Att", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Pass_Completions_Home <- Comp_Att_Raw_Home[1]
    Pass_Completions_Away <- Comp_Att_Raw_Away[1]
    
    Pass_Att_Home <- Comp_Att_Raw_Home[2]
    Pass_Att_Away <- Comp_Att_Raw_Away[2]
    
    Yards_Per_Pass_Home <- game_stats[game_stats$Stat == "Yards per pass", 3] %>% as.numeric()
    Yards_Per_Pass_Away <- game_stats[game_stats$Stat == "Yards per pass", 2] %>% as.numeric()
    
    Interceptions_Thrown_Home <- game_stats[game_stats$Stat == "Interceptions thrown", 3] %>% as.numeric()
    Interceptions_Thrown_Away <- game_stats[game_stats$Stat == "Interceptions thrown", 2] %>% as.numeric()
    
    Rushing_Yards_Home <- game_stats[game_stats$Stat == "Rushing", 3] %>% as.numeric()
    Rushing_Yards_Away <- game_stats[game_stats$Stat == "Rushing", 2] %>% as.numeric()
    
    Rushing_Att_Home <- game_stats[game_stats$Stat == "Rushing Attempts", 3] %>% as.numeric()
    Rushing_Att_Away <- game_stats[game_stats$Stat == "Rushing Attempts", 2] %>% as.numeric()
    
    Yards_Per_Rush_Home <- game_stats[game_stats$Stat == "Yards per rush", 3] %>% as.numeric()
    Yards_Per_Rush_Away <- game_stats[game_stats$Stat == "Yards per rush", 2] %>% as.numeric()
    
    Penalties_Raw_Home <- game_stats[game_stats$Stat == "Penalties", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Penalties_Raw_Away <- game_stats[game_stats$Stat == "Penalties", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Penalties_Home <- Penalties_Raw_Home[1]
    Penalties_Away <- Penalties_Raw_Away[1]
    
    Penalties_Yards_Home <- Penalties_Raw_Home[2]
    Penalties_Yards_Away <- Penalties_Raw_Away[2]
    
    Turnovers_Home <- game_stats[game_stats$Stat == "Turnovers", 3] %>% as.numeric()
    Turnovers_Away <- game_stats[game_stats$Stat == "Turnovers", 2] %>% as.numeric()
    
    Fumbles_Lost_Home <- game_stats[game_stats$Stat == "Fumbles lost", 3] %>% as.numeric()
    Fumbles_Lost_Away <- game_stats[game_stats$Stat == "Fumbles lost", 2] %>% as.numeric()
    
    Possession_Raw_Home <- game_stats[game_stats$Stat == "Possession", 3] %>% str_split(":") %>% unlist() %>% as.numeric()
    Possession_Raw_Away <- game_stats[game_stats$Stat == "Possession", 2] %>% str_split(":") %>% unlist() %>% as.numeric()
    
    Possession_Home <- Possession_Raw_Home[1] + Possession_Raw_Home[2]/60
    Possession_Away <- Possession_Raw_Away[1] + Possession_Raw_Away[2]/60
    
    Possession_Percent_Home <- Possession_Home / (Possession_Home + Possession_Away)
    Possession_Percent_Away <- Possession_Away / (Possession_Home + Possession_Away)
    
    #### Fill out dataset ####
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id, "Line_Favored"] <- ifelse(length(line_favored) == 0, "No Line", line_favored)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id, "Line_Amount"] <- ifelse(length(line_amount) == 0, NA, line_amount)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id, "Line_Over_Under"] <- ifelse(length(over_under) == 0, NA, over_under)
    
    ##
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "First_Down"] <- ifelse(length(First_Down_Home) == 0, 0, First_Down_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Conversions"] <- ifelse(length(Third_Down_Conversions_Home) == 0, 0, Third_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Att"] <- ifelse(length(Third_Down_Att_Home) == 0, 0, Third_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Conversions"] <- ifelse(length(Fourth_Down_Conversions_Home) == 0, 0, Fourth_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Att"] <- ifelse(length(Fourth_Down_Att_Home) == 0, 0, Fourth_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Total_Yards"] <- ifelse(length(Total_Yards_Home) == 0, 0, Total_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Passing_Yards"] <- ifelse(length(Passing_Yards_Home) == 0, 0, Passing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Completions"] <- ifelse(length(Pass_Completions_Home) == 0, 0, Pass_Completions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Att"] <- ifelse(length(Pass_Att_Home) == 0, 0, Pass_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Pass"] <- ifelse(length(Yards_Per_Pass_Home) == 0, 0, Yards_Per_Pass_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Interceptions_Thrown"] <- ifelse(length(Interceptions_Thrown_Home[1]) == 0, 0, Interceptions_Thrown_Home[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Yards"] <- ifelse(length(Rushing_Yards_Home) == 0, 0, Rushing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Att"] <- ifelse(length(Rushing_Att_Home) == 0, 0, Rushing_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Rush"] <- ifelse(length(Yards_Per_Rush_Home) == 0, 0, Yards_Per_Rush_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties"] <- ifelse(length(Penalties_Home) == 0, 0, Penalties_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties_Yards"] <- ifelse(length(Penalties_Yards_Home) == 0, 0, Penalties_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Turnovers"] <- ifelse(length(Turnovers_Home) == 0, 0, Turnovers_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fumbles_Lost"] <- ifelse(length(Fumbles_Lost_Home) == 0, 0, Fumbles_Lost_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession"] <- ifelse(length(Possession_Home) == 0, 0, Possession_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession_Percent"] <- ifelse(length(Possession_Percent_Home) == 0, 0, Possession_Percent_Home)
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "First_Down_Against"] <- ifelse(length(First_Down_Away) == 0, 0, First_Down_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Conversions_Against"] <- ifelse(length(Third_Down_Conversions_Away) == 0, 0, Third_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Att_Against"] <- ifelse(length(Third_Down_Att_Away) == 0, 0, Third_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Conversions_Against"] <- ifelse(length(Fourth_Down_Conversions_Away) == 0, 0, Fourth_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Att_Against"] <- ifelse(length(Fourth_Down_Att_Away) == 0, 0, Fourth_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Total_Yards_Against"] <- ifelse(length(Total_Yards_Away) == 0, 0, Total_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Passing_Yards_Against"] <- ifelse(length(Passing_Yards_Away) == 0, 0, Passing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Completions_Against"] <- ifelse(length(Pass_Completions_Away) == 0, 0, Pass_Completions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Att_Against"] <- ifelse(length(Pass_Att_Away) == 0, 0, Pass_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Pass_Against"] <- ifelse(length(Yards_Per_Pass_Away) == 0, 0, Yards_Per_Pass_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Interceptions_Thrown_Against"] <- ifelse(length(Interceptions_Thrown_Away[1]) == 0, 0, Interceptions_Thrown_Away[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Yards_Against"] <- ifelse(length(Rushing_Yards_Away) == 0, 0, Rushing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Att_Against"] <- ifelse(length(Rushing_Att_Away) == 0, 0, Rushing_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Rush_Against"] <- ifelse(length(Yards_Per_Rush_Away) == 0, 0, Yards_Per_Rush_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties_Against"] <- ifelse(length(Penalties_Away) == 0, 0, Penalties_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties_Yards_Against"] <- ifelse(length(Penalties_Yards_Away) == 0, 0, Penalties_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Turnovers_Against"] <- ifelse(length(Turnovers_Away) == 0, 0, Turnovers_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fumbles_Lost_Against"] <- ifelse(length(Fumbles_Lost_Away) == 0, 0, Fumbles_Lost_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession_Against"] <- ifelse(length(Possession_Away) == 0, 0, Possession_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession_Percent_Against"] <- ifelse(length(Possession_Percent_Away) == 0, 0, Possession_Percent_Away)
    
    ##
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "First_Down"] <- ifelse(length(First_Down_Away) == 0, 0, First_Down_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Conversions"] <- ifelse(length(Third_Down_Conversions_Away) == 0, 0, Third_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Att"] <- ifelse(length(Third_Down_Att_Away) == 0, 0, Third_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Conversions"] <- ifelse(length(Fourth_Down_Conversions_Away) == 0, 0, Fourth_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Att"] <- ifelse(length(Fourth_Down_Att_Away) == 0, 0, Fourth_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Total_Yards"] <- ifelse(length(Total_Yards_Away) == 0, 0, Total_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Passing_Yards"] <- ifelse(length(Passing_Yards_Away) == 0, 0, Passing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Completions"] <- ifelse(length(Pass_Completions_Away) == 0, 0, Pass_Completions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Att"] <- ifelse(length(Pass_Att_Away) == 0, 0, Pass_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Pass"] <- ifelse(length(Yards_Per_Pass_Away) == 0, 0, Yards_Per_Pass_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Interceptions_Thrown"] <- ifelse(length(Interceptions_Thrown_Away[1]) == 0, 0, Interceptions_Thrown_Away[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Yards"] <- ifelse(length(Rushing_Yards_Away) == 0, 0, Rushing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Att"] <- ifelse(length(Rushing_Att_Away) == 0, 0, Rushing_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Rush"] <- ifelse(length(Yards_Per_Rush_Away) == 0, 0, Yards_Per_Rush_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties"] <- ifelse(length(Penalties_Away) == 0, 0, Penalties_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties_Yards"] <- ifelse(length(Penalties_Yards_Away) == 0, 0, Penalties_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Turnovers"] <- ifelse(length(Turnovers_Away) == 0, 0, Turnovers_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fumbles_Lost"] <- ifelse(length(Fumbles_Lost_Away) == 0, 0, Fumbles_Lost_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession"] <- ifelse(length(Possession_Away) == 0, 0, Possession_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession_Percent"] <- ifelse(length(Possession_Percent_Away) == 0, 0, Possession_Percent_Away)
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "First_Down_Against"] <- ifelse(length(First_Down_Home) == 0, 0, First_Down_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Conversions_Against"] <- ifelse(length(Third_Down_Conversions_Home) == 0, 0, Third_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Att_Against"] <- ifelse(length(Third_Down_Att_Home) == 0, 0, Third_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Conversions_Against"] <- ifelse(length(Fourth_Down_Conversions_Home) == 0, 0, Fourth_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Att_Against"] <- ifelse(length(Fourth_Down_Att_Home) == 0, 0, Fourth_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Total_Yards_Against"] <- ifelse(length(Total_Yards_Home) == 0, 0, Total_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Passing_Yards_Against"] <- ifelse(length(Passing_Yards_Home) == 0, 0, Passing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Completions_Against"] <- ifelse(length(Pass_Completions_Home) == 0, 0, Pass_Completions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Att_Against"] <- ifelse(length(Pass_Att_Home) == 0, 0, Pass_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Pass_Against"] <- ifelse(length(Yards_Per_Pass_Home) == 0, 0, Yards_Per_Pass_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Interceptions_Thrown_Against"] <- ifelse(length(Interceptions_Thrown_Home[1]) == 0, 0, Interceptions_Thrown_Home[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Yards_Against"] <- ifelse(length(Rushing_Yards_Home) == 0, 0, Rushing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Att_Against"] <- ifelse(length(Rushing_Att_Home) == 0, 0, Rushing_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Rush_Against"] <- ifelse(length(Yards_Per_Rush_Home) == 0, 0, Yards_Per_Rush_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties_Against"] <- ifelse(length(Penalties_Home) == 0, 0, Penalties_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties_Yards_Against"] <- ifelse(length(Penalties_Yards_Home) == 0, 0, Penalties_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Turnovers_Against"] <- ifelse(length(Turnovers_Home) == 0, 0, Turnovers_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fumbles_Lost_Against"] <- ifelse(length(Fumbles_Lost_Home) == 0, 0, Fumbles_Lost_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession_Against"] <- ifelse(length(Possession_Home) == 0, 0, Possession_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession_Percent_Against"] <- ifelse(length(Possession_Percent_Home) == 0, 0, Possession_Percent_Home) 
  }
  
}

ncaaf_l2_raw <- ncaaf_l1

ncaaf_l2 <- ncaaf_l2_raw

#### Get the ID for the Line_Favored column ####

# Line_Favored_Ids <- ncaaf_l2 %>% filter(Line_Favored != "NA", Team_FBS == 1, Opp_FBS == 1) %>% group_by(Team, Line_Favored) %>% tally() %>% arrange(Team, desc(n))
# Line_Favored_Max <- Line_Favored_Ids %>% group_by(Team) %>% summarize(max_n = max(n))
# Line_Favored_Ids <- Line_Favored_Ids %>% left_join(Line_Favored_Max, by = c("Team" = "Team"))
# Line_Favored_Ids <- Line_Favored_Ids %>% filter(n == max_n) %>% select(-max_n) %>% arrange(n)
# 
# write.csv(Line_Favored_Ids, file = "~/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Short_Names.csv")

#### Bring in short names ####

Short_Names <- read_csv("~/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Short_Names.csv")

ncaaf_l2 <- ncaaf_l2 %>% left_join(Short_Names, by = c("Line_Favored" = "Short_Name"))

ncaaf_l2 <- ncaaf_l2 %>% mutate(
  Line_Favored = Favored,
  Vegas_Result = case_when(
    Team == Line_Favored & Result == "W" ~ 1,
    Team != Line_Favored & Result == "L" ~ 1,
    TRUE ~ 0
  )
) %>% select(-Favored)

Last_Week <- ncaaf_l2 %>% filter(Date <= "2020-11-07" & Date >= "2020-10-31" & Home == T & Team_FBS == 1 & Opp_FBS == 1 & Played == TRUE)
sum(Last_Week$Vegas_Result)/nrow(Last_Week)

Season_2019 <- ncaaf_l2 %>% filter(Season == 2019 & Home == T & Team_FBS == 1 & Opp_FBS == 1 & Played == TRUE)
sum(Season_2019$Vegas_Result)/nrow(Season_2019)
View(Season_2019 %>% select())

write.csv(ncaaf_l2, "C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Level_Two.csv", row.names = F)

termId <- rstudioapi::terminalCreate()
rstudioapi::terminalSend(termId, paste0('
                         cd GitHub/Open_Data\n
                         git add .\n
                         git commit -m "NCAAF Update ', Sys.Date(), '"\n
                         git push origin master\n
                         '))

