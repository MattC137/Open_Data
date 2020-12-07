library(dplyr)
library(rvest)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)

#### Get Teams ####

## Only run to update and generate .csv the rest need to be complete by hand 

# team_html <- read_html("https://www.espn.com/nba/teams")
# team_ids <- team_html %>% str_extract_all('(?<=nba/team/_/name/)(.*?)(?=")') %>% unlist() %>% data.frame()
# names(team_ids) <- c("ids_raw")
# team_ids <- team_ids %>%
#   separate("ids_raw", c("Short_Name", "Team_ID") ,sep = "/") %>%
#   arrange(Short_Name) %>%
#   mutate(
#     dup = ifelse(Short_Name == lag(Short_Name, 1), 1, 0)
#   )
# 
# team_ids[1, "dup"] <- 0
# 
# team_ids <- team_ids %>% filter(dup == 0) %>% select(-dup)
# 
# write.csv(team_ids, "~/GitHub/Open_Data/Scripts/Sports/NBA/team_ids_TO_UPDATE.csv", row.names = F)

## End .csv Update

team_ids <- read_csv("~/GitHub/Open_Data/Scripts/Sports/NBA/team_ids.csv")

Schedule <- as.data.frame(matrix(nrow = 0, ncol = 9))
names(Schedule) <- c("Date", "Team", "Opponent", "Result", "W_L", "Season", "Season_Type", "Playoff_Round", "Game_Id")

for(i in 1:nrow(team_ids)){
  # i = 3
  team_name <- as.character(team_ids[i, "Team_Name"])
  short_name <- as.character(team_ids[i, "Short_Name"])
  team_id <- as.character(team_ids[i, "Team_ID"])
  team_city <- as.character(team_ids[i, "City"])
  
  #### Update Seasons ####
  # current_year <- year(Sys.Date())
  
  for(season in 2020:2020){
    # season = 2020
    
    Schedule_Season <- as.data.frame(matrix(nrow = 0, ncol = 9))
    names(Schedule_Season) <- c("Date", "Team", "Opponent", "Result", "W_L", "Season", "Season_Type", "Playoff_Round", "Game_Id")
    
    print(paste(team_name, season))
    
    for(season_type in 1:3){
      # season_type = 2
      
      if(season_type != 3){
        
        ### TRY 3 TIMES
        end_while <- FALSE
        j <- 1
        while(!end_while){
          
          if(j > 2){
            Sys.sleep(100)
          }
          
          team_schedule_url <- try({read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", short_name, "/season/", season, "/seasontype/", season_type))})
          schedule <- try({team_schedule_url %>% html_node("table") %>% html_table()}, silent = T)
          
          if(is.data.frame(schedule)){
            if(nrow(schedule) > 0){
              update_data <- TRUE
            }else{
              ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
              update_data <- TRUE
            }
            
          }else{
            update_data <- FALSE
          }
          
          end_while <- ifelse(is.data.frame(schedule) | j == 3, TRUE, FALSE)
          j <- j + 1
          
        }
        ###
        
        if(update_data){
        
          game_ids <- team_schedule_url %>% str_extract_all('(?<=gameId=)(.*?)(?=")') %>% unlist() %>% unique()
          
          if(class(schedule) != 'try-error'){
            names(schedule) <- c("Date", "Opponent", "Result", "W_L", "Hi_Points", "Hi_Rebounds", "Hi_Assists")
            schedule <- schedule %>% 
              select(Date, Opponent, Result, W_L) %>% filter(Date != "DATE") %>% 
              mutate(
                Team = team_city,
                Season = season,
                Season_Type = case_when(
                  season_type == 1 ~ "Pre-Season",
                  season_type == 2 ~ "Regular-Season",
                  season_type == 3 ~ "Post-Season"
                ),
                Playoff_Round = "Non-Playoff"
              )
            
            schedule <- schedule %>% filter(Result != "Postponed")
            
            schedule$Game_Id <- game_ids[1:nrow(schedule)]
            # schedule$Order <- 1:nrow(schedule) DELETE ME
            
            Schedule_Season <- rbind(Schedule_Season, schedule %>% select(Date, Team, Opponent, Result, W_L, Season, Season_Type, Playoff_Round, Game_Id))
          }
          
        }else{
  
          ### TRY 3 TIMES
          end_while <- FALSE
          j <- 1
          while(!end_while){
            
            if(j > 2){
              Sys.sleep(100)
            }
            
            team_schedule_url <- try({read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", short_name, "/season/", season, "/seasontype/", season_type))})
            schedule <- try({team_schedule_url %>% html_node("table") %>% html_table()}, silent = T)
            
            if(is.data.frame(schedule)){
              if(nrow(schedule) > 0){
                update_data <- TRUE
              }else{
                ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
                update_data <- TRUE
              }
              
            }else{
              update_data <- FALSE
            }
            
            end_while <- ifelse(is.data.frame(schedule) | j == 3, TRUE, FALSE)
            j <- j + 1
            
          }
          ###
          
          if(update_data){
          
            if(class(schedule) != 'try-error'){
            
              game_ids <- team_schedule_url %>% str_extract_all('(?<=gameId=)(.*?)(?=")') %>% unlist() %>% unique()
              
              round <- schedule[str_detect(schedule$X1, "Conference") | 
                                  str_detect(schedule$X1, "final") |
                                  str_detect(schedule$X1, "Final"), 1]
              
              schedule <- schedule[!(str_detect(schedule$X1, "Conference") | 
                                     str_detect(schedule$X1, "final") |
                                     str_detect(schedule$X1, "Final") |
                                     str_detect(schedule$X1, "DATE")), ]
              
              names(schedule) <- c("Date", "Opponent", "Result", "Hi_Points", "Hi_Rebounds", "Hi_Assists")
              
              #### Detect playoff round ####
              
              Playoff_Round_Table <- data.frame(Opp_Raw = unique(schedule$Opponent))
              
              Playoff_Round_Table <- Playoff_Round_Table %>% mutate(
                Opp = str_replace(Opp_Raw, "vs", ""),
                Opp = str_replace(Opp, "@", "")
              )
              
              Round_mapping <- data.frame(Opp = unique(Playoff_Round_Table$Opp))
              Round_mapping$Playoff_Round <- round
              
              Playoff_Round_Table <- Playoff_Round_Table %>% left_join(Round_mapping, by = c("Opp" = "Opp"))
              
              schedule <- schedule %>% 
                select(Date, Opponent, Result) %>% 
                mutate(
                  Team = team_name,
                  W_L = "NA-NA",
                  Season = season,
                  Season_Type = case_when(
                    season_type == 1 ~ "Pre-Season",
                    season_type == 2 ~ "Regular-Season",
                    season_type == 3 ~ "Post-Season"
                  )
                )
              
              schedule <- schedule %>% left_join(Playoff_Round_Table, by = c("Opponent" = "Opp_Raw"))
              
              schedule <- schedule %>% filter(Result != "Postponed")
              
              schedule$Game_Id <- game_ids[1:nrow(schedule)]
              
              Schedule_Season <- rbind(Schedule_Season, schedule %>% select(Date, Team, Opponent, Result, W_L, Season, Season_Type, Playoff_Round, Game_Id))
            }
          }
        }
      }
    }
    
    #Schedule_Season <- Schedule_Season %>% arrange(Game_Id)
    
    Schedule_Season <- Schedule_Season %>% mutate(
      Date = str_replace(Date, "Sat, ", ""),
      Date = str_replace(Date, "Sun, ", ""),
      Date = str_replace(Date, "Mon, ", ""),
      Date = str_replace(Date, "Tue, ", ""),
      Date = str_replace(Date, "Wed, ", ""),
      Date = str_replace(Date, "Thu, ", ""),
      Date = str_replace(Date, "Fri, ", ""),
      Date = paste0(Date, ", ", season),
      Date = as.Date(Date, format = "%b %d, %Y")
    )
    
    if(month(Schedule_Season$Date[1]) > 4){
      i <- 2
      update_date = TRUE
      while(update_date){
        
        update_date = Schedule_Season[i-1, "Date"] < Schedule_Season[i, "Date"]
        Schedule_Season[i-1, "Date"] <- as.Date(Schedule_Season[i-1, "Date"] - years(1))
        # schedule[i, Date] = ifelse(schedule[i, Date] <= schedule[i-1, Date], as.Date(schedule[i, Date] - years(1)), as.Date(schedule[i, Date]))

        i = i + 1
      }
    }
    Schedule <- rbind(Schedule, Schedule_Season)
  }
}

Schedule_Raw <- Schedule

#### Cleaning Schedule ####

Schedule <- Schedule_Raw

Schedule <- Schedule %>% mutate(
  Home = ifelse(str_detect(Opponent, "vs"), 1, 0),
  Neutral = ifelse(str_detect(Opponent, " \\*"), 1, 0),
  Opponent = str_remove(Opponent, "vs"),
  Opponent = str_remove(Opponent, "@"),
  Opponent = str_remove(Opponent, " \\*"),
  OT_Rounds = case_when(
    str_detect(Result, " OT") ~ 1,
    str_detect(Result, " 2OT") ~ 2,
    str_detect(Result, " 3OT") ~ 3,
    str_detect(Result, " 4OT") ~ 4,
    str_detect(Result, " 5OT") ~ 5,
    str_detect(Result, " 6OT") ~ 6,
    str_detect(Result, " 7OT") ~ 7,
    str_detect(Result, " 8OT") ~ 8,
    str_detect(Result, " 9OT") ~ 9,
    str_detect(Result, " 10OT") ~ 10,
    str_detect(Result, " 11OT") ~ 11,
    str_detect(Result, " 12OT") ~ 12,
    TRUE ~ 0
    ),
  Score = Result,
  Score = ifelse(Score == "Postponed", "0-0", Score),
  Score = str_replace(Score, " OT", ""),
  Score = str_replace(Score, " 2OT", ""),
  Score = str_replace(Score, " 3OT", ""),
  Score = str_replace(Score, " 4OT", ""),
  Score = str_replace(Score, " 5OT", ""),
  Score = str_replace(Score, " 6OT", ""),
  Score = str_replace(Score, " 7OT", ""),
  Score = str_replace(Score, " 8OT", ""),
  Score = str_replace(Score, " 9OT", ""),
  Score = str_replace(Score, " 10OT", ""),
  Score = str_replace(Score, " 11OT", ""),
  Score = str_replace(Score, " 12OT", ""),
  Score = str_replace(Score, "L", ""),
  Score = str_replace(Score, "W", ""),
  Score = str_replace(Score, "P", ""),
  Score = str_replace(Score, "D", ""),
  Score = str_replace(Score, "T", ""),
  Score = ifelse(Score == "Postponed", "0-0", Score),
  Result = substr(Result, 1, 1)
)

Schedule <- Schedule %>% separate(Score, c("Points_Winner", "Points_Loser"), sep = "-")
Schedule <- Schedule %>% mutate(
  Points_For = ifelse(Result == "W", Points_Winner, Points_Loser),
  Points_Against = ifelse(Result == "W", Points_Loser, Points_Winner)
)

Schedule <- Schedule %>% select(
  Date, Team, Opponent, Result, Points_For, Points_Against, Home, Neutral, OT_Rounds, Season, Season_Type, Playoff_Round, Game_Id
)

opp_team_ids <- team_ids
names(opp_team_ids) <- paste0("Opp_", names(opp_team_ids))

Schedule <- Schedule %>% 
  left_join(team_ids, by = c("Team" = "City")) %>% 
  left_join(opp_team_ids, by = c("Opponent" = "Opp_City")) %>% 
  filter(complete.cases(.)) %>% 
  arrange(Game_Id, Date)

#### Update Level Two Data ####

Schedule <- Schedule %>% mutate(
  Q1_Points_For = 0,
  Q2_Points_For = 0,
  Q3_Points_For = 0,
  Q4_Points_For = 0,
  OT_Points_For = 0,
  Q1_Points_Against = 0,
  Q2_Points_Against = 0,
  Q3_Points_Against = 0,
  Q4_Points_Against = 0,
  OT_Points_Against = 0,
  
  FG_For = "0",
  FG_Per_For = "0",
  Threes_For = "0",
  Three_Per_For = "0",
  FT_For = "0",
  FT_Per_For = "0",
  Rebounds_For = "0",
  Offensive_Rebounds_For = "0",
  Defensive_Rebounds_For = "0",
  Assists_For = "0",
  Steals_For = "0",
  Blocks_For = "0",
  Total_Turnovers_For = "0",
  Points_Off_Turnovers_For = "0",
  Fast_Break_Points_For = "0",
  Points_in_Paint_For = "0",
  Fouls_For = "0",
  Technical_Fouls_For = "0",
  Flagrant_Fouls_For = "0",
  Largest_Lead_For = "0",
  
  FG_Against = "0",
  FG_Per_Against = "0",
  Threes_Against = "0",
  Three_Per_Against = "0",
  FT_Against = "0",
  FT_Per_Against = "0",
  Rebounds_Against = "0",
  Offensive_Rebounds_Against = "0",
  Defensive_Rebounds_Against = "0",
  Assists_Against = "0",
  Steals_Against = "0",
  Blocks_Against = "0",
  Total_Turnovers_Against = "0",
  Points_Off_Turnovers_Against = "0",
  Fast_Break_Points_Against = "0",
  Points_in_Paint_Against = "0",
  Fouls_Against = "0",
  Technical_Fouls_Against = "0",
  Flagrant_Fouls_Against = "0",
  Largest_Lead_Against = "0",
  
  Line_Favored = "",
  Line_Amount = NA,
  Over_Under = NA,
  
  Correct_Line = NA,
  Spread_Winner = NA,
  Over_Under_Winner = NA,
)

#### Scrape Box Score Tables ####

Box_Scores <- as.data.frame(matrix(nrow = 0, ncol = 30))
names(Box_Scores) <- c("Players", "MIN", "FG", "THREES", "FT", "OREB", "DREB", "REB", "AST", "STL", "BLK", "TO", "PF", 
                       "PLUS_MINUS", "PTS", "Position", "Reason_For_Benching", "Played", "Player_Ids","Team", "Opponent", 
                       "Game_Id", "Date", "Home", "Neutral", "OT_Rounds", "Season", "Season_Type", "Result", "Playoff_Round")

for(i in 1:nrow(Schedule)){
  # i = 1
  
  
  if(i %% 2 != 0){
    print(i)
    
    game_id <- Schedule$Game_Id[i]

    ### TRY 3 TIMES
    end_while <- FALSE
    
    j <- 1
    while(!end_while){
      
      if(j > 2){
        # Sys.sleep(100)
      }
      
      team_stats_url <- try({read_html(paste0("https://www.espn.com/nba/matchup?gameId=", game_id))})
      team_stats_tables <- try({team_stats_url %>% html_nodes("table") %>% html_table()})
      
      t1 <- ifelse(try({is.data.frame(team_stats_tables[[1]])}) %in% c(TRUE, FALSE), try({is.data.frame(team_stats_tables[[1]])}), FALSE)
      t2 <- ifelse(try({is.data.frame(team_stats_tables[[2]])}) %in% c(TRUE, FALSE), try({is.data.frame(team_stats_tables[[2]])}), FALSE)
      
      if(t1 & t2){
        if(nrow(team_stats_tables[[1]]) > 0 & nrow(team_stats_tables[[2]]) > 0){
          update_data <- TRUE
        }else{
          ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
          update_data <- FALSE
        }
        
      }else{
        update_data <- FALSE
      }
      
      end_while <- ifelse(t1 & t2 | j == 3, TRUE, FALSE)
      j <- j + 1
      
    }
    ###
    
    if(update_data){
    
      #### Quarter Points ####
      
      quarters <- team_stats_tables[[1]]
      home_team <- quarters[2, 1]
      away_team <- quarters[1, 1]
      
      # First solve which team is home on Neutral games
      
      if(Schedule$Neutral[i] == TRUE){
        Schedule$Home[i] <- ifelse(Schedule$Short_Name[i] == home_team, 1, 0)
        Schedule$Home[i+1] <- ifelse(Schedule$Short_Name[i+1] == home_team, 1, 0)
      }
      
      quarters <- quarters[ , -ncol(quarters)]
      
      Schedule$Q1_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 2], quarters[1, 2])
      Schedule$Q2_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 3], quarters[1, 3])
      Schedule$Q3_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 4], quarters[1, 4])
      Schedule$Q4_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 5], quarters[1, 5])
      Schedule$Q1_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 2], quarters[1, 2])
      Schedule$Q2_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 3], quarters[1, 3])
      Schedule$Q3_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 4], quarters[1, 4])
      Schedule$Q4_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 5], quarters[1, 5])
      
      if(ncol(quarters) > 5){
        Schedule$OT_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
        Schedule$OT_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
      }
  
      Schedule$Q1_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 2], quarters[1, 2])
      Schedule$Q2_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 3], quarters[1, 3])
      Schedule$Q3_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 4], quarters[1, 4])
      Schedule$Q4_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 5], quarters[1, 5])
      Schedule$Q1_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 2], quarters[1, 2])
      Schedule$Q2_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 3], quarters[1, 3])
      Schedule$Q3_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 4], quarters[1, 4])
      Schedule$Q4_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 5], quarters[1, 5])
      
      if(ncol(quarters) > 5){
        Schedule$OT_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
        Schedule$OT_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
      }
      
      #### Team Stats ####
      
      print("Team Stats")
      
      # cols 32:51/52:71
      
      team_stats <- team_stats_tables[[2]]
      names(team_stats) <- c("Stat", "Away_Stats", "Home_Stats")
      
      if(Schedule$Home[i] == TRUE){
        Schedule[i, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Home_Stats
        Schedule[i, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Away_Stats
        
        Schedule[i+1, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Away_Stats
        Schedule[i+1, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Home_Stats
        
      }else{
        Schedule[i, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Away_Stats
        Schedule[i, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Home_Stats
        
        Schedule[i+1, c(32:51)] <- team_stats$Home_Stats
        Schedule[i+1, c(52:71)] <- team_stats$Away_Stats
        
      }
      
      Line <- team_stats_url %>% 
        html_nodes(".odds-details") %>% 
        html_text("li") %>% 
        str_extract_all('(?<=Line: )(.*?)(?=\n)') %>% 
        unlist() %>% 
        str_split(" ") %>% 
        unlist()
      
      line_favored <- ifelse(is.null(Line[1]), "No Line", Line[1])
      line_amount <- ifelse(line_favored == "Even", 0, as.numeric(Line[2]))
      line_amount <- ifelse(is.null(Line), "No Line", line_amount)
      
      over_under <- team_stats_url %>% 
        html_nodes(".odds-details") %>% 
        html_text("li") %>% 
        str_extract_all('(?<=Over/Under: )(.*?)(?=\n)') %>% 
        unlist() %>% 
        as.numeric()
      
      Schedule[i, "Line_Favored"] <- ifelse(is.null(line_favored), "No Line", line_favored)
      Schedule[i+1, "Line_Favored"] <- ifelse(is.null(line_favored), "No Line", line_favored)
      
      Schedule[i, "Line_Amount"] <- ifelse(is.null(line_amount) | is.na(line_amount), "No Line", line_amount)
      Schedule[i+1, "Line_Amount"] <- ifelse(is.null(line_amount) | is.na(line_amount), "No Line", line_amount)
      
      Schedule[i, "Over_Under"] <- ifelse(is.null(over_under), "No Line", over_under)
      Schedule[i+1, "Over_Under"] <- ifelse(is.null(over_under), "No Line", over_under)
      
      Schedule[i, "Correct_Line"] <- ifelse((Schedule[i, "Short_Name"] == line_favored & Schedule[i, "Result"] == "W") | (Schedule[i, "Short_Name"] != line_favored & Schedule[i, "Result"] == "L"), 1, 0)
      Schedule[i+1, "Correct_Line"] <- ifelse((Schedule[i+1, "Short_Name"] == line_favored & Schedule[i+1, "Result"] == "W") | (Schedule[i+1, "Short_Name"] != line_favored & Schedule[i+1, "Result"] == "L"), 1, 0)
      
      # Schedule[i, "Spread_Winner"] <- ifelse((Schedule[i, "Short_Name"] == tolower(line_favored) & Schedule[i, "Result"] == "W") | (Schedule[i, "Short_Name"] != tolower(line_favored) & Schedule[i, "Result"] == "L"), 1, 0)
      # Schedule[i+1, "Spread_Winner"] <- line_amount
      # 
      # Schedule[i, "Over_Under_Winner"] <- over_under
      # Schedule[i+1, "Over_Under_Winner"] <- over_under
      
    }
  }
  
  #### Box Scores ####
  
  print("Box Scores")
  
  ### TRY 3 TIMES
  end_while <- FALSE
  
  j <- 1
  while(!end_while){
    
    if(j > 2){
      Sys.sleep(100)
    }
    
    box_score_url <- try({read_html(paste0("https://www.espn.com/nba/boxscore?gameId=", game_id))})
    box_score <- try({box_score_url %>% html_nodes("table") %>% html_table()})
    
    if(is.data.frame(box_score[[2]])){
      if(nrow(box_score[[2]]) > 0){
        update_data <- TRUE
      }else{
        ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
        update_data <- FALSE
      }
      
    }else{
      update_data <- FALSE
    }
    
    end_while <- ifelse(nrow(box_score[[2]]) > 0 | j == 3, TRUE, FALSE)
    j <- j + 1
    
  }
  ###
  
  if(update_data){
    Player_Ids <- box_score_url %>% str_extract_all('(?<=nba/player/_/id/)(.*?)(?=")') %>% unlist()
    
    away_stats <- box_score[[2]]
    home_stats <- box_score[[3]]
    
    away_stats <- away_stats %>% 
      rename(Players = Starters, PLUS_MINUS = `+/-`, THREES = `3PT`) %>% 
      filter(Players != "Bench", Players != "TEAM", Players != "") %>% 
      mutate(
        Position = substr(Players, nchar(Players), nchar(Players)),
        Players = str_replace(Players, Position, ""),
        Players = substr(Players, 1, nchar(Players)/2),
        Reason_For_Benching = case_when(
          str_detect(MIN, "DNP") ~ MIN,
          TRUE ~ "Played"
        ),
        Played = case_when(
          str_detect(MIN, "DNP") ~ FALSE,
          TRUE ~ TRUE
        ),
        MIN = ifelse(Played == F, "0", MIN),  
        FG = ifelse(Played == F, "0-0", FG), 
        THREES = ifelse(Played == F, "0-0", THREES), 
        FT = ifelse(Played == F, "0-0", FT), 
        OREB = ifelse(Played == F, "0", OREB), 
        DREB = ifelse(Played == F, "0", DREB), 
        REB = ifelse(Played == F, "0", REB), 
        AST = ifelse(Played == F, "0", AST), 
        STL = ifelse(Played == F, "0", STL), 
        BLK = ifelse(Played == F, "0", BLK), 
        TO = ifelse(Played == F, "0", TO), 
        PF = ifelse(Played == F, "0", PF), 
        PLUS_MINUS = ifelse(Played == F, "0", PLUS_MINUS), 
        PTS = ifelse(Played == F, "0", PTS)
      )
    
    home_stats <- home_stats %>% 
      rename(Players = Starters, PLUS_MINUS = `+/-`, THREES = `3PT`) %>% 
      filter(Players != "Bench", Players != "TEAM", Players != "") %>% 
      mutate(
        Position = substr(Players, nchar(Players), nchar(Players)),
        Players = str_replace(Players, Position, ""),
        Players = substr(Players, 1, nchar(Players)/2),
        Reason_For_Benching = case_when(
          str_detect(MIN, "DNP") ~ MIN,
          TRUE ~ "Played"
        ),
        Played = case_when(
          str_detect(MIN, "DNP") ~ FALSE,
          TRUE ~ TRUE
        ),
        MIN = ifelse(Played == F, "0", MIN),  
        FG = ifelse(Played == F, "0-0", FG), 
        THREES = ifelse(Played == F, "0-0", THREES), 
        FT = ifelse(Played == F, "0-0", FT), 
        OREB = ifelse(Played == F, "0", OREB), 
        DREB = ifelse(Played == F, "0", DREB), 
        REB = ifelse(Played == F, "0", REB), 
        AST = ifelse(Played == F, "0", AST), 
        STL = ifelse(Played == F, "0", STL), 
        BLK = ifelse(Played == F, "0", BLK), 
        TO = ifelse(Played == F, "0", TO), 
        PF = ifelse(Played == F, "0", PF), 
        PLUS_MINUS = ifelse(Played == F, "0", PLUS_MINUS), 
        PTS = ifelse(Played == F, "0", PTS)
      )
    
    # Player_Ids <- box_score_url %>% str_extract_all('(?<=nba/player/_/id/)(.*?)(?=")') %>% unlist()
    
    away_stats$Player_Ids <- Player_Ids[(1:nrow(away_stats))]
    home_stats$Player_Ids <- Player_Ids[-(1:nrow(away_stats))]
    
    home_result <- Schedule[Schedule$Home == T & Schedule$Game_Id == game_id, "Result"]
    away_result <- Schedule[Schedule$Home == F & Schedule$Game_Id == game_id, "Result"]
    
    away_stats <- away_stats %>% mutate(
      Team = away_team,
      Opponent = home_team,
      Game_Id = game_id,
      Date = Schedule$Date[i],
      Home = FALSE,
      Neutral = Schedule$Neutral[i],
      OT_Rounds = Schedule$OT_Rounds[i],
      Season = Schedule$Season[i],
      Season_Type = Schedule$Season_Type[i],
      Result = away_result,
      Playoff_Round = Schedule$Playoff_Round[i]
    )
    
    home_stats <- home_stats %>% mutate(
      Team = home_team,
      Opponent = away_team,
      Game_Id = game_id,
      Date = Schedule$Date[i],
      Home = TRUE,
      Neutral = Schedule$Neutral[i],
      OT_Rounds = Schedule$OT_Rounds[i],
      Season = Schedule$Season[i],
      Season_Type = Schedule$Season_Type[i],
      Result = home_result,
      Playoff_Round = Schedule$Playoff_Round[i]
    )
    
    Box_Scores <- rbind(Box_Scores, home_stats, away_stats)
    
  }
  #### Play-by-play ####
  
}
