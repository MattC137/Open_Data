library(dplyr)
library(rvest)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)

Season <- 2020

Clean_Player_Id_Str <- function(pid){
  
  isJr <- ifelse(substr(pid, nchar(pid) - 2, nchar(pid)) == "-jr", 1, 0)
  pid <- ifelse(isJr, substr(pid, 1, nchar(pid) - 3), pid)
  pid <- str_remove(pid, "-ii")
  
  hypen_locations <- str_locate_all(pid, "-")
  hypen_locations <- hypen_locations[[1]][, 1]
  
  if(length(hypen_locations) == 1){
    names <- str_split(pid, "-")
    names <- names[[1]]
    names <- str_to_title(names)
    
    rtn_name <- ifelse(isJr, paste0(names[1], " ", names[2], " ", "Jr."), paste0(names[1], " ", names[2]))
  }
  
  if(length(hypen_locations) == 2){
    names <- str_split(pid, "-")
    names <- names[[1]]
    names <- str_to_title(names)
    
    rtn_name <- ifelse(isJr, paste0(names[1], " ", names[2], "-", names[3], " ", "Jr."), paste0(names[1], " ", names[2], "-", names[3]))
  }
  
  return(as.vector(rtn_name))
}

#### Get Schedule ####

team_ids <- read_csv("~/GitHub/Open_Data/Scripts/Sports/NBA/team_ids.csv")

Schedule <- as.data.frame(matrix(nrow = 0, ncol = 9))
names(Schedule) <- c("Date", "Team", "Opponent", "Result", "W_L", "Season", "Season_Type", "Playoff_Round", "Game_Id")

for(i in 1:nrow(team_ids)){
  # i = 21
  team_name <- as.character(team_ids[i, "Team_Name"])
  short_name <- as.character(team_ids[i, "Short_Name"])
  team_id <- as.character(team_ids[i, "Team_ID"])
  team_city <- as.character(team_ids[i, "City"])
  
  #### Update Seasons ####
  # current_year <- year(Sys.Date())
  
  for(season in Season:Season){
    # season = 2021
    
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
            Sys.sleep(300)
          }
          
          team_schedule_url <- try({read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", short_name, "/season/", season, "/seasontype/", season_type))})
          schedule <- try({team_schedule_url %>% html_node("table") %>% html_table(fill = TRUE)}, silent = T)
          
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
                Playoff_Round = "Non-Playoff",
                Result = ifelse(!str_detect(W_L, "-")& W_L != "Postponed", "TBD", Result),
                W_L = ifelse(!str_detect(W_L, "-") & W_L != "Postponed", "TBD", W_L)
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
              Sys.sleep(300)
            }
            
            team_schedule_url <- try({read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", short_name, "/season/", season, "/seasontype/", season_type))})
            schedule <- try({team_schedule_url %>% html_node("table") %>% html_table()}, silent = T)
            
            if(class(schedule) != "try-error"){
              if(nrow(schedule) > 0){
                update_data <- TRUE
              }else{
                ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
                update_data <- TRUE
              }
              
            }else{
              update_data <- FALSE
            }
            
            end_while <- ifelse(class(schedule) != "try-error" | j == 3, TRUE, FALSE)
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
  Score = ifelse(W_L == "TBD", "0-0", Score),
  Result = ifelse(W_L == "TBD", "TBD", substr(Result, 1, 1))
)

Schedule <- Schedule %>% separate(Score, c("Points_Winner", "Points_Loser"), sep = "-")
Schedule <- Schedule %>% mutate(
  Points_For = ifelse(Result == "W", Points_Winner, Points_Loser),
  Points_Against = ifelse(Result == "W", Points_Loser, Points_Winner),
  Points_Loser = ifelse(W_L == "TBD", 0, Points_Loser),
  Points_Winner = ifelse(W_L == "TBD", 0, Points_Winner),
  Points_For = ifelse(W_L == "TBD", 0, Points_For),
  Points_Against = ifelse(W_L == "TBD", 0, Points_Against)
)

## Make sure opponent is NBA opponent. This can screw up the loop because some preseason games are against outside teams

Schedule <- Schedule %>% filter(!is.na(Opp_Short_Name))

Schedule <- Schedule %>% select(
  Date, Team, Opponent, Result, Points_For, Points_Against, Home, Neutral, OT_Rounds, Season, Season_Type, Playoff_Round, Game_Id
)

opp_team_ids <- team_ids
names(opp_team_ids) <- paste0("Opp_", names(opp_team_ids))

#### MAIN LOOP: Update Level Two Data ####

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
  Threes_Per_For = "0",
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
  Threes_Per_Against = "0",
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
  
  Money_Line = NA,
  Money_Line_Opp = NA,
  
  Implied_Odds = NA,
  Implied_Odds_Opp = NA
)

Future_Schedule <- Schedule %>% 
  left_join(team_ids, by = c("Team" = "City")) %>% 
  left_join(opp_team_ids, by = c("Opponent" = "Opp_City")) %>% 
  filter(Result == "TBD") %>% 
  arrange(Game_Id, Date)

Schedule <- Schedule %>% 
  left_join(team_ids, by = c("Team" = "City")) %>% 
  left_join(opp_team_ids, by = c("Opponent" = "Opp_City")) %>% 
  filter(Result != "TBD") %>% 
  arrange(Game_Id, Date)

#### Scrape Box Score Tables ####

Box_Scores <- as.data.frame(matrix(nrow = 0, ncol = 30))
names(Box_Scores) <- c("Players", "MIN", "FG", "THREES", "FT", "OREB", "DREB", "REB", "AST", "STL", "BLK", "TO", "PF", 
                       "PLUS_MINUS", "PTS", "Position", "Reason_For_Benching", "Played", "Player_Ids","Team", "Opponent", 
                       "Game_Id", "Date", "Home", "Neutral", "OT_Rounds", "Season", "Season_Type", "Result", "Playoff_Round")

Play_by_Play <- as.data.frame(matrix(nrow = 0, ncol = 5))
names(Play_by_Play) <- c("Time", "Team", "Play", "Score", "Game_Id")

Game_Summary <- as.data.frame(matrix(nrow = 0, ncol = 2))
names(Game_Summary) <- c("Summary", "Game_Id")

Shots <- as.data.frame(matrix(nrow = 0, ncol = 9))
names(Shots) <- c("Shot", "Description", "Home", "Quarter", "Player_Id", "Left", "Top", "Made", "Game_Id")

for(i in 1:nrow(Schedule)){
  # i = 27
  
  
  if(i %% 2 != 0){
    print(i)
    
    game_id <- Schedule$Game_Id[i]
    
    ### TRY 3 TIMES
    end_while <- FALSE
    
    j <- 1
    while(!end_while){
      
      if(j > 2){
        Sys.sleep(3)
      }
      
      team_stats_url <- try({read_html(paste0("https://www.espn.com/nba/matchup?gameId=", game_id))})
      team_stats_tables <- try({team_stats_url %>% html_nodes("table") %>% html_table()})
      
      # Handle for missing games eg: https://www.espn.com/nba/matchup?gameId=401070856
      if(length(team_stats_tables) == 0){
        t1 <- FALSE
        t2 <- FALSE
      } else{
        t1 <- ifelse(try({is.data.frame(team_stats_tables[[1]])}) %in% c(TRUE, FALSE), try({is.data.frame(team_stats_tables[[1]])}), FALSE)
        t2 <- ifelse(try({is.data.frame(team_stats_tables[[2]])}) %in% c(TRUE, FALSE), try({is.data.frame(team_stats_tables[[2]])}), FALSE)
      }
      

      
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
      
      ## Check to make sure all rows ar present
      ## Example https://www.espn.com/nba/matchup?gameId=400974444 is missing largest lead
      
      
      
      team_stats_order <- c("FG", "Field Goal %", "3PT", "Three Point %", "FT", "Free Throw %", "Rebounds", "Offensive Rebounds",
                            "Defensive Rebounds", "Assists", "Steals", "Blocks", "Total Turnovers", "Points Off Turnovers", 
                            "Fast Break Points", "Points in Paint", "Fouls", "Technical Fouls", "Flagrant Fouls", "Largest Lead")
      
      if(any(team_stats$Stat != team_stats_order)){
        team_stats_df <- data.frame(Stat = team_stats_order)
        team_stats_df <- team_stats_df %>% left_join(team_stats, by = c("Stat" = "Stat"))
        
        team_stats_df$missing_fill_away <- c("0-0", "0", "0-0", "0", "0-0", "0", "0", "0", "0", "0", "0", "0", 
                                             "0", "0", "0", "0", "0", "0", "0", "0")
        
        team_stats_df$missing_fill_home <- c("0-0", "0", "0-0", "0", "0-0", "0", "0", "0", "0", "0", "0", "0", 
                                             "0", "0", "0", "0", "0", "0", "0", "0")
        
        team_stats_df[is.na(team_stats_df$Away_Stats), "Away_Stats"] <- team_stats_df[is.na(team_stats_df$Away_Stats), "missing_fill_away"]
        team_stats_df[is.na(team_stats_df$Home_Stats), "Home_Stats"] <- team_stats_df[is.na(team_stats_df$Home_Stats), "missing_fill_home"]
        
        team_stats <- team_stats_df %>% select(-missing_fill_away, -missing_fill_home)
      }

      
      if(Schedule$Home[i] == TRUE){
        Schedule[i, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Home_Stats
        Schedule[i, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Away_Stats
        
        Schedule[i+1, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Away_Stats
        Schedule[i+1, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Home_Stats
        
      }else{
        Schedule[i, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Away_Stats
        Schedule[i, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Home_Stats
        
        Schedule[i+1, c(which(names(Schedule) == "FG_For"):which(names(Schedule) == "Largest_Lead_For"))] <- team_stats$Home_Stats
        Schedule[i+1, c(which(names(Schedule) == "FG_Against"):which(names(Schedule) == "Largest_Lead_Against"))] <- team_stats$Away_Stats
        
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
    
    
    #### Box Scores ####
    
    print("Box Scores")
    # game_id = 401265848
    
    ### TRY 3 TIMES
    end_while <- FALSE
    
    j <- 1
    while(!end_while){
      
      if(j > 2){
        Sys.sleep(3)
       # print("sleep")
      }
      
      box_score_url <- try({read_html(paste0("https://www.espn.com/nba/boxscore?gameId=", game_id))})
      box_score <- try({box_score_url %>% html_nodes("table") %>% html_table()})
      
      if(class(box_score) != "try-error" & length(box_score) != 0){
        if(nrow(box_score[[2]]) > 0){
          update_data <- TRUE
        }else{
          ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
          update_data <- FALSE
        }
        
      }else{
        update_data <- FALSE
      }
      
      end_while <- ifelse(class(box_score) != "try-error" | j == 3, TRUE, FALSE)
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
          Players = substr(Players, 1, nchar(Players)-1),
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
          Players = substr(Players, 1, nchar(Players)-1),
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
      
      Player_Ids <- Player_Ids[1:(nrow(away_stats) + nrow(home_stats))]
      
      away_stats$Player_Ids <- Player_Ids[(1:nrow(away_stats))]
      home_stats$Player_Ids <- Player_Ids[-(1:nrow(away_stats))]
      
      Schedule$Game_Id <- str_trim(as.character(Schedule$Game_Id))
      
      # if Game_Id isn't matching because there are some strange characters
      # Schedule[i, "Game_Id"] <- game_id
      # Schedule[i+1, "Game_Id"] <- game_id
      
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
    
    print("Play-by-play")
    
    ### TRY 3 TIMES
    end_while <- FALSE
    
    j <- 1
    while(!end_while){
      
      if(j > 2){
        Sys.sleep(20)
        # print("sleep")
      }
      
      play_by_play_url <- try({read_html(paste0("https://www.espn.com/nba/playbyplay?gameId=", game_id))})
      play_by_play <- try({play_by_play_url %>% html_nodes("table") %>% html_table(fill = TRUE)})
      shots <- try({play_by_play_url %>% str_extract_all('(?<=<li id="shot)(.*?)(?=</li>)')})
      
      if(class(play_by_play) != "try-error" & class(shots) != "try-error" & length(play_by_play) != 0){
        if(nrow(box_score[[2]]) > 0){
          update_data <- TRUE
        }else{
          ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
          update_data <- FALSE
        }
        
      }else{
        update_data <- FALSE
      }
      
      end_while <- ifelse((class(play_by_play) != "try-error" & class(shots) != "try-error") | j == 3, TRUE, FALSE)
      j <- j + 1
      
    }
    ###
    
    if(update_data){
      
      # play_by_play_url <- read_html("https://www.espn.com/nba/playbyplay?gameId=401160630")
      # play_by_play <- play_by_play_url %>% html_nodes("table") %>% html_table(fill = TRUE)
      
      ot_rounds <- Schedule$OT_Rounds[i]
      
      if(ot_rounds == 0){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]])
        
      }else if(ot_rounds == 1){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]])
        
      }else if(ot_rounds == 2){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]])
      }else if(ot_rounds == 3){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]], play_by_play[[8]])
      }else if(ot_rounds == 4){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]], play_by_play[[8]], play_by_play[[9]])
      }else if(ot_rounds == 5){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]], play_by_play[[8]], play_by_play[[9]],
                              play_by_play[[10]])
      }else if(ot_rounds == 6){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]], play_by_play[[8]], play_by_play[[9]],
                              play_by_play[[10]], play_by_play[[11]])
      }else if(ot_rounds == 7){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]], play_by_play[[8]], play_by_play[[9]], 
                              play_by_play[[10]], play_by_play[[11]], play_by_play[[12]])
      }else if(ot_rounds == 8){
        play_by_play <- rbind(play_by_play[[2]], play_by_play[[3]], play_by_play[[4]], play_by_play[[5]], 
                              play_by_play[[6]], play_by_play[[7]], play_by_play[[8]], play_by_play[[9]],
                              play_by_play[[10]], play_by_play[[11]], play_by_play[[12]], play_by_play[[13]])
      }
      
      
      start <- play_by_play[1,1]
      end <- play_by_play[nrow(play_by_play),4]
      
      play_by_play_logos <- play_by_play_url %>% 
        str_extract_all('(?<=<td class="logo">)(.*?)(?=</td>)') %>% unlist() %>% 
        str_extract_all('(?<=teamlogos/nba/500/)(.*?)(?=\\.png.*?100.*?100)') %>% unlist() %>% toupper()
      
      play_by_play$team <- play_by_play_logos
      
      play_by_play <- play_by_play[ ,-5]
      names(play_by_play) <- c("Time", "Team", "Play", "Score")
      
      play_by_play$Game_Id <- game_id
      
      Play_by_Play <- rbind(Play_by_Play, play_by_play)
      
      #### Shots ####
      
      shots <- data.frame(shots = shots[[1]])
      shots <- shots %>% separate(col = shots, into = c('Shot', 'delete'), sep = "data-text")
      
      shots <- shots %>% mutate(
        Description = str_extract(delete, '(?<=\")(.*?)(?=\")'),
        Home = str_extract(delete, '(?<=data-homeaway=\")(.*?)(?=\")'),
        Quarter = str_extract(delete, '(?<=data-period=\")(.*?)(?=\")'),
        Player_Id = str_extract(delete, '(?<=data-shooter=\")(.*?)(?=\")'),
        Left = str_extract(delete, '(?<=left:)(.*?)(?=%)'),
        Top = str_extract(delete, '(?<=top:)(.*?)(?=%)'),
        Made = ifelse(str_detect(delete, "background-color:"), 1, 0),
        Game_Id = game_id
      ) %>% select(-delete)
      
      Shots <- rbind(Shots, shots)
      
    }
    
    #### Game Summaries ####
    
    print("Game Summaries")
    
    ### TRY 3 TIMES
    end_while <- FALSE
    
    j <- 1
    while(!end_while){
      
      if(j > 2){
        Sys.sleep(300)
        # print("sleep")
      }
      
      summary_url <- try({read_html(paste0("https://www.espn.com/nba/recap?gameId=", game_id))})
      # summary_url <- read_html(paste0("https://www.espn.com/nba/recap?gameId=401267397"))
      
      scoreboard <- summary_url %>% str_extract_all('(?<=<title>)(.*?)(?=</title>)') %>% unlist()
      
      if(any(class(summary_url) != "try-error") & scoreboard != "NBA Basketball Scores - NBA Scoreboard - ESPN"){
        update_data <- TRUE
      }else{
        update_data <- FALSE
      }
      
      end_while <- ifelse(any(class(summary_url) != "try-error") | j == 3, TRUE, FALSE)
      j <- j + 1
      
    }
    ###
    
    if(update_data){
      game_recap <- summary_url %>% str_extract_all('(?<=<p>)(.*?)(?=</p>)') %>% unlist()
      cut_off <- which(str_detect(game_recap, "UP NEXT"))
      # cut_off <- ifelse(length(cut_off) == 0, which(game_recap == "UP NEXT:"), cut_off)
      # cut_off <- ifelse(is.na(cut_off), which(game_recap == "<strong>UP NEXT</strong>"), cut_off)
      # cut_off <- ifelse(is.na(cut_off), which(game_recap == "<b>UP NEXT</b>"), cut_off)
      # cut_off <- ifelse(is.na(cut_off), which(str_detect(game_recap, "UP NEXT")), cut_off)
      cut_off <- ifelse(length(cut_off) == 0, which(str_detect(game_recap, "---")), cut_off)
      #cut_off <- ifelse(is.na(cut_off), which(game_recap == "<strong>---</strong>"), cut_off)
      cut_off <- ifelse(is.na(cut_off), length(game_recap), cut_off)
      
      game_recap <- paste(game_recap[1:(cut_off-1)], collapse = " ")
      game_recap <- str_replace_all(game_recap, "<.*?>", "")
      
      game_summary <- as.data.frame(matrix(NA, nrow = 1, ncol = 2))
      names(game_summary) <- c("Summary", "Game_Id")
      
      game_summary$Summary[1] <- ifelse(str_detect(summary_url, "No Recap Available"), "No Recap Available", game_recap)
      game_summary$Game_Id[1] <- game_id

      Game_Summary <- rbind(Game_Summary, game_summary)
      
    }
  }
}

# names(Future_Schedule) == names(Schedule)

#### Salaries ####

Salaries <- as.data.frame(matrix(nrow = 0, ncol = 8))
names(Salaries) <- c("Player_Id", "Player_Id_Str", "Player", "Short_Name", "Team_Id", "Team", "Salary", "Season")

salary_url <- read_html(paste0('http://www.espn.com/nba/salaries/_/year/', Season, '/page/1/seasontype/1'))

Pages <- salary_url %>% str_extract('(?<=1 of )(.*?)(?=<)') %>% as.numeric()

for(i in 1:Pages){
  # print(i)
  # i = 13
  
  salary_url <- read_html(paste0('http://www.espn.com/nba/salaries/_/year/', Season, '/page/', i,'/seasontype/1'))
  
  player <- salary_url %>% str_extract_all('(?<=player/_/id/)(.*?)(?=</td>)') %>% unlist()
  team <- salary_url %>% str_extract_all('(?<=team/_/name/)(.*?)(?=</a>)') %>% unlist()
  salary <- salary_url %>% str_extract_all('(?<=right;">)(.*?)(?=</td>)') %>% unlist()
  salary <- salary[salary != "SALARY"]
  
  # Correct for players traded out of the NBA
  if(!(length(player) == length(team) & length(team) == length(salary))){
    Salary_table <- salary_url %>% html_nodes("table") %>% html_table()
    Salary_table <- as.data.frame(Salary_table[[1]]) %>% filter(X1 != "RK")
    
    missing_id = c()
    missing_team = c()
    
    team_cleaned <- data.frame(teams = team)
    team_cleaned$teams <- as.character(team_cleaned$teams)
    team_cleaned <- team_cleaned %>% separate(teams, into = c("delete", "teams"), sep = ">") %>% select(-delete)
    team_cleaned <- team_cleaned$teams
    
    for(k in 1:(length(player)-length(team))){
      for(j in 1:length(player)){
        # j = 1

        if(Salary_table$X3[j] != team_cleaned[j]){
          missing_id <- c(missing_id, j)
          missing_team <- c(missing_team, Salary_table$X3[j])
          team_cleaned <- append(team_cleaned, Salary_table$X3[j], j-1)
          team <- append(team, values = paste0('xxx/NO ID">', Salary_table$X3[j]), j-1)
        }

        if(Salary_table$X3[j] != team_cleaned[j]) break
      }
    }
    
  }
  
  salaries <- data.frame(Player = player, Team = team, Salary = salary, Season = 2020)
  
  salaries <- salaries %>% mutate(
    Player = str_replace(Player, "</a>", "SPLIT_HERE")
  )
  
  salaries <- salaries %>% separate(Player, c("Player_Id", "Player"), sep = "/")
  salaries <- salaries %>% separate(Player, c("Player_Id_Str", "Player"), sep = '">')
  salaries <- salaries %>% separate(Player, c("Player", "Position"), sep = 'SPLIT_HERE, ')
  salaries <- salaries %>% separate(Team, c("Short_Name", "Team"), sep = '/')
  salaries <- salaries %>% separate(Team, c("Team_Id", "Team"), sep = '">')
  
  salaries <- salaries %>% mutate(
    Salary = substr(Salary, 2, nchar(as.vector(Salary))),
    Salary = str_replace_all(Salary, ",", ""),
    Player = str_replace(Player, "<", ""),
    Short_Name = toupper(Short_Name)
  )
  
  Salaries <- rbind(Salaries, salaries)
}

#### Clean Box_Score ####

Box_Scores_Raw <- Box_Scores

Box_Scores[Box_Scores$FG == "-----", "FG"] <- "0-0"
Box_Scores[Box_Scores$THREES == "-----", "THREES"] <- "0-0"
Box_Scores[Box_Scores$FT == "-----", "FT"] <- "0-0"

Box_Scores[is.na(Box_Scores)] <- 0
Box_Scores <- Box_Scores %>% separate(FG, into = c("FG_Made", "FG_Att"), sep = "-")
Box_Scores <- Box_Scores %>% separate(THREES, into = c("THREES_Made", "THREES_Att"), sep = "-")
Box_Scores <- Box_Scores %>% separate(FT, into = c("FT_Made", "FT_Att"), sep = "-")
Box_Scores <- Box_Scores %>% separate(Player_Ids, into = c("Player_Id", "Player_Id_Str"), sep = "/")

Box_Scores <- Box_Scores %>% mutate(
  PLUS_MINUS = str_replace_all(PLUS_MINUS, "\\+", "")
)

### Update players not on the Salaries List ###

missing_players <- Box_Scores %>% 
  group_by(Player_Id) %>% 
  summarize(Player_Id_Str = first(Player_Id_Str), Short_Name = last(Team), Position = first(Position)) %>% 
  ungroup()

missing_players <- missing_players %>% anti_join(Salaries, by = c("Player_Id" = "Player_Id"))
missing_players <- missing_players %>% mutate(
  Player = NA,
  Salary = NA,
  Season = Season     
) %>% select("Player_Id", "Player_Id_Str", "Player", "Position", "Short_Name", "Salary", "Season" )

missing_players <- missing_players %>% 
  left_join(team_ids %>% select(Short_Name, Team_Id = Team_ID, Team = Team_Name), by = c("Short_Name" = "Short_Name"))

missing_players <- missing_players %>% rowwise() %>% mutate(
 Player = Clean_Player_Id_Str(Player_Id_Str) 
) %>% select(names(Salaries)) %>% as.data.frame()

Salaries <- rbind(Salaries, missing_players)

Salaries <- Salaries %>% mutate(
  Height = NA,
  Feet = NA,
  Inches = NA,
  Weight = NA,
  DOB = NA,
  Draft_Year = NA,
  Draft_Round = NA,
  Draft_Pick = NA,
  Draft_Team = NA
)

# names(Future_Schedule) == names(Schedule)

#### Update Player Bio ####

for(i in 1:nrow(Salaries)){
  # i = 51
  # print(i)
  
  player_id <- Salaries$Player_Id[i]
  # player_id <- 4395628
  
  ### TRY 3 TIMES
  end_while <- FALSE
  
  j <- 1
  while(!end_while){
    
    if(j > 2){
      Sys.sleep(300)
      # print("sleep")
    }
    
    player_url <- try({read_html(paste0("https://www.espn.com/nba/player/_/id/", player_id))})
    
    update_data <- TRUE
    
    end_while <- ifelse(class(player_url) != "try-error" | j == 3, TRUE, FALSE)
    j <- j + 1
    
  }
  ###
  
  if(update_data){
    Bio <- player_url %>% html_nodes('li')
    
   #### Get Height/Weight ####
    
    HT_WT <- try({Bio[str_detect(Bio, " lbs")]})
    if(length(HT_WT) == 1){
      HT <- str_extract(HT_WT, '(?<=<div>)(.*?)(?=, )')
      HT <- str_remove(HT, "'") 
      HT <- str_remove(HT, "\"")
      HT <- str_split(HT, " ") %>% unlist()
      WT <- str_extract(HT_WT, '(?<=", )(.*?)(?= lbs)')
      
      Salaries$Height[i] <- as.numeric(HT[1]) + as.numeric(HT[2])/12
      Salaries$Feet[i] <- as.numeric(HT[1])
      Salaries$Inches[i] <- as.numeric(HT[2])
      Salaries$Weight[i] <- as.numeric(WT)
    }

    
    #### Get DOB ####
    
    DOB <- try({Bio[str_detect(Bio, "DOB")]})
    if(length(DOB) == 1){
      DOB <- str_extract(DOB, '(?<=<div>)(.*?)(?= )') %>% as.Date(format = "%m/%d/%Y")
      Salaries$DOB[i] <- DOB
    }
    
    #### Get Draft Info ####
    
    Draft <- try({Bio[str_detect(Bio, "Draft Info")]})
    if(length(Draft) == 1){
      Draft_Year <- str_extract(Draft, '(?<=<div>)(.*?)(?=:)')
      Draft_Round <- str_extract(Draft, '(?<=Rd )(.*?)(?=,)')
      Draft_Pick <- str_extract(Draft, '(?<=Pk )(.*?)(?= )')
      Draft_Team <- str_extract(Draft, '(?<=\\()(.*?)(?=\\))')
      
      Salaries$Draft_Year[i] <- Draft_Year
      Salaries$Draft_Round[i] <- Draft_Round
      Salaries$Draft_Pick[i] <- Draft_Pick
      Salaries$Draft_Team[i] <- Draft_Team
    }
  }
}

Players <- Salaries

Players <- Players %>% mutate(
  DOB = as.Date(DOB, origin = "1970-01-01")
)

# names(Future_Schedule) == names(Schedule)

#### Clean Objects ####

rm(list = ls()[!(ls() %in% c("Schedule", "Box_Scores", "Play_by_Play", "Game_Summary", "Shots", "Players", "Season", "Future_Schedule", "team_ids"))])

#### Clean Schedule

Schedule <- Schedule %>% separate(FG_For, into = c("FG_Made_For", "FG_Att_For"), sep = "-")
Schedule <- Schedule %>% separate(FG_Against, into = c("FG_Made_Against", "FG_Att_Against"), sep = "-")

Schedule <- Schedule %>% separate(Threes_For, into = c("Threes_Made_For", "Threes_Att_For"), sep = "-")
Schedule <- Schedule %>% separate(Threes_Against, into = c("Threes_Made_Against", "Threes_Att_Against"), sep = "-")

Schedule <- Schedule %>% separate(FT_For, into = c("FT_Made_For", "FT_Att_For"), sep = "-")
Schedule <- Schedule %>% separate(FT_Against, into = c("FT_Made_Against", "FT_Att_Against"), sep = "-")

Schedule <- Schedule %>% mutate(
  FG_Per_For = as.numeric(FG_Per_For)/100,
  FG_Per_Against = as.numeric(FG_Per_Against)/100,
  
  Threes_Per_For = as.numeric(Threes_Per_For)/100,
  Threes_Per_Against = as.numeric(Threes_Per_Against)/100,
  
  FT_Per_For = as.numeric(FT_Per_For)/100,
  FT_Per_Against = as.numeric(FT_Per_Against)/100
  
)

Future_Schedule <- Future_Schedule %>% select(-FG_For, -FG_Against, -Threes_For, -Threes_Against, -FT_For, -FT_Against) %>% mutate(
  FG_Made_For = NA,
  FG_Att_For = NA,
  FG_Made_Against = NA,
  FG_Att_Against = NA,
  Threes_Made_For = NA,
  Threes_Att_For = NA,
  Threes_Made_Against = NA,
  Threes_Att_Against = NA,
  FT_Made_For = NA,
  FT_Att_For = NA,
  FT_Made_Against = NA,
  FT_Att_Against = NA
) %>% select(names(Schedule))

# names(Future_Schedule) == names(Schedule)

#### Play by Play ####

Play_by_Play_Copy <- Play_by_Play

play_by_play <- Play_by_Play_Copy

Play_by_Play <- as.data.frame(matrix(ncol = 8, nrow = 0))
names(Play_by_Play) <- c("Quarter", "Time", "Quarter_Time", "Team", "Play", "Home_Points", "Away_Points", "Game_Id")

for(i in 1:length(unique(play_by_play$Game_Id))){
  # i = 1
  
  pbp_game_id <- unique(play_by_play$Game_Id)[i]
  
  # pbp_game_id <- 401160666
  pbp <- play_by_play %>% filter(Game_Id == pbp_game_id)
  
  pbp <- pbp %>% separate(Score, c("Home_Points", "Away_Points"), sep = "-")
  pbp <- pbp %>% mutate(
    Time = ifelse(str_detect(Time, ":"), Time, paste0("0:", Time))
  )
  pbp <- pbp %>% separate(Time, c("Minutes", "Seconds_Percent"), sep = ":")
  pbp <- pbp %>% mutate(
    Quarter = 1,
    Seconds_Percent = as.numeric(Seconds_Percent)/60,
    Quarter_Time = as.numeric(Minutes) + Seconds_Percent,
    Time = 0
  ) %>% select(-Minutes, -Seconds_Percent)
  
  pbp$Time <- as.numeric(pbp$Time)
  
  for(j in 2:nrow(pbp)){
    # j = 2
    if(pbp$Quarter_Time[j] <= pbp$Quarter_Time[j-1]){
      pbp$Quarter[j] = pbp$Quarter[j-1]
      pbp$Time[j] = pbp$Time[j-1] + (pbp$Quarter_Time[j-1] - pbp$Quarter_Time[j])
      
    }else{
      pbp$Quarter[j] = pbp$Quarter[j-1] + 1
      pbp$Time[j] = pbp$Time[j-1]
      
    }
    
    if(pbp$Home_Points[j] < pbp$Home_Points[j-1]){
      pbp$Home_Points[j] = pbp$Home_Points[j-1]
    }
    
    if(pbp$Away_Points[j] < pbp$Away_Points[j-1]){
      pbp$Away_Points[j] = pbp$Away_Points[j-1]
    }
    
  }
  
  pbp <- pbp %>% select(Quarter, Time, Quarter_Time, Team, Play, Home_Points, Away_Points, Game_Id)
  Play_by_Play <- rbind(Play_by_Play, pbp)
}

#### Clean Objects ####

rm(list = ls()[!(ls() %in% c("Schedule", "Box_Scores", "Play_by_Play", "Game_Summary", "Shots", "Players", "Season", "Future_Schedule", "team_ids"))])

#### Get Future Game Lines ####

for(i in 1:nrow(Future_Schedule)){
  # i = 1
  
  
  if(i %% 2 != 0){
    # print(i)
    
    game_id <- Future_Schedule$Game_Id[i]
    
    # game_id <- 401267248
    
    ### TRY 3 TIMES
    end_while <- FALSE
    
    j <- 1
    while(!end_while){
      
      if(j > 2){
        Sys.sleep(300)
      }
      
      team_stats_url <- try({read_html(paste0("https://www.espn.com/nba/game/_/gameId/", game_id))})
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
      
      Upcoming_Tables <- team_stats_url %>% 
        html_nodes("table") %>% 
        html_table()
      
      #### ADD INJURIES HERE USING Upcoming_Tables ####
      
      if(any(str_detect(names(Upcoming_Tables[[1]]), "Money Line"))){
        Line_Table <- Upcoming_Tables[[1]]
        names(Line_Table) <- c("Team", "TeamRankings", "numberFire", "Spread_Consensus_Pick", "Spread", "Money_Line", "Over_Under")
        Line_Table <- Line_Table %>% select(Team, Spread, Money_Line, Over_Under)
        Line_Table <- Line_Table %>% separate(Team, c("Team", "Delete"), "\n") %>% select(-Delete)
        
        if(Line_Table$Spread[1] != "--"){
          line_favored <- ifelse(Line_Table$Spread[1] < 0 | Line_Table$Spread[2] < 0, "Not Even", "Even")
          line_favored <- ifelse(line_favored == "Not Even" & Line_Table$Spread[1] < 0, Line_Table$Team[1], Line_Table$Team[2])
          line_favored <- ifelse(line_favored == "Even", "Even", as.character(team_ids[team_ids$Team_Name == line_favored, "Short_Name"]))
          
          line_amount <- ifelse(line_favored == "Even", 0, as.numeric(-abs(Line_Table$Spread[1])))
          
          over_under <- Line_Table$Over_Under[1]
          
          away_ml <- Line_Table$Money_Line[1]
          home_ml <- Line_Table$Money_Line[2]
          
          implied_odds_away <- ifelse(away_ml < 0, -away_ml/(-away_ml+100), 100/(away_ml+100))
          implied_odds_home <- ifelse(home_ml < 0, -home_ml/(-home_ml+100), 100/(home_ml+100))
          
          Future_Schedule[i, "Line_Favored"] <- ifelse(is.null(line_favored), "No Line", line_favored)
          Future_Schedule[i+1, "Line_Favored"] <- ifelse(is.null(line_favored), "No Line", line_favored)
          
          Future_Schedule[i, "Line_Amount"] <- ifelse(is.null(line_amount) | is.na(line_amount), "No Line", line_amount)
          Future_Schedule[i+1, "Line_Amount"] <- ifelse(is.null(line_amount) | is.na(line_amount), "No Line", line_amount)
          
          Future_Schedule[i, "Over_Under"] <- ifelse(is.null(over_under), "No Line", over_under)
          Future_Schedule[i+1, "Over_Under"] <- ifelse(is.null(over_under), "No Line", over_under)
          
          Future_Schedule[i, "Money_Line"] <- ifelse(Future_Schedule$Home[i], home_ml, away_ml)
          Future_Schedule[i+1, "Money_Line_Opp"] <- ifelse(Future_Schedule$Home[i+1], away_ml, home_ml)
          
          Future_Schedule[i+1, "Money_Line"] <- ifelse(Future_Schedule$Home[i+1], home_ml, away_ml)
          Future_Schedule[i, "Money_Line_Opp"] <- ifelse(Future_Schedule$Home[i], away_ml, home_ml)
          
          Future_Schedule[i, "Implied_Odds"] <-  ifelse(Future_Schedule$Home[i], implied_odds_home, implied_odds_away)
          Future_Schedule[i+1, "Implied_Odds_Opp"] <- ifelse(Future_Schedule$Home[i+1], implied_odds_away, implied_odds_home)
          
          Future_Schedule[i+1, "Implied_Odds"] <- ifelse(Future_Schedule$Home[i+1], implied_odds_home, implied_odds_away)
          Future_Schedule[i, "Implied_Odds_Opp"] <- ifelse(Future_Schedule$Home[i], implied_odds_away, implied_odds_home)
        }
      }
    }
    
  }
  
}

length(unique(Schedule$Game_Id))

if(nrow(Future_Schedule) > 0){
  Schedule <- rbind(Schedule, Future_Schedule)
}

time_stamp = Sys.time()

line_log <- Future_Schedule %>% 
  select(Date, Team, Opponent, Season, Game_Id, Line_Favored, Line_Amount, Over_Under, Money_Line, Money_Line_Opp, Implied_Odds, Implied_Odds_Opp) %>% 
  filter(!is.na(Money_Line)) %>% 
  mutate(
    Time_Stamp = time_stamp
  )

# write.csv(line_log, file = paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_LIne_Log_", Season,".csv"), row.names = F)

if(nrow(Future_Schedule) > 0){
  Line_Log <- read_csv(paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_LIne_Log_", Season,".csv"))
  Line_Log <- rbind(Line_Log, line_log)
  write.csv(Line_Log, file = paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_LIne_Log.csv"), row.names = F)
}


#### Data testing ####

length(unique(Schedule$Game_Id))*2 == nrow(Schedule)

length(unique(Schedule$Game_Id))
length(unique(Box_Scores$Game_Id))
length(unique(Play_by_Play$Game_Id))

View(Schedule %>% filter(Season_Type == "Regular-Season") %>% 
       group_by(Team) %>% 
       summarize(W = sum(Result == "W"), L = sum(Result == "L"), ppg = mean(as.numeric(Points_For)), opp_ppg = mean(as.numeric(Points_Against))) %>% 
       arrange(desc(W))
)

View(Box_Scores %>%
       filter(Season_Type == "Regular-Season") %>% 
       group_by(Player_Id_Str) %>% 
       summarize(
          Games = sum(Played), 
          Points = sum(as.numeric(PTS)), 
          Assists = sum(as.numeric(AST)), 
          Rebounds = sum(as.numeric(REB)), 
          Blocks = sum(as.numeric(BLK)),
          Threes_Made = sum(as.numeric(THREES_Made)),
          Steals = sum(as.numeric(STL))
          ) %>% 
       ungroup() %>% 
       mutate(
         Points = Points/Games,
         Assists = Assists/Games,
         Rebounds = Rebounds/Games,
         Blocks = Blocks/Games,
         Threes_Made = Threes_Made/Games,
         Steals = Steals/Games
         ) %>% 
       arrange(desc(Points))
     
     )

#### RENAME DFs ####

Games <- Schedule

#### Write Data ####

write.csv(Games, paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_Games.csv"), row.names = F)
write.csv(Box_Scores, paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_Box_Score.csv"), row.names = F)
write.csv(Play_by_Play, paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_Play_by_Play.csv"), row.names = F)
write.csv(Game_Summary, paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_Game_Recaps.csv"), row.names = F)
write.csv(Shots, paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_Shots.csv"), row.names = F)
write.csv(Players, paste0("~/GitHub/Open_Data/Data/Sports/NBA/NBA_", Season,"_Players.csv"), row.names = F)

#### GitHub Push ####

termId <- rstudioapi::terminalCreate()
rstudioapi::terminalSend(termId, paste0('
                         cd GitHub/Open_Data\n
                         git add --all\n
                         git commit -m "NBA Update ', Sys.time(), '"\n
                         git push origin master\n
                         '))


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

# -e "C:/Users/Matt C137/Documents/GitHub/Open_Data/Scripts/Sports/NBA/Get_NBA_Data.R"